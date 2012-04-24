{-# LANGUAGE RankNTypes, ExistentialQuantification, MagicHash, DeriveDataTypeable #-}
module Data.Serialization.Internal.PtrSet (PtrSet, PtrKey, newPtrSet, ptrSetAdd, ptrSetAdd', ptrSetMember) where

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits
import Control.Exception

import Data.Typeable
import Data.Dynamic
import Data.Data
import Data.Generics
import Data.IORef
import Data.Array (Ix)
import Data.Hashable

import Foreign.StablePtr
import System.Mem.Weak

import System.Time

import Unsafe.Coerce
import GHC.Exts

------------------

data PtrBox a = PtrBox a deriving Typeable
data SBoxPtr = SBoxPtr (StablePtr Dynamic)

data GCCheckPoint = forall a. GCCheckPoint (Weak a)

----------------------

data PtrSet = PtrSet [SBoxPtr]

type PtrKey = Int

-------------------------

gcCheckPoint :: IO GCCheckPoint
gcCheckPoint = do key <- generateTemporaryPtr
                  ptr <- mkWeakPtr key (Just $ return ())
                  return $ GCCheckPoint ptr
 where generateTemporaryPtr = getClockTime
 
gcHasRun :: GCCheckPoint -> IO Bool
gcHasRun (GCCheckPoint w) = fmap isNothing $ deRefWeak w
 
 

 
unsafePtrCompare :: a -> a -> Bool
unsafePtrCompare a b = reallyUnsafePtrEquality# a b /=# 0#


-- Tests whether a stable pointer points to the same value as another pointer. This operation is 
-- atomic within GC runs.
stablePtrCompare :: Typeable a => a -> SBoxPtr -> IO Bool
stablePtrCompare x b@(SBoxPtr sp) = do cp <- gcCheckPoint
                                       box <- deRefStablePtr sp
                                       case fromDynamic box of
                                        Nothing         -> return False
                                        Just (PtrBox y) -> 
                                            do eq <- evaluate $ unsafePtrCompare x y
                                               gc <- gcHasRun cp
                                               if gc
                                                then stablePtrCompare x b
                                                else return eq
                     
-----------------------

newPtrSet :: IO PtrSet
newPtrSet = return $ PtrSet []

ptrSetAdd :: Typeable a => a -> PtrSet -> IO PtrSet
ptrSetAdd x s = fmap fst $ ptrSetAdd' x s

ptrSetAdd' :: Typeable a => a -> PtrSet -> IO (PtrSet, PtrKey)
ptrSetAdd' x (PtrSet ps) = do sp <- newStablePtr $ toDyn $ PtrBox x
                              return (PtrSet $ ps ++ [SBoxPtr sp], length ps)

ptrSetMember :: Typeable a => a -> PtrSet -> IO (Maybe PtrKey)
ptrSetMember x (PtrSet ps) = do cmp <- forM ps (stablePtrCompare x)
                                return $ findIndex id cmp


