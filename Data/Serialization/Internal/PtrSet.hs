{-# LANGUAGE RankNTypes, ExistentialQuantification, MagicHash #-}
module Data.Serialization.Internal.PtrSet (PtrSet, PtrKey, newPtrSet, ptrSetAdd, ptrSetMember) where

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits
import Control.Exception

import Data.Typeable
import Data.Data
import Data.Generics
import Data.IORef
import Data.Array (Ix)

import Foreign.StablePtr
import System.Mem.Weak

import System.Time

import Unsafe.Coerce
import GHC.Exts

------------------

data PtrBox a = PtrBox a
data SBoxPtr = forall a. SBoxPtr (StablePtr (PtrBox a))

data GCCheckPoint = forall a. GCCheckPoint (Weak a)

----------------------

data PtrSet = PtrSet [SBoxPtr]

data PtrKey = PtrKey Int
               deriving (Eq, Ord, Ix, Show)

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
stablePtrCompare :: a -> SBoxPtr -> IO Bool
stablePtrCompare x b@(SBoxPtr sp) = do cp <- gcCheckPoint
                                       (PtrBox y) <- deRefStablePtr $ unsafeCoerce# sp
                                       eq <- evaluate $ unsafePtrCompare x y
                                       gc <- gcHasRun cp
                                       if gc
                                        then stablePtrCompare x b
                                        else return eq                            
                             
-----------------------

newPtrSet :: IO PtrSet
newPtrSet = return $ PtrSet []

ptrSetAdd :: a -> PtrSet -> IO PtrSet
ptrSetAdd x (PtrSet ps) = do sp <- newStablePtr $ PtrBox x
                             return $ PtrSet $ ps ++ [SBoxPtr sp]

ptrSetMember :: a -> PtrSet -> IO (Maybe PtrKey)
ptrSetMember x (PtrSet ps) = do cmp <- forM ps (stablePtrCompare x)
                                return $ fmap PtrKey $ findIndex id cmp


