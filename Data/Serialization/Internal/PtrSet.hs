{-# LANGUAGE RankNTypes, ExistentialQuantification, MagicHash, DeriveDataTypeable #-}
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
import Data.Dynamic
import Data.Data
import Data.Generics
import Data.IORef
import Data.Array (Ix)
import Data.Hashable

import System.Mem.StableName
import Data.HashTable (HashTable)
import qualified Data.HashTable as HT
import Data.Dynamic

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

data PtrSet = PtrSet {table   :: HashTable SomeStableName PtrKey,
                      nextKey :: IORef PtrKey}

data SomeStableName = forall a. SomeStableName TypeRep (StableName a)

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

someSname :: Typeable a => StableName a -> SomeStableName
someSname n = SomeStableName (typeOf $ inner n) n
 where inner = undefined :: StableName a -> a

newPtrSet :: IO PtrSet
newPtrSet = do table <- HT.new eq hasher
               key <- newIORef 0
               return $ PtrSet table key
 where eq (SomeStableName t1 s1) (SomeStableName t2 s2)
             | t1 == t2 = unsafeCoerce s1 == s2
             | otherwise = False
       hasher (SomeStableName _ s) = HT.hashInt $ hashStableName s

ptrSetAdd :: Typeable a => a -> PtrSet -> IO PtrKey
ptrSetAdd x ps = do sname <- makeStableName x
                    key <- readIORef $ nextKey ps
                    HT.insert (table ps) (someSname sname) key
                    writeIORef (nextKey ps) (key + 1)
                    return key


ptrSetMember :: Typeable a => a -> PtrSet -> IO (Maybe PtrKey)
ptrSetMember x ps = do sname <- makeStableName x
                       HT.lookup (table ps) $ someSname sname

