module Data.Serialization.Internal.PtrSet (newPtrSet, ptrSetAdd, ptrSetMember) where

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits

import Data.Typeable
import Data.Data
import Data.Generics


data PtrSet = TODO

data PtrKey = TODO2

newPtrSet :: IO PtrSet
newPtrSet = undefined

ptrSetAdd :: a -> PtrSet -> IO PtrSet
ptrSetAdd = undefined

ptrSetMember :: a -> PtrSet -> IO (Maybe PtrKey)
ptrSetMember = undefined
