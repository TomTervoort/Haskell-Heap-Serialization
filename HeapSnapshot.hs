module HeapSnapshot where

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Word
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

newtype HeapSnapshot = HeapSnapshot (UArray Int Word) -- TODO: Maybe ByteArray#?

-- Lazily computed view of a HeapSnapshot that can be used to iterate over a snapshot in Haskell land.
newtype SnapshotView = SnapshotView (IntMap HNode)

-- Datatypes for exploring a heap snapshot.
data HNode = HNode {
                nodePointers :: [HPointer],
                nodeLiterals :: [HLiteral],
                nodeInfo     :: [HNodeInfo]
                }
                
-- HPointers are actually indexes of the IntMap in the SnapshotView
newtype HPointer = HPointer {ptrIndex :: Int}


