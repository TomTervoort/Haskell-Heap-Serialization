module HeapSnapshot where

import Serializable

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Word
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Array.Unboxed 

-- Placeholder for types I've yet to define.
type TODO = ()

newtype HeapSnapshot = HeapSnapshot (UArray Int Word8) -- TODO: Use ByteArray# instead?

-- Lazily computed view of a HeapSnapshot that can be used to iterate over a snapshot in Haskell land.
-- The root of the snapshot can be found at position 0 in the IntMap.
newtype SnapshotView = SnapshotView (IntMap HNode)

-- Datatypes for exploring a heap snapshot.
data HNode = HNode {
                nodePointers :: [HPointer],
                nodeLiterals :: [HLiteral],
                nodeInfo     :: [HNodeInfo]
             }
                
-- HPointers are actually indexes of the IntMap in the SnapshotView
newtype HPointer = HPointer {ptrIndex :: Int}

data HLiteral = LitInt Int
              | LitFloat Float
              | LitDouble Double
              | LitByteArray (UArray Int Word8)
              | LitChan HChannel
              | LitWeakPtr TODO
              
data HChannel = HChannel {
                   channelHandle :: TODO,
                   channelName   :: String,
                   channelFlags  :: TODO
                }
                
data HNodeInfo = HNodeInfo TODO

------------------------------------------

snapshot :: a -> HeapSnapshot
snapshot = undefined

exploreSnapshot :: HeapSnapshot -> SnapshotView
exploreSnapshot = undefined

modifySnapshot :: (SnapshotView -> SnapshotView) -> HeapSnapshot -> HeapSnapshot
modifySnapshot = undefined


