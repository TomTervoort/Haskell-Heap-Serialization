module UHC.HeapSnapshot.Serialization where

import UHC.HeapSnapshot
import Data.Serialization

import System.IO

makeSafeForSerialization :: SnapshotView -> SnapshotView
makeSafeForSerialization = undefined

instance Serializable HeapSnapshot where
 serialVersionID _ = ProgramUniqueVID
 toBytes s = let HeapSnapshot arr = modifySnapshot makeSafeForSerialization s
              in toBytes arr
 fromBytes = HeapSnapshot . fromBytes
 

storeHeapSnapshot :: Handle -> a -> IO ()
storeHeapSnapshot h x = snapshot x >>= hStore h . serialize

retrieveHeapSnapshot :: Handle -> IO a
retrieveHeapSnapshot h = do obj <- hLoad h 
                            case deserialize obj of
                             Nothing -> error "Serialized object is not a heap snapshot."
                             Just s  -> unsafeRestoreSnapshot s
