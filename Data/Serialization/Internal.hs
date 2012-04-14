module Data.Serialization.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B


-- | A datatype containing a version of a serialization method. ProgramUniqueVID indicates only 
-- | serialized objects of the same build of the same program are compatible.
data VersionID = VersionID Int
               | ProgramUniqueVID 
                deriving (Eq, Show)

type TypeID = String

data Serialized = Serialized {dataType :: TypeID, serializerVersion :: VersionID, dataPacket :: ByteString} deriving Show
