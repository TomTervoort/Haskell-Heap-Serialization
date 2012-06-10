module Data.Serialization.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Bits
import Data.Word


-- | A datatype containing a version of a serialization method. ProgramUniqueVID indicates only 
-- | serialized objects of the same build of the same program are compatible.
data VersionID = VersionID Int
              -- | ProgramUniqueVID 
                deriving (Eq, Show)
                
-- Computes a checksum using Fletcher's algorithm.
checksum :: [Word8] -> Word64
checksum = fletcher 0 0 . map fromIntegral
 where fletcher :: Word32 -> Word32 -> [Word32] -> Word64
       fletcher s1 s2 []     = fromIntegral s1 `shiftL` 32 .|. fromIntegral s2
       fletcher s1 s2 (x:xs) = let a = s1 + x
                                   b = s2 + a
                                in s1 `seq` s2 `seq` fletcher a b xs
                                
checksumInt :: [Word8] -> Int
checksumInt x = let c = checksum x 
                 in fromIntegral $ (c `shiftR` 32) `xor` c

type TypeID = String

data Serialized = Serialized {dataType :: TypeID, 
                              serializerVersion :: VersionID, 
                              dataPacket :: ByteString} deriving Show
