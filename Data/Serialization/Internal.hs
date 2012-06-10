module Data.Serialization.Internal where

import Data.Serialization.Internal.IntegralBytes

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.Word
import Data.Bits
import Data.Typeable



                
-- Computes a checksum using Fletcher's algorithm.
checksum :: [Word8] -> Word64
checksum = fletcher 0 0 . map fromIntegral
 where fletcher :: Word32 -> Word32 -> [Word32] -> Word64
       fletcher s1 s2 []     = fromIntegral s1 `shiftL` 32 .|. fromIntegral s2
       fletcher s1 s2 (x:xs) = let a = s1 + x
                                   b = s2 + a
                                in s1 `seq` s2 `seq` fletcher a b xs
                                
-- Applies checksum to an Int rather than a Word64
checksumInt :: [Word8] -> Int
checksumInt x = let c = checksum x 
                 in fromIntegral $ (c `shiftR` 32) `xor` c

-- Unique identifier for types.
type TypeID = String

------------------

someListToBytes :: (a -> [Byte]) -> [a] -> [Byte]
someListToBytes _ [] = []
someListToBytes f (x:xs) = let bx = f x 
                            in concat [varbytes $ length bx, bx, someListToBytes f xs]
                            
someListFromBytes :: ([Byte] -> a) -> [Byte] -> [a]
someListFromBytes _ [] = []
someListFromBytes f str = let (len, str1)  = varunbytes str
                              (str2, rest) = splitAt len str1
                           in f str2 : someListFromBytes f rest

-----------------

------------------------------------------------

-- | Represents the binary representation of a serialized object, along with version and type 
-- information.
data Serialized = Serialized {dataType :: TypeID, 
                              serializerVersion :: VersionID, 
                              dataPacket :: ByteString} deriving Show
                              
-- | A datatype containing a version of a serialization method.
data VersionID = VersionID Int
              -- | ProgramUniqueVID 
                deriving (Eq, Ord, Show)

-- | Byte is simply a synonym for a 'Word8'.
type Byte = Word8
                              
class Typeable a => Serializable a where
 toBytes   :: a -> [Byte]
 fromBytes :: [Byte] -> a
 
 serialVersionID :: a -> VersionID
 dependencies :: a -> [VersionID]
 dependencies _ = []
 
 constBytesSize :: a -> Maybe Int
 constBytesSize _ = Nothing
 
 listToBytes :: [a] -> [Byte]
 listToBytes l | isJust $ constBytesSize $ head l = concatMap toBytes l
               | otherwise = someListToBytes toBytes l
 listFromBytes :: [Byte] -> [a]
 listFromBytes l = case constBytesSize $ head defaultresult of
                    Nothing -> defaultresult
                    Just n  -> chunks n l
  where defaultresult = someListFromBytes fromBytes l
        chunks _ [] = []
        chunks n xs = case splitAt n xs of
                       (c, rest) -> fromBytes c : chunks n rest
                       
-------------------------------------------------

combineVIDs :: [VersionID] -> VersionID
combineVIDs vids = VersionID $ checksumInt $ concatMap bytes $ map (\(VersionID i) -> i) vids

-- | Shorthand for serialVersionID.
sid :: Serializable a => a -> VersionID
sid = serialVersionID 
