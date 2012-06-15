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
              -- ProgramUniqueVID 
                deriving (Eq, Ord, Show)

-- | Byte is simply a synonym for a 'Word8'.
type Byte = Word8
                              
-- | Represents objects for which a specific serialization method has been defined. Instances exist
-- for many standard types. If you want to serialize something of a different type, either use
-- the generic serializers from @Data.Serialization.Generic@ or make a custom instance of
-- 'Serializable' by importing @Data.Serialization.Extent@.
class Typeable a => Serializable a where
 -- | Define an object's binary representation by converting it to a list of bytes. When 
 -- implementing this, you do not have to take version and type information in account as that is 
 -- taken care of by the library. 
 --
 -- The only rule that should apply is that @fromBytes (toBytes x) == x@. 
 toBytes   :: a -> [Byte]
 
 -- | Decodes a binary representation to an object. You may assume that the argument is a valid 
 -- result of @toBytes@. 
 
 -- If the list of bytes providided to this function does not conform to your 
 -- binary protocol, it is either the result of a bug (either in this library or your own) or an 
 -- incompatibility caused because @serialVersionID@ was not updated or @dependencies@ are missing.
 -- In that case it is recommended to cause an error or throw an exception.
 fromBytes :: [Byte] -> a
 
 -- | A number indicating the version of the serialization method. This is included in the binary
 -- representation so an error can be generated whenever the serialization methods of two versions 
 -- of a program are incompatible with each other.
 -- 
 -- This identifier should be incremented or otherwise changed whenever the definitions of 
 -- @toBytes@ and @fromBytes@ have been updated.
 --
 -- The same ID should always be returned for any object of the same type, it should not depend on
 -- the contents of the argument (otherwise, undefined behavior may occur).
 serialVersionID :: a -> VersionID
 
 -- | If the @toBytes@ and @fromBytes@ definitions for other 'Serializable' types are used in order
 -- to encode an object of the current type. This should return a list of @serialVersionID@'s of
 -- the serializers that are depended on.
 --
 -- Defining this function is optional and by default, it returns an empty list.
 --
 -- Like @serialVersionID@, the argument only indicates the type and it should furthermore be 
 -- ignored.
 dependencies :: a -> [VersionID]
 dependencies _ = []
 
 -- | Whenever the list returned by @toBytes@ always has the same length, regardless of the input, 
 -- this can be defined to indicate this constant size. When this is present, no lengths have to be
 -- encoded for this type, which can safe quite a few bytes, especially if many small objects are
 -- serialized.
 --
 -- Like @serialVersionID@, the argument only indicates the type and it should furthermore be 
 -- ignored.
 constBytesSize :: a -> Maybe Int
 constBytesSize _ = Nothing
 
 -- | Can be implemented, in conjunction with its inverse @listFromBytes@ to define a specialized 
 -- serializer for lists of this object. For example, this is used to encode lists of Bool's as
 -- bitlists, making them eight times as small as with the default definition.
 --
 -- Is optional. If implemented, @listFromBytes@ should be made compatible.
 listToBytes :: [a] -> [Byte]
 listToBytes l | isJust $ constBytesSize $ head l = concatMap toBytes l
               | otherwise = someListToBytes toBytes l
               
 -- | See @listToBytes@.
 listFromBytes :: [Byte] -> [a]
 listFromBytes l = case constBytesSize $ head defaultresult of
                    Nothing -> defaultresult
                    Just n  -> chunks n l
  where defaultresult = someListFromBytes fromBytes l
        chunks _ [] = []
        chunks n xs = case splitAt n xs of
                       (c, rest) -> fromBytes c : chunks n rest
                       
-------------------------------------------------

-- | Shorthand for serialVersionID. Is practical for when defining a lot of @dependencies@.
sid :: Serializable a => a -> VersionID
sid = serialVersionID 

-- Combine multiple version identifiers by taking a checksum over them.
combineVIDs :: [VersionID] -> VersionID
combineVIDs vids = VersionID $ checksumInt $ concatMap bytes $ map (\(VersionID i) -> i) vids
