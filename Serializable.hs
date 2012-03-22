{-# LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable, TypeSynonymInstances, OverlappingInstances #-}
module Serializable (
                     Serialized,
                     Serializable,
                     VersionID,
                     toBytes,
                     fromBytes,
                     serialize,
                     deserialize,
                     SerializableByShow,
                     storeInByteString,
                     loadfromBytes,
                     store,
                     load
                    ) where

import IntegralBytes
import ProgramVersionID

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

import Data.Word
import Data.Bits
import Data.Typeable

import Data.Ratio
import System.Environment
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Array.IArray

-----------------

type TypeID = String

typeID :: Typeable a => a -> TypeID
typeID = show . typeOf

libraryVersion :: String
libraryVersion = "serihask001"

-----------------

data VersionID = VersionID Word64
               | ProgramUniqueVID 
                deriving (Eq, Show)
               
-- TODO
combineVIDs :: [VersionID] -> VersionID
combineVIDs = undefined

data Serialized = Serialized {dataType :: TypeID, serializerVersion :: VersionID, dataPacket :: ByteString} deriving Show

-----------------

class Typeable a => Serializable a where
 toBytes   :: a -> ByteString
 fromBytes :: ByteString -> a
 serialVersionID :: a -> VersionID
 serialVersionID _ = ProgramUniqueVID
 
serialize :: (Serializable a) => a -> Serialized
serialize x = Serialized {dataType = typeID x, serializerVersion = serialVersionID x, dataPacket = toBytes x}

deserialize :: (Serializable a) => Serialized -> Maybe a
deserialize (Serialized tid sv dp) | tid /= typeID result = Nothing
                                   | sv /= serialVersionID result = error "Version of serializer used for this object does not match the current one."
                                   | otherwise = Just result
 where result = fromBytes dp

-----------------

storeInByteString :: Serialized -> IO ByteString
storeInByteString obj = do pid <- programVersionID
                           -- Prepend version identifier with 0 if manual or 1 if generated
                           let vid = B.pack $ case serializerVersion obj of
                                               VersionID id -> 0 : bytes id
                                               ProgramUniqueVID -> 1 : bytes pid
                           let tid = BS.pack $ dataType obj ++ "\0"
                           let pdata = dataPacket obj
                           let datalen = B.pack $ bytes $ BS.length pdata
                           let libid = BS.pack libraryVersion
                           return $ BS.concat 
                                     [
                                      libid,   -- Identifier of serialization library
                                      vid,     -- Version identifier
                                      tid,     -- NULL-terminated string uniquely identifying the type
                                      datalen, -- Length of object data
                                      pdata    -- Actual data of serialized object
                                     ]
                            
-- TODO: throw proper exception on failure
loadfromBytes :: ByteString -> IO (Serialized, ByteString)
loadfromBytes str = do let str0 = B.unpack str
                       let (lid, str1) = splitAt (length libraryVersion) str0
                       let (vid, str2) = splitAt 9 str1
                       let (tid, str3) = (takeWhile (/= 0) str2, drop (length tid + 1) str2)
                       let (len, str4) = splitAt 4 str3
                       let (dat, str5) = splitAt (unbytes len) str4
                            
                       when (asciiString lid /= libraryVersion) $ error "Object was not serialized with the current version of the library."
                       pvid <- case vid of
                                (0:id) -> return $ VersionID $ unbytes id
                                (1:id) -> do pid <- programVersionID
                                             when (pid /= unbytes id) $ error "Object was not serialized with the current build of this application."
                                             return ProgramUniqueVID
                                _      -> error "Invalid data"
                            
                       return (Serialized (asciiString tid) pvid (B.pack dat), B.pack str5)
 where asciiString :: [Word8] -> String
       asciiString = map $ chr . fromIntegral
                            

store :: Serialized -> FilePath -> IO ()
store ob path = storeInByteString ob >>= BS.writeFile path

load :: FilePath -> IO Serialized
load path = BS.readFile path >>= fmap fst . loadfromBytes

-----------------

class (Show a, Read a, Typeable a) => SerializableByShow a

instance SerializableByShow a => Serializable a where
 toBytes   = B.pack . concat . map (bytes . ord) . show
 fromBytes = read . stringify . B.unpack
  where stringify [] = ""
        stringify (a:b:c:d:xs) = chr (unbytes [a,b,c,d]) : stringify xs
        
-----------------

instance Serializable ByteString where
 toBytes = id
 fromBytes = id
 
instance Serializable a => Serializable [a] where
 toBytes [] = BS.empty
 toBytes (x:xs) = BS.concat [B.pack $ bytes $ BS.length sx, sx, toBytes xs]
  where sx = toBytes x
 fromBytes str | BS.null str = []
               | otherwise = let (len, str1)  = BS.splitAt 4 str
                                 (str2, rest) = BS.splitAt (unbytes $ B.unpack len) str1
                              in fromBytes str2 : fromBytes rest     
                              
instance Serializable String where
 toBytes = encodeUtf8 . T.pack
 fromBytes = T.unpack . decodeUtf8
                              
instance Serializable a => Serializable (Maybe a) where
 toBytes Nothing  = BS.empty
 toBytes (Just x) = B.cons 0 $ toBytes x
 fromBytes str | BS.null str = Nothing
               | otherwise   = Just $ fromBytes $ B.tail str
               
instance Serializable () where
 toBytes   _ = B.empty
 fromBytes _ = ()
               
instance (Serializable a, Serializable b) => Serializable (a,b) where
 toBytes (a,b) = toBytes [Left a, Right b]
 fromBytes str = let [Left a, Right b] = fromBytes str in (a,b)
 
instance (Serializable a, Serializable b) => Serializable (Either a b) where
 toBytes (Left  x) = B.cons 0 $ toBytes x
 toBytes (Right x) = B.cons 1 $ toBytes x
 fromBytes str | B.head str == 0 = Left  $ fromBytes $ B.tail str
               | otherwise       = Right $ fromBytes $ B.tail str
               
instance Serializable Char where
 toBytes = toBytes . ord
 fromBytes = chr . fromBytes
 
instance Serializable Int where
 toBytes = B.pack . bytes
 fromBytes = unbytes . B.unpack
 
instance (IArray ar e, Typeable2 ar, Ix i, Serializable i, Serializable e) => Serializable (ar i e) where
 toBytes ar   = toBytes (bounds ar, elems ar)
 fromBytes ar = listArray bnds els
  where (bnds, els) = fromBytes ar
  

-- The following instances use SerializableByShow for now, but should get more compact representations
instance SerializableByShow Integer
instance SerializableByShow Float
instance SerializableByShow Double
instance SerializableByShow Word
instance (Read a, Typeable a, Integral a) => SerializableByShow (Ratio a)




