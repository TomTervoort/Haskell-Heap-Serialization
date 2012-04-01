{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TypeSynonymInstances, OverlappingInstances #-}
module Serializable (
                     Serialized,
                     Serializable,
                     VersionID (..),
                     toBytes,
                     fromBytes,
                     serialVersionID,
                     dependencies,
                     serialize,
                     deserialize,
                     LazyByteString,
                     storeInByteString,
                     loadFromBytes,
                     hStore,
                     hLoad,
                     hLoads,
                     store,
                     load,
                     loads
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
import qualified Data.ByteString.Lazy as BL

import Data.Word
import Data.Bits
import Data.Typeable

import System.Environment
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Array.IArray

-----------------

-- | Type definition that is useful to distinguish lazy from regular ByteStrings.
type LazyByteString = BL.ByteString
 
-----------------

type TypeID = String

-- Returns a unique identifier for the type of a Typeable value.
-- TODO: More reliable method, although that will probably require compiler support.
typeID :: Typeable a => a -> TypeID
typeID = show . typeOf

-- Current version of the serialization library.
libraryVersion :: String
libraryVersion = "serihask001"

-----------------

data VersionID = VersionID Int
               | ProgramUniqueVID 
                deriving (Eq, Show)
               
combineVIDs :: [VersionID] -> VersionID
combineVIDs vids | ProgramUniqueVID `elem` vids = ProgramUniqueVID
                 | otherwise =  VersionID $ toInt $ checksum $ concatMap bytes 
                                          $ map (\(VersionID i) -> i) vids
 where toInt :: Word64 -> Int
       toInt x = fromIntegral $ (x `shiftR` 32) `xor` x

data Serialized = Serialized {dataType :: TypeID, serializerVersion :: VersionID, dataPacket :: ByteString} deriving Show

-----------------

class Typeable a => Serializable a where
 toBytes   :: a -> ByteString
 fromBytes :: ByteString -> a
 serialVersionID :: a -> VersionID
 serialVersionID _ = ProgramUniqueVID
 dependencies :: a -> [VersionID]
 dependencies _ = []
 
completeID :: Serializable a => a -> VersionID
completeID x = combineVIDs $ serialVersionID x : dependencies x
 
serialize :: Serializable a => a -> Serialized
serialize x = Serialized {dataType = typeID x, serializerVersion = completeID x, dataPacket = toBytes x}

deserialize :: Serializable a => Serialized -> Maybe a
deserialize (Serialized tid sv dp) | tid /= typeID result = Nothing
                                   | sv /= completeID result = error "Version of serializer used for this object does not match the current one."
                                   | otherwise = Just result
 where result = fromBytes dp

-----------------

storeInByteString :: Serialized -> IO LazyByteString
storeInByteString obj = do pid <- programVersionID
                           -- Prepend version identifier with 0 if manual or 1 if generated
                           let vid = B.pack $ case serializerVersion obj of
                                               VersionID id -> 0 : bytes id
                                               ProgramUniqueVID -> 1 : bytes pid
                           let tid = BS.pack $ dataType obj ++ "\0"
                           let pdata = dataPacket obj
                           let datalen = B.pack $ bytes $ B.length pdata
                           let libid = BS.pack libraryVersion
                           return $ BL.fromChunks
                                     [
                                      libid,   -- Identifier of serialization library
                                      vid,     -- Version identifier
                                      tid,     -- NULL-terminated string uniquely identifying the type
                                      datalen, -- Length of object data
                                      pdata    -- Actual data of serialized object
                                     ]
                            
-- TODO: throw proper exception on failure
loadFromBytes :: LazyByteString -> IO (Serialized, LazyByteString)
loadFromBytes str = do let str0 = BL.unpack str
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
                            
                       return (Serialized (asciiString tid) pvid (B.pack dat), BL.pack str5)
 where asciiString :: [Word8] -> String
       asciiString = map $ chr . fromIntegral
                            

hStore :: Handle -> Serialized -> IO ()
hStore h ob = storeInByteString ob >>= BL.hPutStr h

hLoads :: Handle -> IO [Serialized]
hLoads h = BL.hGetContents h >>= loadContent
 where loadContent str | BL.null str = return []
                       | otherwise   = do (x, rest) <- loadFromBytes str
                                          xs <- loadContent rest
                                          return (x:xs)
                                          
hLoad :: Handle -> IO Serialized
hLoad h = do xs <- hLoads h
             case xs of
              []  -> error "Nothing to be read from handle."
              [x] -> return x
              _   -> error "Handle contains more data than a single serialized object."
                                           

store :: FilePath -> Serialized -> IO ()
store path ob = withBinaryFile path WriteMode $ flip hStore ob

stores :: FilePath -> [Serialized] -> IO ()
stores path obs = withBinaryFile path WriteMode 
                    $ \h -> mapM_ (hStore h) obs

load :: FilePath -> IO Serialized
load path = withBinaryFile path ReadMode hLoad

loads :: FilePath -> IO [Serialized]
loads path = withBinaryFile path ReadMode hLoads


-----------------
        
-----------------

instance Serializable ByteString where
 serialVersionID _ = VersionID 1
 toBytes = id
 fromBytes = id
 
instance Serializable a => Serializable [a] where
 serialVersionID _ = VersionID 1
 dependencies xs = [serialVersionID $ head xs]
 toBytes [] = BS.empty
 toBytes (x:xs) = BS.concat [B.pack $ bytes $ BS.length sx, sx, toBytes xs]
  where sx = toBytes x
 fromBytes str | BS.null str = []
               | otherwise = let (len, str1)  = BS.splitAt 4 str
                                 (str2, rest) = BS.splitAt (unbytes $ B.unpack len) str1
                              in fromBytes str2 : fromBytes rest     
                              
instance Serializable String where
 serialVersionID _ = VersionID 1
 toBytes = encodeUtf8 . T.pack
 fromBytes = T.unpack . decodeUtf8
                              
instance Serializable a => Serializable (Maybe a) where
 serialVersionID _ = VersionID 1
 dependencies m = [serialVersionID $ fromJust m]
 toBytes Nothing  = BS.empty
 toBytes (Just x) = B.cons 0 $ toBytes x
 fromBytes str | BS.null str = Nothing
               | otherwise   = Just $ fromBytes $ B.tail str
               
instance Serializable () where
 serialVersionID _ = VersionID 1
 toBytes   _ = B.empty
 fromBytes _ = ()
               
instance (Serializable a, Serializable b) => Serializable (a,b) where
 serialVersionID _ = VersionID 1
 dependencies t = [serialVersionID $ fst t, serialVersionID $ snd t]
 toBytes (a,b) = toBytes [Left a, Right b]
 fromBytes str = let [Left a, Right b] = fromBytes str in (a,b)
 
instance (Serializable a, Serializable b) => Serializable (Either a b) where
 serialVersionID _ = VersionID 1
 dependencies e = [serialVersionID $ lefttype e, serialVersionID $ righttype e]
  where lefttype  :: Either a b -> a
        righttype :: Either a b -> b
        lefttype = undefined
        righttype = undefined
 toBytes (Left  x) = B.cons 0 $ toBytes x
 toBytes (Right x) = B.cons 1 $ toBytes x
 fromBytes str | B.head str == 0 = Left  $ fromBytes $ B.tail str
               | otherwise       = Right $ fromBytes $ B.tail str
               
instance Serializable Char where
 serialVersionID _ = VersionID 1
 toBytes = toBytes . ord
 fromBytes = chr . fromBytes
 
instance Serializable Int where
 serialVersionID _ = VersionID 1
 toBytes = B.pack . bytes
 fromBytes = unbytes . B.unpack
 
instance (IArray ar e, Typeable2 ar, Ix i, Serializable i, Serializable e) => Serializable (ar i e) where
 serialVersionID _ = VersionID 1
 dependencies ar = [serialVersionID $ head $ indices ar, serialVersionID $ head $ elems ar]
 toBytes ar   = toBytes (bounds ar, elems ar)
 fromBytes ar = listArray bnds els
  where (bnds, els) = fromBytes ar
  

-- TODO: Integer, Float, Text, Double, Word etc.
