{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TypeSynonymInstances, OverlappingInstances #-}
module Data.Serialization 
    (
      Serialized,
      Serializable,
      VersionID (..),
      Byte,
      toBytes,
      fromBytes,
      serialVersionID,
      sid,
      dependencies,
      constBytesSize,
      listToBytes,
      listFromBytes,
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

import Data.Serialization.Internal.IntegralBytes
import Data.Serialization.Internal.ProgramVersionID

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
typeID :: Typeable a => a -> TypeID
typeID = show . typeOf -- TODO: dataTypeName . dataTypeOf

-- Current version of the serialization library.
libraryVersion :: String
libraryVersion = "serihask001"

-- | A Byte is a Word8.
type Byte = Word8

-----------------

-- | A datatype containing a version of a serialization method. ProgramUniqueVID indicates only 
-- | serialized objects of the same build of the same program are compatible.
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
 toBytes   :: a -> [Byte]
 fromBytes :: [Byte] -> a
 
 serialVersionID :: a -> VersionID
 serialVersionID _ = ProgramUniqueVID
 dependencies :: a -> [VersionID]
 dependencies _ = []
 
 constBytesSize :: a -> Maybe Int
 constBytesSize _ = Nothing
 
 listToBytes :: [a] -> [Byte]
 listToBytes l | isJust $ constBytesSize $ head l = concatMap toBytes l
               | otherwise = listToBytes' l
  where listToBytes' [] = []
        listToBytes' (x:xs) = let bx = toBytes x in concat [bytes $ length bx, bx, listToBytes' xs]
 listFromBytes :: [Byte] -> [a]
 listFromBytes l = case constBytesSize $ head defaultresult of
                    Nothing -> defaultresult
                    Just n  -> chunks n l
  where defaultresult = listFromBytes' l
        chunks _ [] = []
        chunks n xs = case splitAt n xs of
                       (c, rest) -> fromBytes c : chunks n rest
        listFromBytes' []  = []
        listFromBytes' str = let (len, str1)  = splitAt 4 str
                                 (str2, rest) = splitAt (unbytes len) str1
                              in fromBytes str2 : listFromBytes' rest
 
 -- TODO: Something like toByteString to prevent unnessesary conversions?
 
-- | Shorthand for serialVersionID.
sid :: Serializable a => a -> VersionID
sid = serialVersionID 

completeID :: Serializable a => a -> VersionID
completeID x = combineVIDs $ serialVersionID x : dependencies x
 
serialize :: Serializable a => a -> Serialized
serialize x = Serialized {dataType = typeID x, serializerVersion = completeID x, dataPacket = B.pack $ toBytes x}

deserialize :: Serializable a => Serialized -> Maybe a
deserialize (Serialized tid sv dp) | tid /= typeID result = Nothing
                                   | sv /= completeID result = error "Version of serializer used for this object does not match the current one."
                                   | otherwise = Just result
 where result = fromBytes $ B.unpack dp

-----------------

storeInByteString :: Serialized -> IO LazyByteString
storeInByteString obj = do pid <- programVersionID
                           -- Prepend version identifier with 0 if manual or 1 if generated
                           let vid = B.pack $ case serializerVersion obj of
                                               VersionID id -> 0 : bytes (fromIntegral id :: Word64)
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


----------------------

instance Serializable ByteString where
 serialVersionID _ = VersionID 1
 toBytes = B.unpack
 fromBytes = B.pack
 
instance Serializable a => Serializable [a] where
 serialVersionID _ = VersionID 1
 dependencies xs = [serialVersionID $ head xs]
 toBytes = listToBytes
 fromBytes = listFromBytes
                              
instance Serializable a => Serializable (Maybe a) where
 serialVersionID _ = VersionID 1
 dependencies m = [serialVersionID $ fromJust m]
 toBytes Nothing  = []
 toBytes (Just x) = 0 : toBytes x
 fromBytes [] = Nothing
 fromBytes (_:xs) = Just $ fromBytes xs
               
instance Serializable () where
 serialVersionID _ = VersionID 1
 toBytes   _ = []
 fromBytes _ = ()
 constBytesSize _ = Just 0
               
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
 toBytes (Left  x) = 0 : toBytes x
 toBytes (Right x) = 1 : toBytes x
 fromBytes (0:xs) = Left  $ fromBytes xs
 fromBytes (1:xs) = Right $ fromBytes xs
               
instance Serializable Char where
 serialVersionID _ = VersionID 1
 toBytes = toBytes . ord
 fromBytes = chr . fromBytes
 -- Specialization for Strings, encode as UTF-8:
 listToBytes = B.unpack . encodeUtf8 . T.pack
 listFromBytes = T.unpack . decodeUtf8 . B.pack
 
instance Serializable Int where
 serialVersionID _ = VersionID 1
 toBytes = bytes
 fromBytes = unbytes
 constBytesSize = Just . (`div` 8) . bitSize
 
instance Serializable Word where
 serialVersionID _ = VersionID 1
 toBytes = bytes
 fromBytes = unbytes
 constBytesSize = Just . (`div` 8) . bitSize
 
instance (IArray ar e, Typeable2 ar, Ix i, Serializable i, Serializable e) => Serializable (ar i e) where
 serialVersionID _ = VersionID 1
 dependencies ar = [serialVersionID $ head $ indices ar, serialVersionID $ head $ elems ar]
 toBytes ar   = toBytes (bounds ar, elems ar)
 fromBytes ar = listArray bnds els
  where (bnds, els) = fromBytes ar
  
instance Serializable Byte where
 serialVersionID _ = VersionID 1
 toBytes b = [b]
 fromBytes [b] = b
 listToBytes = id
 listFromBytes = id
 constBytesSize _ = Just 1
 
instance Serializable Integer where
 serialVersionID _ = VersionID 1
 toBytes 0 = []
 toBytes i = fromIntegral (i .&. 0xff) : toBytes (i `rotateR` 8)
 fromBytes = fromBytes' 0 -- FIXME
  where fromBytes' i []     = i
        fromBytes' i (x:xs) = fromBytes' (i `rotateL` 8 .|. toInteger x) xs
  
floatToBytes :: (RealFloat a) => a -> [Byte]
floatToBytes = toBytes . decodeFloat

floatFromBytes :: (RealFloat a) => [Byte] -> a
floatFromBytes = uncurry encodeFloat . fromBytes

instance Serializable Double where
 serialVersionID _ = VersionID 1
 toBytes = floatToBytes
 fromBytes = floatFromBytes
 dependencies _ = let tup = undefined :: (Int,Integer)
                    in sid tup : dependencies tup
                    
instance Serializable Float where
 serialVersionID _ = VersionID 1
 toBytes = floatToBytes
 fromBytes = floatFromBytes
 dependencies _ = let tup = undefined :: (Int,Integer)
                    in sid tup : dependencies tup


-- TODO: Integer, Float, Text, Double etc.
