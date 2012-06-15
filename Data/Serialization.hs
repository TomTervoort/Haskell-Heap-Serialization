{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, OverlappingInstances, ScopedTypeVariables #-}

-------------------------------------
-- | 
-- [headers]
--
-- Contains an easily extensible 'Serializable' class that allows converting an object from and to 
-- binary data. Versioning and dynamic typing are also handled, as well as writing to and reading 
-- from files and such.
--
-- 'Data.Serialization.Generic' specifies generic versions of the serializer and deserializer that
-- can be used for arbitrary data structures. It can be used in conjunction with custom 
-- 'Serializable' instances if you want to manually introduce optimizations to the binary 
-- representation of (a partial) structure.
-------------------------------------

module Data.Serialization 
    (
      -- * Types
      Serialized,
      LazyByteString,
      SerializationException (..),
      
      -- * Serializable class
      Serializable,
      
      -- * Serialization
      serialize,
      deserialize,
      
      -- * Storage and retrieval of serialized objects
      store,
      load,
      stores,
      loads,
      hStore,
      hStores,
      hLoad,
      hLoads,
      storeInByteString,
      loadFromByteString
    ) where

import Data.Serialization.Internal
import Data.Serialization.Internal.IntegralBytes
--import Data.Serialization.Internal.ProgramVersionID

import Prelude hiding (catch)
import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.Word
import Data.Bits
import Data.Typeable

import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Array.IArray

-----------------

-- | Type definition that is useful to distinguish lazy from regular @ByteString@s.
type LazyByteString = BL.ByteString


----------------------

-- | Is thrown by the deserialization functions in case something goes wrong.
data SerializationException = IncompatibleLibraries
                                -- ^If an object has been serialized with another version of this 
                                -- library.
                            | IncompatibleSerializers
                                -- ^In case a different version of the serializer for a particular
                                -- object was used.
                            | InvalidSerializedData
                                -- ^When the data read from a file or such does not conform to the
                                -- protocol.
                            | DeserializationError String --TODO: utilise this
                                -- ^When deserializing, an exception was thrown by the instance of 
                                -- @Serializable. Contains a description.
                            deriving (Eq, Typeable)
                            
instance Show SerializationException where
 show ex = case ex of
            IncompatibleLibraries     -> "This object appears to have been serialized with a " 
                                        ++ "different version of this library."
            IncompatibleSerializers   -> "Incompatible serializers used."
            InvalidSerializedData     -> "Data does not conform to protocol."
            DeserializationError str  -> "Deserialization error: '" ++ str ++ "'"
            
instance Exception SerializationException
 
-----------------

-- Returns a unique identifier for the type of a Typeable value.
typeID :: Typeable a => a -> TypeID
typeID = show . typeOf

-- Current version of the serialization library. This String is included in serialized packages.
libraryVersion :: String
libraryVersion = "serihask001"

-----------------
                           
-- Create a version identifier for the serializer of a type and all its dependencies.
completeID :: Serializable a => a -> VersionID
completeID x = combineVIDs $ serialVersionID x : dependencies x
 
-- | Encode a 'Serializable' object into a binary representation that also contains type and 
-- version info. The resulting object can then be written to a file or stream using the storage
-- functions.
serialize :: Serializable a => a -> IO Serialized
-- IO monad is redundant but is more consistant with deserializer and generic serializer.
serialize x = return Serialized {dataType = typeID x, 
                                 serializerVersion = completeID x, 
                                 dataPacket = B.pack $ toBytes x}

-- | Decodes a @Serialized@ object back into a Haskell structure. In order to assert the type is
-- correct, @Nothing@ is returned when the inferred type is not correct. This works similarly as
-- 'Data.Typeable.cast'.
--
-- When a version incompatibility or other error occurs, a @SerializationException@ is thrown.
deserialize :: Serializable a => Serialized -> IO (Maybe a)
deserialize (Serialized tid sv dp) | tid /= typeID result = return Nothing
                                   | sv /= completeID result = throwIO IncompatibleSerializers
                                   | otherwise = catches (return $ Just result) exHandlers
 where result = fromBytes $ B.unpack dp
       exHandlers = [Handler $ \(e :: SerializationException) -> throw e,
                     Handler $ \(e :: SomeException) -> throw (DeserializationError $ show e)]

-----------------

-- | Store the binary representation of a serialized object in a @LazyByteString@.
storeInByteString :: Serialized -> IO LazyByteString
storeInByteString obj = do -- pid <- programVersionID
                           -- Prepend version identifier with 0 if manual or 1 if generated
                           let vid = B.pack $ case serializerVersion obj of
                                               VersionID id -> bytes (fromIntegral id :: Word64)
                                               -- ProgramUniqueVID -> 1 : bytes pid
                           let tid = BS.pack $ dataType obj ++ "\0"
                           let pdata = dataPacket obj
                           let datalen = B.pack $ varbytes $ B.length pdata
                           let libid = BS.pack libraryVersion
                           return $ BL.fromChunks
                                     [
                                      libid,   -- Identifier of serialization library
                                      vid,     -- Version identifier
                                      tid,     -- NULL-terminated string uniquely identifying the type
                                      datalen, -- Length of object data
                                      pdata    -- Actual data of serialized object
                                     ]
                            

-- | Reads a serialized object from the beginning of a lazy bytestring. Returns it along with the 
-- rest of the string. Throws a @SerializationException@ when an error occurs.
loadFromByteString :: LazyByteString -> IO (Serialized, LazyByteString)
loadFromByteString str = do let str0 = BL.unpack str
                            let (lid, str1) = splitAt (length libraryVersion) str0
                            let (vid, str2) = splitAt 9 str1
                            let (tid, str3) = (takeWhile (/= 0) str2, drop (length tid + 1) str2)                            
                            let (len, str4) = varunbytes str3
                            let (dat, str5) = splitAt len str4
                            
                            -- Assert str3 is not empty.
                            when (null str3) $ throw InvalidSerializedData
                            
                            -- Check library versions.
                            when (asciiString lid /= libraryVersion) $ throw IncompatibleLibraries
                            
                            {-- pvid <- case vid of
                                     (0:id) -> return $ VersionID $ unbytes id
                                     (1:id) -> do pid <- programVersionID
                                                  when (pid /= unbytes id) $ error "Object was not serialized with the current build of this application."
                                                  return ProgramUniqueVID
                                     _      -> error "Invalid data" --}
                            let pvid = VersionID $ unbytes vid
                                 
                            return (Serialized (asciiString tid) pvid (B.pack dat), BL.pack str5)
                       
 where asciiString :: [Word8] -> String
       asciiString = map $ chr . fromIntegral
                            

-- | Writes a serialized object to a handle.
hStore :: Handle -> Serialized -> IO ()
hStore h ob = storeInByteString ob >>= BL.hPutStr h

-- | Writes multiple serialized objects to a handle.
hStores :: Handle -> [Serialized] -> IO ()
hStores h obs = forM_ obs $ hStore h

-- | Loads multiple serialized objects from a handle. Note: this handle may not contain any data
-- besides these objects.
hLoads :: Handle -> IO [Serialized]
hLoads h = BL.hGetContents h >>= loadContent
 where loadContent str | BL.null str = return []
                       | otherwise   = do (x, rest) <- loadFromByteString str
                                          xs <- loadContent rest
                                          return (x:xs)
                                          
-- | Loads a single serialized object from a handle. Note: this handle may not contain any data
-- besides this objects.
hLoad :: Handle -> IO Serialized
-- TODO: only read neccessary part.
hLoad h = do xs <- hLoads h
             case xs of
              []  -> error "Nothing to be read from handle."
              [x] -> return x
              _   -> error "Handle contains more data than a single serialized object."
                                           
-- | Writes a serialized object to a file.
store :: FilePath -> Serialized -> IO ()
store path ob = withBinaryFile path WriteMode $ flip hStore ob

-- | Writes multiple serialized objects to a file.
stores :: FilePath -> [Serialized] -> IO ()
stores path obs = withBinaryFile path WriteMode $ \h -> hStores h obs

-- | Reads a serialized from a file.
load :: FilePath -> IO Serialized
load path = withBinaryFile path ReadMode hLoad

-- | Reads multiple serialized objects from a file.
loads :: FilePath -> IO [Serialized]
loads path = withBinaryFile path ReadMode hLoads

----------------------

instance Serializable ByteString where
 serialVersionID _ = VersionID 1
 toBytes = B.unpack
 fromBytes = B.pack
               
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
 
instance Serializable Text where
 serialVersionID _ = VersionID 1
 toBytes = B.unpack . encodeUtf8
 fromBytes = decodeUtf8 . B.pack
 
instance Serializable Int where
 serialVersionID _ = VersionID 1
 toBytes = toBytes . toInteger
 fromBytes = fromInteger . fromBytes
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
 toBytes i | i < 0 = toBytes (-i) ++ [0]
           | otherwise = fromIntegral (i .&. 0xff) : toBytes (i `rotateR` 8)
 fromBytes xs = case reverse xs of
                 (0:ys) -> (-1) * fromBytes' 0 ys
                 ys      -> fromBytes' 0 ys
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
                    
instance Serializable Bool where
 serialVersionID _ = VersionID 1
 toBytes False = [0]
 toBytes True = [maxBound]
 fromBytes [x] = x /= 0
 constBytesSize _ = Just 1
 -- List specialization: encode as bitstrings.
 listToBytes xs = let padding = 8 - length xs `mod` 8
                   in fromIntegral padding : ltb (replicate padding False ++ xs)
  where ltb [] = []
        ltb xs = let (a,b) = splitAt 8 xs
                  in foldr setBit' 0 (zip (reverse [0..7]) a) : ltb b
        setBit' (bit, bool) | bool      = flip setBit bit
                            | otherwise = flip clearBit bit
 listFromBytes (padding:xs) = drop (fromIntegral padding) $ lfb xs
  where lfb [] = []
        lfb (x:xs) = map (testBit x) (reverse [0..7]) ++ lfb xs
        
instance Serializable a => Serializable [a] where
 serialVersionID _ = VersionID 1
 toBytes = listToBytes
 fromBytes = listFromBytes
 dependencies xs = [sid $ head xs]
 
instance Serializable a => Serializable (Maybe a) where
 serialVersionID _ = VersionID 1
 toBytes Nothing = []
 toBytes (Just x) | constBytesSize x > Just 0 = toBytes x
                  | otherwise = 0 : toBytes x
 fromBytes [] = Nothing
 fromBytes xs = result
  where result | constBytesSize result > Just 0 = fromBytes xs
               | otherwise = fromBytes $ tail xs
