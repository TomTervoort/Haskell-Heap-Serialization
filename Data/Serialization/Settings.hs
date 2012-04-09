{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Data.Serialization.Settings where

import Data.Serialization

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.IORef
import Data.Typeable
import Data.Data
import Data.Generics
import System.IO.Unsafe

import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)

import Data.Array (Array)
import Data.Array.IO (IOArray)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)

data Serializer a = Serializer (a -> [Byte]) ([Byte] -> a)
                  | NoSerializer
                  --TODO: | RecursiveSerializer ([SWrapper] -> Serializer a)
                  deriving (Typeable, Data)


data SWrapper = SWrapper (Generic Serializer)

data SerializationSettings = SerializationSettings {
                               specializedInstances :: [SWrapper] -- TODO: Map TypeRep SWrapper
                               -- TODO ...
                             }

assertType :: a -> a -> a
assertType _ x = x

assertType1 :: f a -> f b -> f b
assertType1 _ x = x

sWrapper :: (Serializable a, Data a) => a -> SWrapper
sWrapper x = SWrapper getSerializer
 where getSerializer :: Data b => b -> Serializer b
       getSerializer y = if typeOf y == typeOf x
                          then Serializer (toBytes . assertType x . fromJust . cast) (fromJust . cast . assertType x . fromBytes)
                          else NoSerializer 
                          
standardSpecializations :: [SWrapper]
standardSpecializations = [
                             sWrapper (u :: Int),
                             sWrapper (u :: Integer),
                             sWrapper (u :: ByteString),
                             sWrapper (u :: ()),
                             sWrapper (u :: Char),
                             sWrapper (u :: Text),
                             sWrapper (u :: Word),
                             sWrapper (u :: Byte),
                             sWrapper (u :: Float),
                             sWrapper (u :: Double)
                             --sWrapper1 [()]
                             {--sWrapper1 (u :: Serializable a => Maybe a),
                             sWrapper2 (u :: (Serializable i, Serializable a) => Array i a),
                             sWrapper2 (u :: (Serializable i, Serializable a) => IOArray i a),
                             sWrapper2 (u :: (Serializable i, Serializable a) => UArray i a),
                             sWrapper2 (u :: (Serializable a, Serializable b) => Either a b),
                             sWrapper2 (u :: (Serializable a, Serializable b) => (a,b)),
                             sWrapper3 (u :: (Serializable i, Serializable a) => STArray s i a)--}
                          ]
 where u = undefined
 


defaultSettings :: SerializationSettings
defaultSettings = SerializationSettings {
                                          specializedInstances = standardSpecializations
                                        }

seriSettings :: IORef SerializationSettings
{-# NOINLINE seriSettings #-}
seriSettings = unsafePerformIO $ newIORef defaultSettings

getSerializationSettings :: IO SerializationSettings
getSerializationSettings = readIORef seriSettings

updateSerializationSettings :: (SerializationSettings -> SerializationSettings) -> IO ()
updateSerializationSettings = modifyIORef seriSettings

setSerializationSettings :: SerializationSettings -> IO ()
setSerializationSettings = writeIORef seriSettings

addSerializableSpecialization :: (Serializable a, Data a) => a -> SerializationSettings -> SerializationSettings
addSerializableSpecialization x set = set {specializedInstances = sWrapper x : specializedInstances set}


