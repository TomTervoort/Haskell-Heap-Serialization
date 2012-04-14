{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Data.Serialization.Settings where

import Data.Serialization
import Data.Serialization.Internal.IntegralBytes
import Data.Serialization.Internal.ProgramVersionID

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

import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import Data.Array.IO (IOArray)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)

data Serializer a = Serializer (a -> [Byte]) ([Byte] -> a)
                  | NoSerializer


data SWrapper = SWrapper (Generic Serializer) VersionID

type TypeKey = Int -- TODO: Replace with TypeRep

data SerializationSettings = SerializationSettings {
                               specializedInstances :: Map TypeKey SWrapper,
                               settingsVID :: VersionID
                             }
                             
typeKey :: TypeRep -> TypeKey
typeKey = unsafePerformIO . typeRepKey -- TODO: replace with id

assertType :: a -> a -> a
assertType _ x = x

sWrapper :: (Serializable a, Data a) => a -> (TypeKey, SWrapper)
sWrapper x = (typeKey $ typeOf x, SWrapper getSerializer $ serialVersionID x)
 where getSerializer :: Data b => b -> Serializer b
       getSerializer y = if typeOf y == typeOf x
                          then Serializer (toBytes . assertType x . fromJust . cast) (fromJust . cast . assertType x . fromBytes)
                          else NoSerializer 
                          

sWrapper1 :: (Serializable1 f, Data (f a)) => SerializationSettings -> f a -> (TypeKey, SWrapper)
sWrapper1 set x = (typeKey $ typeOf x, SWrapper getSerializer $ serialVersionID1 x)
 where getSerializer :: Data b => b -> Serializer b
       getSerializer y = if typeRepTyCon (typeOf y) == typeRepTyCon (typeOf1 x)
                          then case specializedSerializer set y of
                                n@NoSerializer -> n
                                Serializer to from -> undefined --TODO
                          else NoSerializer
                          
specializedSerializer :: (Data a) => SerializationSettings -> a -> Serializer a
specializedSerializer set x = findMatch $ specializedInstances set
 where findMatch map = case M.lookup (typeKey $ typeOf x) map of
                        Nothing             -> NoSerializer
                        Just (SWrapper s _) -> s x
                                                                         
standardSpecializations :: [(TypeKey, SWrapper)]
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
                                          specializedInstances = M.fromList standardSpecializations,
                                          settingsVID = VersionID 1
                                        }

addSerializableSpecialization :: (Serializable a, Data a) => a -> SerializationSettings -> SerializationSettings
addSerializableSpecialization x set = set {specializedInstances = uncurry M.insert (sWrapper x) 
                                                                    $ specializedInstances set,
                                           settingsVID = combineVIDs [serialVersionID x, 
                                                                      typeVID x,
                                                                      settingsVID set]}
 where typeVID = VersionID . checksumInt . concatMap (bytes . ord) . show . typeOf

addSerSpec :: (Serializable a, Data a) => a -> SerializationSettings -> SerializationSettings
addSerSpec = addSerializableSpecialization


