{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, UndecidableInstances, CPP #-}
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
import Data.Ord

import Data.IORef
import Data.Typeable
import Data.Data
import Data.Generics
import System.IO.Unsafe

import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)

import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import Data.Array.IO (IOArray)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)

import Debug.Trace

data Serializer a = Serializer (a -> [Byte]) ([Byte] -> a)
                  | Serializer1 (forall b. Data b => (b -> [Byte], [Byte] -> b) -> Serializer a)
                  | NoSerializer


data SWrapper = SWrapper (Generic Serializer) VersionID

data TypeKey = TypeKey {unKey :: (Either TypeRep TyCon)} deriving Eq

data SerializationSettings = SerializationSettings {
                               specializedInstances :: Map TypeKey SWrapper,
                               settingsVID :: VersionID
                             }

instance Ord TypeKey where
#if __GLASGOW_HASKELL__ >= 700
 compare = comparing unKey
#else
 compare = comparing $ either (Left . unsafePerformIO . typeRepKey) (Right . tyConString) . unKey
#endif

{-- instance Hashable TypeKey where
 hash = either (hash . unsafePerformIO . typeRepKey) (hash . tyConString) . unKey --}


typeKey :: TypeRep -> TypeKey
typeKey = TypeKey . Left

typeKey1 :: TypeRep -> TypeKey
typeKey1 = TypeKey . Right . typeRepTyCon

assertType :: a -> a -> a
assertType _ x = x

assertType1 :: f a -> f b -> f b
assertType1 _ x = x

sWrapper :: (Serializable a, Data a) => a -> [(TypeKey, SWrapper)]
sWrapper x = [(typeKey $ typeOf  x , SWrapper getSerializer  $ serialVersionID x),
              (typeKey $ typeOf [x], SWrapper listSerializer $ serialVersionID x)]
 where getSerializer :: Data b => b -> Serializer b
       getSerializer y | typeOf y == typeOf x 
                          = Serializer (toBytes . assertType x . fromJust . cast) 
                                       (fromJust . cast . assertType x . fromBytes)
                       | otherwise =  NoSerializer
                       
       listSerializer :: Data b => b -> Serializer b
       listSerializer y | typeOf y == typeOf [x]
                           = Serializer (listToBytes . assertType [x] . fromJust . cast) 
                                        (fromJust . cast . assertType [x] . listFromBytes)
                        | otherwise = NoSerializer
       
                          

{-- sWrapper1 :: (Serializable1 f, Data (f a)) => f a -> (TypeKey, SerializationSettings -> SWrapper)
sWrapper1 x = (typeKey1 $ typeOf1 x, \s -> SWrapper (getSerializer s) $ serialVersionID1 x)
 where getSerializer :: Data b => SerializationSettings -> b -> Serializer b
       getSerializer set y = if typeRepTyCon (typeOf y) == typeRepTyCon (typeOf1 x)
                              then Serializer1 makeS
                              else NoSerializer
       makeS :: (Data b, Data c) => (c -> [Byte], [Byte] -> c) -> Serializer b
       makeS (to, from) = Serializer (toBytes1 to . assertType1 x . fromJust . cast)
                                     (fromJust . cast . assertType1 x . fromBytes1 from) --}
                          
specializedSerializer :: (Data a) => SerializationSettings -> a -> Serializer a
specializedSerializer set x = findMatch $ specializedInstances set
 where findMatch map = {-- case M.lookup (typeKey1 $ typeOf x) map of
                        Just f  -> let (SWrapper s _) = f set in s x
                        Nothing -> --}
                       case M.lookup (typeKey $ typeOf x) map of
                        Just (SWrapper s _) -> s x
                        Nothing -> NoSerializer
                                                                         
standardSpecializations :: [(TypeKey, SWrapper)]
standardSpecializations = concat 
                           [
                             sWrapper (u :: Int),
                             sWrapper (u :: Integer),
                             sWrapper (u :: ByteString),
                             sWrapper (u :: ()),
                             sWrapper (u :: Char),
                             sWrapper (u :: Text),
                             sWrapper (u :: Word),
                             sWrapper (u :: Byte),
                             sWrapper (u :: Float),
                             sWrapper (u :: Double),
                             sWrapper (u :: Bool)
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
addSerializableSpecialization x set = set {specializedInstances = 
                                            foldr (\(k,v) map -> M.insert k v map) 
                                                  (specializedInstances set)
                                                  (sWrapper x),                                                                    
                                           settingsVID = combineVIDs [serialVersionID x, 
                                                                      typeVID x,
                                                                      settingsVID set]}
 where typeVID = VersionID . checksumInt . concatMap (bytes . ord) . show . typeOf

addSerSpec :: (Serializable a, Data a) => a -> SerializationSettings -> SerializationSettings
addSerSpec = addSerializableSpecialization


