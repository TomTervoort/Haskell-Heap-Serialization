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

--------------------------

type ToByter m state = forall a. Data a => a -> state -> m ([Byte], state)
type FromByter m state = forall a. Data a => [Byte] -> state -> m (a, state)

class Data a => Serializable1 a where
 toBytes1   :: Monad m => ToByter m s -> a -> s -> m ([Byte], s)
 fromBytes1 :: Monad m => FromByter m s -> [Byte] -> s -> m (a, s)
 serialVersionID1 :: a -> VersionID
 
instance Data a => Serializable1 [a] where
 serialVersionID1 _ = VersionID 1
 toBytes1 _ [] s = return ([],s)
 toBytes1 f (x:xs) s = do (bs, s') <- f x s
                          (rest, s'') <- toBytes1 f xs s'
                          let len = varbytes $ length bs
                          return (len ++ bs ++ rest, s'')
 fromBytes1 _ [] s = return ([], s)
 fromBytes1 f bs s = do let (len, bs') = varunbytes bs
                        let (curr, rest) = splitAt len bs'
                        (x, s')   <- f curr s
                        (xs, s'') <- fromBytes1 f rest s'
                        return (x : xs, s'')
 
instance Data a => Serializable1 (Maybe a) where
 serialVersionID1 _ = VersionID 1
 toBytes1 _ Nothing s  = return ([],s)
 toBytes1 f (Just x) s = do (bs, s') <- f x s
                            return (0 : bs, s')
 fromBytes1 _ [] s = return (Nothing, s)
 fromBytes1 f (_:xs) s = do (v, s') <- f xs s
                            return (Just v, s')

----------------------------

data Serializer a = Serializer (a -> [Byte]) ([Byte] -> a)
                  | forall m s. Serializer1 (ToByter m s -> a -> m ([Byte], s))
                                            (FromByter m s -> [Byte] -> m (a, s))
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
       
                          

sWrapper1 :: Serializable1 (f a) => f a -> [(TypeKey, SWrapper)]
sWrapper1 x = undefined --TODO
                                     
                          
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


