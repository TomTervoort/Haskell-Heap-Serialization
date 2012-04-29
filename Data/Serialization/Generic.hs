{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExistentialQuantification #-}
module Data.Serialization.Generic (
                                     module Data.Serialization,
                                     serializeWith,
                                     serialize,
                                     deserializeWith,
                                     deserialize
                                  ) where

import Data.Serialization hiding (serialize, deserialize)
import Data.Serialization.Settings
import Data.Serialization.Internal
import Data.Serialization.Internal.IntegralBytes 
import Data.Serialization.Internal.ProgramVersionID
import Data.Serialization.Internal.PtrSet

import Data.List
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits
import Data.IORef
import System.IO
import Data.Dynamic

import Data.Hashable
import Data.HashMap (Map)
import qualified Data.HashMap as M
import Data.HashSet (Set)
import qualified Data.HashSet as S

import Data.Typeable
import Data.Data
import Data.Generics

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Debug.Trace

----

data Test = Test Integer Integer
          | Test2 Float Int (Test3 Bool Test)
          deriving (Typeable, Data, Show)
          
data Test3 a b = Blaat a (Either a b) b
               | Blah
               deriving (Typeable, Data, Show)
               
-----

type RefMap = Map PtrKey [Byte]

shorter :: [a] -> Int -> Bool
shorter _ 0  = False
shorter [] _ = True
shorter (_:xs) n = shorter xs (n - 1)

concatWithLengths :: [[Byte]] -> [Byte]
concatWithLengths [] = []
concatWithLengths (x:xs) = varbytes (length x) ++ x ++ concatWithLengths xs

genericToBytes :: (Data a) => SerializationSettings -> a -> IO [Byte]
genericToBytes s d = do ps <- newPtrSet
                        (key, _, rm) <- genericToBytes' s d (ps, M.empty)
                        return $ varbytes key ++ serializeRefMap rm

 where serializeRefMap :: RefMap -> [Byte]
       serializeRefMap = concatWithLengths . map (\(k,bs) -> varbytes k ++ bs) . M.toList

genericToBytes' :: (Data a) => SerializationSettings -> a -> (PtrSet, RefMap) -> IO (PtrKey, PtrSet, RefMap)
genericToBytes' set d (ps, rm) = do memb <- ptrSetMember d ps
                                    case memb of
                                     Just k  -> return (k, ps, rm)
                                     Nothing -> do (bs, ps', rm') <- use (specializedSerializer set d)
                                                   (ps'', k) <- ptrSetAdd' d ps'
                                                   let rm'' = M.insert k bs rm'
                                                   return (k, ps'', rm'')
 
 where use s = case s of
                Serializer to _ -> return (to d, ps, rm)
                Serializer1 f   -> undefined -- TODO
                NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                    AlgRep ctors -> do let fs = gmapQ (genericToBytes' set) d
                                                       ref <- newIORef (ps, rm)
                                                       xs <- forM fs
                                                               (\f -> do tup <- readIORef ref
                                                                         (k, ps', rm') <- f tup
                                                                         writeIORef ref (ps', rm')
                                                                         return $ varbytes k)
                                                       (ps', rm') <- readIORef ref
                                                       let ctorRep | shorter ctors 2 = []
                                                                   | otherwise = varbytes (constrIndex $ toConstr d)
                                                       return (ctorRep ++ concat xs, ps', rm')
                                    _ -> error $ "A specialized serializer is required for type " 
                                                    ++ dataTypeName (dataTypeOf d)


data Unfolder r = Unfolder ([Byte], DeRefMap) r              
 
type DeRefMap = Map PtrKey (Either [Byte] Dynamic)
 
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> IO a
genericFromBytes s b = do let (k, b') = varunbytes b
                          let rm = deRefMap b'
                          let (Just (Left b'')) = M.lookup k rm
                          return . fst $ genericFromBytes' s b'' rm
 where deRefMap :: [Byte] -> DeRefMap
       deRefMap [] = M.empty
       deRefMap xs = let (len, xs1) = varunbytes xs
                         (bs', xs2) = splitAt len xs1
                         (key, bs)  = varunbytes bs'
                      in M.insert key (Left bs) $ deRefMap xs2


genericFromBytes' :: Data a => SerializationSettings -> [Byte] -> DeRefMap -> (a, DeRefMap)
genericFromBytes' set bs drm = result
 where result = case specializedSerializer set $ fst result of
                 Serializer _ from -> (from bs, drm)
                 NoSerializer      -> case dataTypeRep dtype of
                                       AlgRep ctors -> 
                                        let (i,xs) | length ctors == 1 = (1,bs)
                                                   | otherwise = varunbytes bs
                                            ctor = ctors !! (i - 1)
                                            (Unfolder (left, dm) x) = gunfold unfolder (Unfolder (xs,drm)) ctor
                                         in if null left
                                             then (x, dm)
                                             else error $ "Not all bytes are consumed when deserializing as "
                                                            ++ dataTypeName dtype ++ "."
                 _                 -> error $ "A specialized serializer for " ++ dataTypeName dtype
                                               ++ " is required."
       dtype = dataTypeOf $ fst result
       
       unfolder :: Data b => Unfolder (b -> r) -> Unfolder r
       unfolder (Unfolder (bs, dm) f) = let (key, rest) = varunbytes bs
                                            x = lookupData dm key
                                         in Unfolder (rest, snd x) $ f (fst x)
                                         
       lookupData :: Data a => DeRefMap -> PtrKey -> (a, DeRefMap)
       lookupData dm key = case M.lookup key dm of
                            Just (Right d) -> (fromDyn d undefined, dm)
                            Just (Left bs) -> let (x,m) = genericFromBytes' set bs dm
                                               in (x, M.insert key (Right $ toDyn x) dm)
                            _ -> error "Corrupt data: key not in reference map."
       
genericTest :: (Data a) => a -> IO a
genericTest x = genericToBytes defaultSettings x >>= genericFromBytes defaultSettings

genericVersionID :: (Data a) => SerializationSettings -> a -> VersionID
genericVersionID set x = combineVIDs $ [settingsVID set, structureID S.empty $ dataTypeOf x] 
-- Take checksum over characters in data type name and constructor kinds.
 where structureID :: Set String -> DataType -> VersionID
       structureID set d | S.member (dataTypeName d) set  = VersionID 0
                         | otherwise = let newset = S.insert (dataTypeName d) set
                                        in VersionID . checksumInt
                                            $  concatMap (bytes . ord) (dataTypeName d)
                                            ++ 0 : case dataTypeRep d of
                                                    AlgRep ctors -> 0 : concatMap (ctorID newset) ctors
                                                    IntRep       -> [1]
                                                    FloatRep     -> [2]
                                                    CharRep      -> [3]
                                                    NoRep        -> [4]
       ctorID :: Set String -> Constr -> [Byte]
       ctorID set c = 
            concatMap (bytes . ord) (concat $ intersperse "\0" $ showConstr c : constrFields c)
             ++ [0, if constrFixity c == Prefix then 1 else 2] 
             ++ concatMap (\(VersionID x) -> bytes x) (gmapQ (structureID set . dataTypeOf) 
                                                                    $ assertType x $ fromConstr c)

-----------------------

gsTypeID :: Data a => a -> TypeID
gsTypeID x = '$' : show (typeOf x)

serializeWith :: Data a => SerializationSettings -> a -> IO Serialized
serializeWith set x = do packet <- genericToBytes set x
                         return Serialized {
                                  -- The type name is prepended with a $ in order to indicate usage
                                  -- of the generic serializer.
                                  dataType = gsTypeID x,
                                  serializerVersion = genericVersionID set x, 
                                  dataPacket = B.pack $ packet
                                 }

serialize :: Data a => a -> IO Serialized
serialize = serializeWith defaultSettings

deserializeWith :: Data a => SerializationSettings -> Serialized -> IO (Maybe a)
deserializeWith set (Serialized tid sv dp) = do result <- genericFromBytes set $ B.unpack dp
                                                if tid /= gsTypeID result 
                                                 then return Nothing
                                                 else
                                                  if sv /= genericVersionID set result
                                                   then error "Version of serializer used for this object does not match the current one."
                                                   else return $ Just result

deserialize :: Data a => Serialized -> IO (Maybe a)
deserialize = deserializeWith defaultSettings
