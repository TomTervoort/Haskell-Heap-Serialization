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

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as M

import Data.Hashable
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

genericToBytes :: (Data a) => SerializationSettings -> a -> [Byte]
genericToBytes set d = case specializedSerializer set d of
                        Serializer to _ -> to d
                        NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                            AlgRep _     -> varbytes (constrIndex $ toConstr d) ++ concatWithLengths (gmapQ (genericToBytes set) d) 
                                            _ -> error $ "A specialized serializer is required for type " ++ dataTypeName (dataTypeOf d)
 where concatWithLengths [] = []
       concatWithLengths (x:xs) = varbytes (length x) ++ x ++ concatWithLengths xs


data Unfolder r = Unfolder [Byte] r              
 
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> a
genericFromBytes set bs = result
 where result = case specializedSerializer set result of
                        Serializer _ from -> from bs
                        NoSerializer      -> case dataTypeRep $ dataTypeOf result of
                                              AlgRep ctors -> let (i,xs) = varunbytes bs
                                                                  ctor = ctors !! (i - 1)
                                                                  (Unfolder left x) = gunfold unfolder (Unfolder xs) ctor
                                                               in if null left
                                                                   then x 
                                                                   else error $ "Not all bytes are consumed when deserializing as "
                                                                                  ++ dataTypeName (dataTypeOf result) ++ "."
                                              _ -> undefined -- Should not occur if versions match.
       unfolder :: Data b => Unfolder (b -> r) -> Unfolder r
       unfolder (Unfolder bs f) = let (len, bs')   = varunbytes bs
                                      (curr, rest) = splitAt len bs'
                                   in Unfolder rest (f $ genericFromBytes set curr)
       
       
genericTest :: (Data a) => a -> a
genericTest = genericFromBytes defaultSettings . genericToBytes defaultSettings

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
gsTypeID x = '$' : dataTypeName (dataTypeOf x)

serializeWith :: Data a => SerializationSettings -> a -> Serialized
serializeWith set x = Serialized {
                                  -- The type name is prepended with a $ in order to indicate usage
                                  -- of the generic serializer.
                                  dataType = gsTypeID x,
                                  serializerVersion = genericVersionID set x, 
                                  dataPacket = B.pack $ genericToBytes set x
                                 }

serialize :: Data a => a -> Serialized
serialize = serializeWith defaultSettings

deserializeWith :: Data a => SerializationSettings -> Serialized -> Maybe a
deserializeWith set (Serialized tid sv dp) | tid /= gsTypeID result = Nothing
                                           | sv /= genericVersionID set result = error "Version of serializer used for this object does not match the current one."
                                           | otherwise = Just result
  where result = genericFromBytes set $ B.unpack dp

deserialize :: Data a => Serialized -> Maybe a
deserialize = deserializeWith defaultSettings
