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

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Map (Map)
import qualified Data.Map as M

import Data.Typeable
import Data.Data
import Data.Generics

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Test = Test Integer Integer
          | Test2 Float Int (Test3 Bool Test)
          deriving (Typeable, Data, Show)
          
data Test3 a b = Blaat a (Either a b) b
               | Blah
               deriving (Typeable, Data, Show)

genericToBytes :: (Data a) => SerializationSettings -> a -> [Byte]
genericToBytes set d = case specializedSerializer set d of
                        Serializer to _ -> to d
                        NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                            -- TODO: Can it be safely assumed that a datatype has less than 256 alternatives?
                                            AlgRep _     -> fromIntegral (constrIndex $ toConstr d) : concatWithLengths (gmapQ (genericToBytes set) d) 
                                            _ -> undefined
 where concatWithLengths [] = []
       concatWithLengths (x:xs) = varbytes (length x) ++ x ++ concatWithLengths xs


data Unfolder r = Unfolder [Byte] r              
 
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> a
genericFromBytes set bs = result
 where result = case specializedSerializer set result of
                        Serializer _ from -> from bs
                        NoSerializer      -> case dataTypeRep $ dataTypeOf result of
                                              AlgRep ctors -> let (i:xs) = bs
                                                                  ctor = ctors !! (fromIntegral i - 1)
                                                                  (Unfolder left x) = gunfold unfolder (Unfolder xs) ctor
                                                               in if null left
                                                                   then x 
                                                                   else error $ "Not all bytes are consumed when deserializing as "
                                                                                  ++ dataTypeName (dataTypeOf result) ++ "."
                                              _ -> undefined
       unfolder :: Data b => Unfolder (b -> r) -> Unfolder r
       unfolder (Unfolder bs f) = let (len, bs')   = varunbytes bs
                                      (curr, rest) = splitAt len bs'
                                   in Unfolder rest (f $ genericFromBytes set curr)
       
       
genericTest :: (Data a) => a -> a
genericTest = genericFromBytes defaultSettings . genericToBytes defaultSettings

--TODO
genericVersionID :: (Data a) => SerializationSettings -> a -> VersionID
genericVersionID _ _ = ProgramUniqueVID

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
