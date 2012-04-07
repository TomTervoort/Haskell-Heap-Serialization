{-# LANGUAGE DeriveDataTypeable #-}
module Data.Serialization.Generic where

import Data.Serialization
import Data.Serialization.Settings
import Data.Serialization.Internal.IntegralBytes 

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Typeable
import Data.Data
import Data.Generics

data Test = Test Integer Integer deriving (Typeable, Data)

genericToBytes :: (Data a) => SerializationSettings -> a -> [Byte]
genericToBytes set d = case specializedSerializer set d of
                        Serializer to _ -> to d
                        NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                            -- TODO: Can it be safely assumed that a datatype has less than 256 alternatives?
                                            AlgRep _     -> fromIntegral (constrIndex $ toConstr d) : concatWithLengths (gmapQ (genericToBytes set) d) 
                                            _ -> undefined
 where concatWithLengths [] = []
       concatWithLengths (x:xs) = bytes (length x) ++ x ++ concatWithLengths xs
                                
 
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> a
genericFromBytes set bs = result
 where result = case specializedSerializer set result of
                        Serializer _ from -> from bs
                        NoSerializer      -> case dataTypeRep $ dataTypeOf result of
                                              AlgRep ctors -> let (i:xs) = bs
                                                                  ctor = ctors !! (fromIntegral i - 1)
                                                               in undefined --TODO
                                              _ -> undefined


specializedSerializer :: (Data a) => SerializationSettings -> a -> Serializer a
specializedSerializer set x = findMatch $ map (\(SWrapper f) -> f x) $ specializedInstances set
 where findMatch [] = NoSerializer
       findMatch (s@(Serializer _ _):_) = s
       findMatch (_:xs) = findMatch xs
