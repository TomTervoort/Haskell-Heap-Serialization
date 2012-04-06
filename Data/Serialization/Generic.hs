{-# LANGUAGE DeriveDataTypeable #-}
module Data.Serialization.Generic where

import Data.Serialization

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Typeable
import Data.Data
import Data.Generics

data Foo = Foo Char
         | Bar
         | Blaat
          deriving (Typeable, Data)

genericToBytes :: (Data a) => a -> [Byte]
genericToBytes d = case dataTypeRep $ dataTypeOf d of
                    AlgRep ctors -> (fromIntegral $ fromJust $ findIndex (== ctor) ctors) : concat (gmapQ genericToBytes d)
                    IntRep       -> undefined
                    FloatRep     -> undefined
                    CharRep      -> (mkQ [] (toBytes :: Char -> [Byte])) d
                    NoRep        -> undefined
 where ctor = toConstr d
 
genericFromBytes :: (Data a) => [Byte] -> a
genericFromBytes bs = result
 where result = case dataTypeRep $ dataTypeOf result of
                      AlgRep ctors -> undefined
                      IntRep       -> undefined
                      FloatRep     -> undefined
                      CharRep      -> undefined
                      NoRep        -> undefined
