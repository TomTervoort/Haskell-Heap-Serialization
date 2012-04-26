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

import Data.HashMap (Map)
import qualified Data.HashMap as M

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
                        (bs, _, rm) <- genericToBytes' s d (ps, M.empty)
                        let rmbytes = serializeRefMap rm
                        return $ varbytes (length rmbytes) ++ rmbytes ++ bs

 where serializeRefMap :: RefMap -> [Byte]
       serializeRefMap = concatWithLengths . map (\(k,bs) -> varbytes k ++ bs) . M.toList

genericToBytes' :: (Data a) => SerializationSettings -> a -> (PtrSet, RefMap) -> IO ([Byte], PtrSet, RefMap)
genericToBytes' set d (ps, rm) = do memb <- ptrSetMember d ps
                                    case memb of
                                     Just k  -> return (varbytes k, ps, rm)
                                     Nothing -> do t@(bs, ps', rm') <- use (specializedSerializer set d)
                                                   if shorter bs (sharingLimit set + 1)
                                                    then return t
                                                    else do (ps'', k) <- ptrSetAdd' d ps'
                                                            let rm'' = M.insert k bs rm'
                                                            return (varbytes k, ps'', rm'')
 where use s = case s of
                Serializer to _ -> return (to d, ps, rm)
                Serializer1 f   -> undefined -- TODO
                NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                    AlgRep ctors -> do let fs = gmapQ (genericToBytes' set) d
                                                       ref <- newIORef (ps, rm)
                                                       xs <- forM fs
                                                               (\f -> do tup <- readIORef ref
                                                                         (bs, ps', rm') <- f tup
                                                                         writeIORef ref (ps', rm')
                                                                         return bs)
                                                       (ps', rm') <- readIORef ref
                                                       let ctorRep | shorter ctors 2 = []
                                                                   | otherwise = varbytes (constrIndex $ toConstr d)
                                                       return (ctorRep ++ concatWithLengths xs, ps', rm')
                                    _ -> error $ "A specialized serializer is required for type " 
                                                    ++ dataTypeName (dataTypeOf d)


data Unfolder r = Unfolder [Byte] r              
 
-- TODO
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> IO a
genericFromBytes s b = undefined -- return $ genericFromBytes' s b

genericFromBytes' :: Data a => SerializationSettings -> [Byte] -> a
genericFromBytes' set bs = result
 where result = case specializedSerializer set result of
                        Serializer _ from -> from bs
                        NoSerializer      -> case dataTypeRep $ dataTypeOf result of
                                              AlgRep ctors -> let (i,xs) | length ctors == 1 = (1,bs)
                                                                         | otherwise = varunbytes bs
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
                                   in Unfolder rest (f $ genericFromBytes' set curr)
       
       
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
gsTypeID x = '$' : dataTypeName (dataTypeOf x)

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
