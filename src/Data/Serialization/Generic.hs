{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExistentialQuantification #-}

-------------------------------------
-- | 
-- [headers]
--
-- Defines generic serialization functions that require the type of objects that can serialized to 
-- be an instance of @Data@, rather than 'Serializable'. The binary representation of objects 
-- serialized this way will depend on the layout of their data structure.
--
-- If objects of certain types need to be serialized in a certain way because doing so based on 
-- their layout is either not possible or less efficient, it is possible to manually make them an
-- instance of 'Serializable' and notifying the generic serializer of this by adding that type as
-- a so-called specialization.
-- 
-- Whenever a data structure contains an object of a type for which a specialization exist, the 
-- serializer will use that for that object's binary representation; all objects inside the 
-- structure without such a specialization will be serialized using the generic method.
-------------------------------------
module Data.Serialization.Generic (
                                     -- | @Data.Serialization@ is exposed, with the exception of the
                                     -- functions serialize and deserialize. These have been given
                                     -- a different, generic, definition in this module.
                                     module Data.Serialization,
                                     
                                     -- * Generic serialization
                                     serializeWith,
                                     serialize,
                                     deserializeWith,
                                     deserialize,
                                     
                                     -- * Settings
                                     SerializationSettings,
                                     defaultSettings,
                                     
                                     -- * Adding specializations
                                     addSerializableSpecialization,
                                     addSerSpec
                                  ) where

import Data.Serialization hiding (serialize, deserialize)
import Data.Serialization.Internal
import Data.Serialization.Internal.Settings
import Data.Serialization.Internal.IntegralBytes 
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

-- 'shorter a b' is equivalent to 'length a < length b'.
shorter :: [a] -> Int -> Bool
shorter _ 0  = False
shorter [] _ = True
shorter (_:xs) n = shorter xs (n - 1)

concatWithLengths :: [[Byte]] -> [Byte]
concatWithLengths [] = []
concatWithLengths (x:xs) = varbytes (length x) ++ x ++ concatWithLengths xs

-------------------------------------------

-- A 'reference map' from PtrKeys to their binary representation.
type RefMap = Map PtrKey [Byte]

-- The method used by the generic serializer to convert an arbitrary data structure to its binary
-- representation.
genericToBytes :: (Data a) => SerializationSettings -> a -> IO [Byte]
genericToBytes s d = do ps <- newPtrSet
                        -- Build a 'reference map' from 'PtrKeys' to substructure serializations.
                        (key, rm) <- genericToBytes' s d (ps, M.empty)
                        -- Serialize the RefMap and prepend it 
                        return $ varbytes key ++ serializeRefMap rm

 where -- Convert a RefMap to a byte sequence.
       serializeRefMap :: RefMap -> [Byte]
       serializeRefMap = concatWithLengths . map (\(k,bs) -> varbytes k ++ bs) . M.toList

-- Convert a structure to a RefMap. Shared references will be serialized once. Returns the key to the 'entry point' in
-- the RefMap.
genericToBytes' :: (Data a) => SerializationSettings -> a -> (PtrSet, RefMap) -> IO (PtrKey, RefMap)
genericToBytes' set d (ps, rm) = do -- Check whether the object already resides in the PtrSet.
                                    memb <- ptrSetMember d ps
                                    case memb of
                                     -- If so, return its PtrKey.
                                     Just k  -> return (k, rm)
                                     -- Otherwise, serialize it and add it to the PtrSet.
                                     Nothing -> do (bs, rm') <- use (specializedSerializer set d)
                                                   k <- ptrSetAdd d ps
                                                   let rm'' = M.insert k bs rm'
                                                   return (k, rm'')
 
 -- If a serializer for a structure of this type exists, use its toBytes function to encode it. Otherwise use the 
 -- generic serializer for algebraic datatypes.
 where use s = case s of
                Serializer to _ -> return (to d, rm)
                NoSerializer    -> case dataTypeRep $ dataTypeOf d of
                                    AlgRep ctors -> do -- Recursively apply serializer to create functions of type
                                                       -- RefMap -> IO (PtrKey, RefMap).
                                                       let fs = gmapQ (\d rm -> genericToBytes' set d (ps, rm)) d
                                                       -- Maintain an IORef containing the RefMap.
                                                       ref <- newIORef rm
                                                       -- Sequentially execute the serializers of the children on the 
                                                       -- RefMap.
                                                       xs <- forM fs
                                                               (\f -> do (k, rm') <- readIORef ref >>= f
                                                                         writeIORef ref rm'
                                                                         return $ varbytes k)
                                                       rm' <- readIORef ref
                                                       -- Prepend the index of the data constructor, but only if there
                                                       -- is more than one.
                                                       let ctorRep | shorter ctors 2 = []
                                                                   | otherwise = varbytes (constrIndex $ toConstr d)
                                                       return (ctorRep ++ concat xs, rm')
                                    _ -> -- This is not an algebraic datatype, nor has it got a specialized serializer.
                                         error $ "A specialized serializer is required for type " 
                                                    ++ dataTypeName (dataTypeOf d)


-- Helper object for using gunfold to consume bytes and convert them in a DeRefMap.
data Unfolder r = Unfolder ([Byte], DeRefMap) r              
 
-- Left objects will yet have to be deserialized, while Right ones already are.
type DeRefMap = Map PtrKey (Either [Byte] Dynamic)
 
-- Inverse of genericToBytes.
genericFromBytes :: (Data a) => SerializationSettings -> [Byte] -> IO a
genericFromBytes s b = do -- Obtain the root PtrKey.
                          let (k, b') = varunbytes b
                          -- Obtain the DeRefMap (with values still being [Byte]'s).
                          let rm = deRefMap b'
                          -- Lookup the root key.
                          let (Just (Left b'')) = M.lookup k rm
                          -- Apply the recursive deserializer to the assoiated byte sequence.
                          return . fst $ genericFromBytes' s b'' rm
 where -- Decodes a RefMap as a DeRefMap.
       deRefMap :: [Byte] -> DeRefMap
       deRefMap [] = M.empty
       deRefMap xs = let (len, xs1) = varunbytes xs
                         (bs', xs2) = splitAt len xs1
                         (key, bs)  = varunbytes bs'
                      in M.insert key (Left bs) $ deRefMap xs2


-- Recursive generic deserializer. Also returns an updated DeRefMap.
genericFromBytes' :: Data a => SerializationSettings -> [Byte] -> DeRefMap -> (a, DeRefMap)
genericFromBytes' set bs drm = result
 where result = -- Check whether there exists a specialized serializer for the desired type.
                case specializedSerializer set $ fst result of
                 -- If so, use it.
                 Serializer _ from -> (from bs, drm)
                 -- Otherwise use the generic deserializer.
                 NoSerializer      -> case dataTypeRep dtype of
                                       AlgRep ctors -> 
                                        -- Determine to Constr to use.
                                        let (i,xs) | length ctors == 1 = (1,bs)
                                                   | otherwise = varunbytes bs
                                            ctor = ctors !! (i - 1)
                                            -- Apply the generic infold.
                                            (Unfolder (left, dm) x) = gunfold unfolder (Unfolder (xs,drm)) ctor
                                         in if null left
                                             then (x, dm)
                                             else error $ "Not all bytes are consumed when deserializing as "
                                                            ++ dataTypeName dtype ++ "."

                                       -- Object must be an algebraic datatype.
                                       _                 -> error $ "A specialized serializer for " ++ dataTypeName dtype
                                                                      ++ " is required."
       dtype = dataTypeOf $ fst result
       
       -- Helper for gunfold that consumes a [Byte], updates a DeRefMap and constructs a data structure.
       unfolder :: Data b => Unfolder (b -> r) -> Unfolder r
       unfolder (Unfolder (bs, dm) f) = let (key, rest) = varunbytes bs
                                            x = lookupData dm key
                                         in Unfolder (rest, snd x) $ f (fst x)
                                         
       -- Lookups up an object from the DeRefMap. If it has not yet been deserialized, that will be done through
       -- genericFromBytes'. An updated DeRefMap is also returned.
       lookupData :: Data a => DeRefMap -> PtrKey -> (a, DeRefMap)
       lookupData dm key = case M.lookup key dm of
                            Just (Right d) -> (fromDyn d undefined, dm)
                            Just (Left bs) -> let (x,m) = genericFromBytes' set bs dm
                                               in (x, M.insert key (Right $ toDyn x) dm)
                            _ -> error "Corrupt data: key not in reference map."
                            
-------------------------------------------
       
genericTest :: (Data a) => a -> IO a
genericTest x = genericToBytes defaultSettings x >>= genericFromBytes defaultSettings

-- Compute a checksum over the layout and contents of a data structure, so version inconsisancies 
-- can be detected.
genericVersionID :: (Data a) => SerializationSettings -> a -> VersionID
genericVersionID set x = combineVIDs $ [settingsVID set, structureID S.empty $ dataTypeOf x] 
-- Take checksum over characters in data type name and constructor kinds.
 where structureID :: Data a => Set String -> a -> VersionID
       structureID set x | S.member dname set  = VersionID 0
                         | otherwise = let newset = S.insert dname set
                                        in VersionID . checksumInt
                                            $  concatMap (bytes . ord) dname
                                            ++ 0 : case dataTypeRep $ dataTypeOf x of
                                                    AlgRep ctors -> trace (show ctors) $ 0 : concatMap (ctorID x newset) ctors
                                                    IntRep       -> [1]
                                                    FloatRep     -> [2]
                                                    CharRep      -> [3]
                                                    NoRep        -> [4]
        where dname = dataTypeName $ dataTypeOf x
        
       ctorID :: Data a => a -> Set String -> Constr -> [Byte]
       ctorID x set c = 
            concatMap (bytes . ord) (concat $ intersperse "\0" $ showConstr c : constrFields c)
             ++ [0, if constrFixity c == Prefix then 1 else 2] 
             ++ concatMap (\(VersionID x) -> bytes x) (gmapQ (structureID set . dataTypeOf) 
                                                                    $ assertType x $ fromConstr c)

-----------------------

gsTypeID :: Data a => a -> TypeID
gsTypeID x = '$' : typeID x

-- | Apply the generic serializer to some value, using certain settings. Functions from 
-- 'Data.Serialization' (such as 'store' and 'load') can be used to handle the resulting
-- 'Serialized' object.
-- 
-- This function exists in the IO-monad because that is required for detection of sharing (multiple 
-- pointers referring to the same object).
serializeWith :: Data a => SerializationSettings -> a -> IO Serialized
serializeWith set x = do packet <- genericToBytes set x
                         return Serialized {
                                  -- The type name is prepended with a $ in order to indicate usage
                                  -- of the generic serializer.
                                  dataType = gsTypeID x,
                                  serializerVersion = genericVersionID set x, 
                                  dataPacket = B.pack $ packet
                                 }

-- | Serialize with default settings. See 'serializeWith' and 'defaultSettings'.
serialize :: Data a => a -> IO Serialized
serialize = serializeWith defaultSettings

-- | Decodes a @Serialized@ object back into a Haskell structure, using certain settings. In order 
-- to assert the type is correct, @Nothing@ is returned when the inferred type is not correct.
--
-- The same 'SerializationSettings' should be used as for the serialization of the object in 
-- question.
--
-- When a version incompatibility or other error occurs, a @SerializationException@ is thrown.
deserializeWith :: Data a => SerializationSettings -> Serialized -> IO (Maybe a)
deserializeWith set (Serialized tid sv dp) = do result <- genericFromBytes set $ B.unpack dp
                                                if tid /= gsTypeID result 
                                                 then return Nothing
                                                 else
                                                  if sv /= genericVersionID set result
                                                   then error "Version of serializer used for this object does not match the current one."
                                                   else return $ Just result

-- | Deserialize with default settings. See 'deserializeWith' and 'defaultSettings'.
deserialize :: Data a => Serialized -> IO (Maybe a)
deserialize = deserializeWith defaultSettings
