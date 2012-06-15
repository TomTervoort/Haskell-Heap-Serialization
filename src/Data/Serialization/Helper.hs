{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Serialization.Helper where

import Data.Serialization.Internal.IntegralBytes
import Data.Serialization

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Data.Typeable
import Data.Ratio

class (Show a, Read a, Typeable a) => SerializableByShow a where
 showVersionID :: a -> VersionID
 showVersionID _ = VersionID 0

instance SerializableByShow a => Serializable a where
 serialVersionID = showVersionID
 toBytes   = concat . map (bytes . ord) . show
 fromBytes = read . stringify
  where stringify [] = ""
        stringify (a:b:c:d:xs) = chr (unbytes [a,b,c,d]) : stringify xs
  

-- The following instances use SerializableByShow for now, but should get more compact representations
instance SerializableByShow Integer
instance SerializableByShow Float
instance SerializableByShow Double
instance (Read a, Typeable a, Integral a) => SerializableByShow (Ratio a)

