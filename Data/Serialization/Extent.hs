-------------------------------------
-- | 
-- [headers]
--
-- This module contains the definitions of the class methods of 'Data.Serialization.Serializable' 
-- and therefore it can be used when you want to make a custom serializer for objects of a certain
-- type.
-------------------------------------

module Data.Serialization.Extent 
    (
          module Data.Serialization,
          Byte,
          VersionID (..),
          Serializable (..),
          sid
     ) where

import Data.Serialization
import Data.Serialization.Internal


