module Data.Serialization.Extent 
    (
          module Data.Serialization,
          Byte,
          VersionID (..),
          Serializable (..),
          
          -- * Versioning helpers
          sid,
          combineVIDs
     ) where

import Data.Serialization
import Data.Serialization.Internal


