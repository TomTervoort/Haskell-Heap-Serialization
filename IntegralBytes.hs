{- | This module offers basic functionality for converting fixed-size integers to lists of bytes 
     and vice-versa. -}
module IntegralBytes (
                      bytes,
                      unbytes
                     ) where

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Bits
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- | Converts an integral to a list of bytes using a big endian byte order.
bytes :: (Bits a, Integral a, Bounded a) => a -> [Word8]
bytes x = bytes' (bitSize x `div` 8) x
 where bytes' 0 _ = []
       bytes' n x = let rotx = x `rotateL` 8 in (fromIntegral $ rotx .&. 0xff) : bytes' (n-1) (rotx .&. complement 0xff)

-- | Converts a list of bytes in big endian byte order to an integral that should be large enough to contain them.
unbytes :: (Bits a, Integral a, Bounded a) => [Word8] -> a
unbytes = unbytes' 8
unbytes' _ [] = 0
unbytes' s (x:xs) = (fromIntegral x `rotateR` s) .|. unbytes' (s + 8) xs
