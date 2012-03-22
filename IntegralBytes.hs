module IntegralBytes (
                      Byte,
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


type Byte = Word8

bytes :: (Bits a, Integral a, Bounded a) => a -> [Byte]
bytes x = bytes' (bitSize x `div` 8) x
 where bytes' 0 _ = []
       bytes' n x = let rotx = x `rotateL` 8 in (fromIntegral $ rotx .&. 0xff) : bytes' (n-1) (rotx .&. complement 0xff)

unbytes :: (Bits a, Integral a, Bounded a) => [Byte] -> a
unbytes = unbytes' 8
unbytes' _ [] = 0
unbytes' s (x:xs) = (fromIntegral x `rotateR` s) .|. unbytes' (s + 8) xs
