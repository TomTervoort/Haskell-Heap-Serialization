{- | This module offers basic functionality for converting integers to lists of bytes and 
     vice-versa. -}
module Data.Serialization.Internal.IntegralBytes 
    (
      bytes,
      unbytes,
      varbytes,
      varunbytes
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


-- | Uses a variable length encoding instead of a constant-sized one. Usefull for encoding lengths 
-- | that generally don't take up four or eight whole bytes and of which the size is 
-- | platform-specific.
-- |
-- | Currently only works on positive integers.
varbytes :: (Bits a, Integral a) => a -> [Word8]
varbytes x | x < 0 = error "Varbytes only works on positive integers."
           | x < 0x80 = [fromIntegral x]
           | otherwise = (0x80 .|. fromIntegral (x .&. 0x7f)) : varbytes (x `shiftR` 7)
           

varunbytes :: (Bits a, Integral a) => [Word8] -> (a, [Word8])
varunbytes (x:xs) | x .&. 0x80 /= 0 = let (y,ys) = varunbytes xs
                                       in ((y `shiftL` 7) .|. fromIntegral (x .&. 0x7f), ys)
                  | otherwise = (fromIntegral x, xs)
                  

vartest :: (Bits a, Integral a) => a -> Bool
vartest x = let (y, ys) = varunbytes $ varbytes x
             in x == y && null ys
