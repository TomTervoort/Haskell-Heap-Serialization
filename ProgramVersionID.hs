module ProgramVersionID (programVersionID) where 

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Word
import Data.IORef
import System.IO.Unsafe

import qualified Data.ByteString.Lazy as BSL

type LazyByteString = BSL.ByteString

checksum :: (Integral a, Integral b) => [a] -> b
checksum = foldl' (+) 0 . map fromIntegral

cachedPid :: IORef (Maybe Word64)
{-# NOINLINE cachedPid #-}
cachedPid = unsafePerformIO $ newIORef Nothing

-- TODO: Portable version
programVersionID :: IO Word64
--programVersionID = return 424242424242424242
programVersionID = do cached <- readIORef cachedPid
                      case cached of 
                       Nothing  -> do exe <- BSL.readFile "/proc/self/exe"
                                      let csum = checksum $ BSL.unpack exe
                                      writeIORef cachedPid $ Just csum
                                      return csum
                       (Just x) -> return x
