{-# LANGUAGE CPP, ForeignFunctionInterface #-}
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
import System.Info
import Foreign.C.Types (CULLong)

import qualified Data.ByteString.Lazy as BSL

type LazyByteString = BSL.ByteString

#ifndef __GLASGOW_HASKELL__
#error Currently only GHC is supported.
#endif

-- TODO: Use (MD5) hash instead?
checksum :: (Integral a, Integral b) => [a] -> b
checksum = foldl' (+) 0 . map fromIntegral

cachedPid :: IORef (Maybe Word64)
{-# NOINLINE cachedPid #-}
cachedPid = unsafePerformIO $ newIORef Nothing

programBinaryChecksum :: IO Word64
#ifdef linux_HOST_OS
programBinaryChecksum = do exe <- BSL.readFile "/proc/self/exe"
                           return $ checksum $ BSL.unpack exe
#elif defined(mingw_HOST_OS)
foreign import ccall "winprogcsum.h programBinaryChecksum" windowsProgramChecksum :: IO CULLong
programBinaryChecksum = do (CULLong csum) <- windowsProgramChecksum
                           when csum == 0 $ error "Calculating the program binary checksum failed."
                           return csum
#endif

-- TODO: Portable version
programVersionID :: IO Word64
--programVersionID = return 424242424242424242
programVersionID = do cached <- readIORef cachedPid
                      case cached of 
                       Nothing  -> do csum <- programBinaryChecksum
                                      writeIORef cachedPid $ Just csum
                                      return csum
                       (Just x) -> return x
