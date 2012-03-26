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

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

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

executablePath :: IO String
#ifdef linux_HOST_OS
executablePath = return "/proc/self/exe"
#elif defined(mingw_HOST_OS)
foreign import ccall "windows.h GetModuleFileName" windows_moduleFilename :: Ptr a -> (Ptr CChar) -> CUInt -> IO CUInt
executablePath = allocaArray 1000 
                  $ \buffer -> do len <- windows_moduleFilename nullPtr buffer 1000
                                  when (len == 1000) $ error "Error while trying to retrieve the executable filename."
                                  peekArray (fromIntegral len) buffer >>= return . map castCCharToChar
#endif

-- TODO: Portable version
programVersionID :: IO Word64
--programVersionID = return 424242424242424242
programVersionID = do cached <- readIORef cachedPid
                      case cached of 
                       Nothing  -> do exe <- executablePath >>= BSL.readFile
                                      let csum = checksum $ BSL.unpack exe
                                      writeIORef cachedPid $ Just csum
                                      return csum
                       (Just x) -> return x
