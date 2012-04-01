{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{- | A module that can be used to uniquely identify the build of the executable it is included in.
     It does this by creating a checksum of the program binary.
     
     The module currently only works with GHC under Linux or Windows.
-} 
module ProgramVersionID (checksum, programVersionID) where 

import Data.List
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char

import Data.Bits
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

-- | Computes a checksum using Fletcher's algorithm.
checksum :: [Word8] -> Word64
checksum = fletcher 0 0 . map fromIntegral
 where fletcher :: Word32 -> Word32 -> [Word32] -> Word64
       fletcher s1 s2 []     = fromIntegral s1 `shiftL` 32 .|. fromIntegral s2
       fletcher s1 s2 (x:xs) = let a = s1 + x
                                   b = s2 + a
                                in s1 `seq` s2 `seq` fletcher a b xs

-- Used to memoize the program binary checksum.
cachedPid :: IORef (Maybe Word64)
{-# NOINLINE cachedPid #-}
cachedPid = unsafePerformIO $ newIORef Nothing

-- Determines the path of the current executable. TODO: support more platforms.
executablePath :: IO String
#ifdef linux_HOST_OS
executablePath = return "/proc/self/exe"
#elif defined(mingw32_HOST_OS) | defined(mingw64_HOST_OS)
--TODO: test
foreign import ccall "windows.h GetModuleFileName" windows_moduleFilename :: Ptr a -> (Ptr CChar) -> CUInt -> IO CUInt
executablePath = allocaArray 1000 
                  $ \buffer -> do len <- windows_moduleFilename nullPtr buffer 1000
                                  when (len == 1000) $ error "Error while trying to retrieve the executable filename."
                                  peekArray (fromIntegral len) buffer >>= return . map castCCharToChar
#else
#error OS not supported.
#endif

-- | Calculates a 64-bit integer extremely likely to uniquely identify this build of the binary
-- | that utilizes this module. The result is cached so only the first call is somewhat expensive.
programVersionID :: IO Word64
programVersionID = do cached <- readIORef cachedPid
                      case cached of
                       Nothing  -> do exe <- executablePath >>= BSL.readFile
                                      let csum = checksum $ BSL.unpack exe
                                      writeIORef cachedPid $ Just csum
                                      return csum
                       (Just x) -> return x
