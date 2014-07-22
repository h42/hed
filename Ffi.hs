{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi (
    getFileMode
    ,setFileMode
    ,h_readlink
    ,h_winsize
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
-- import Foreign.Ptr
--import Foreign.Marshal.Array

--
-- WIN_SIZE - Passes int ptr so we can return multiple ints
--
foreign import ccall "win_size"  c_winsize :: Ptr CInt -> IO Int
h_winsize :: IO (Int, Int, Int)
h_winsize = do
    ptr <- mallocArray 2  :: IO (Ptr CInt)
    rc  <- c_winsize ptr
    xp2 <- peekArray 2 ptr
    free ptr
    return (rc,fromIntegral $ head xp2,fromIntegral $ xp2 !! 1)

--
-- GETFILEMODE
--
foreign import ccall "getfilemode" c_getfilemode :: CString -> IO CInt
getFileMode :: String -> IO Int
getFileMode fn = do
    s <- newCString fn
    m <- c_getfilemode s
    free s --Foreign.Marshal.Alloc.free 
    return (fromIntegral m)

--
-- SETFILEMODE
--
foreign import ccall "setfilemode" c_setfilemode :: CString -> CInt -> IO CInt
setFileMode :: String -> Int -> IO Int
setFileMode fn m = do
    s <- newCString fn
    rc <- c_setfilemode s (fromIntegral m)
    free s --Foreign.Marshal.Alloc.free
    return  (fromIntegral rc)

--
-- READLINK
--
foreign import ccall "c_readlink" c_readlink 
    :: CString -> CString -> CInt -> IO CInt
h_readlink :: String -> IO (Int,String)
h_readlink fn = do
    cs <- newCString fn
    let size = 1000
    cs2 <- mallocBytes size :: IO CString
    rc <- c_readlink cs cs2 (fromIntegral size)
    let rc' = fromIntegral rc
    let ofn
	  | rc' <= 0 = peekCString cs
	  | otherwise = peekCString cs2
    ofn' <- ofn -- MUST COME BEFORE FREE
    free cs
    free cs2
    return (rc',ofn') -- rc>0 for symlink

{-main = do
    m <- getFileMode "temp"
    --rc <- setFileMode "temp" 0o644
    putStrLn $ show m -}
