module Getfn (
getfn
) where

import Data.Char
import Data.List
import System.IO
import System.Directory (doesDirectoryExist, doesFileExist,
			 getCurrentDirectory, getDirectoryContents)
import Hterm
import GetKB

--main = do
--    f <- getfn [".hs","makefile"]
--    putStrLn f

getfn rows glob = do
    fns <- getfns glob
    let fns' = fns
    selectfn (rows-2) 0 fns'

getfns glob = do
    fns <- dirList "" ""
    return $ sortBy (\x y -> compare (map toLower x) (map toLower y)) $
	filter (\x -> if glob == [[]] then True else checkfn glob x) fns

checkfn glob fn = any (\x -> drop (length fn - length x) fn == x) glob

selectfn rows y fns = do
    let l = length fns
    tclrscr
    let fl = take rows (drop y fns)
    let fl' = zipWith (\x f ->(show x) ++ ". " ++ f) [(y+1)..] fl
    mapM putStrLn fl'
    putStr "\n==> "
    hFlush stdout
    s <- getcmd ""
    if s == "" || (not $ all isDigit s)
	then do
	    case s of
		"n" -> selectfn rows (min (y+rows) (l-1)) fns
		"p" -> selectfn rows (max (y-rows) 0) fns
		"q" -> return ""
		"" -> return ""
		"." -> getfn rows [".c",".cc",".hs",".py","makefile"]
		"*" -> getfn rows [[]]
		_ -> selectfn rows y fns
	else do
	    let n =  read s - 1 :: Int
	    return $ if n>=l then "" else (fns !! n)

getcmd :: String -> IO String
getcmd s = do
    kc <- getkb
    case kc of
	KeyChar 'q' -> return ['q']
	KeyFunc 12  -> return ['q']
	KeyChar 'n' -> return ['n']
	KeyChar ' ' -> return ['n']
	KeyPgdown   -> return ['n']
	KeyPgup     -> return ['p']
	KeyChar 'p' -> return ['p']
	KeyChar '*' -> return ['*']
	KeyChar '.' -> return ['.']
	KeyCntl 'j' -> return s
	KeyChar n -> do
	    if isDigit n  then putChar n >> hFlush stdout >> getcmd (s++[n])
	    else getcmd s
	KeyBs -> do
	    if s /= ""  then putStr "\x08 \x08" >> hFlush stdout >> getcmd (init s)
	    else  getcmd s
	_ -> return ""

-------------------------------
-- DIR.HS
-------------------------------
catdir d1 d2
    | d1 == "" || d1 == "./"   =  d2
    | last d1 == '/'           =  d1 ++ d2
    | otherwise                =  d1 ++ "/" ++ d2

getconts d = do
    fs <- getDirectoryContents (d)
    return $ filter (`notElem` [".",".."]) fs

dirList ::  FilePath -> String -> IO [FilePath]
dirList dir' re = do
    let dir  =  if dir' == ""  then "./"  else dir'
    gotdir <- doesDirectoryExist dir
    if not gotdir then return []
    else do          
	fs <-getconts dir
	locdir dir fs

locdir :: FilePath -> [FilePath] -> IO [FilePath]
locdir d [] = return []
locdir d (f:fs)= do
    gotfile <- (doesFileExist $ catdir d f)
    fnl <- locdir d fs
    return $ if gotfile then (catdir d f) : fnl  else fnl

