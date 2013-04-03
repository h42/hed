module File (
    newf
    ,swapf
    ,loadf
    ,loadfn
    ,savef
    ,checkupd
    ,homeFile
    ,getHistory
    ,addHistory
    ,readHistory
    ,writeHistory
    ,fromHistory
    ,fnHistory
    ,getHistoryFn
) where

import Global
import GetKB
import Func1
import Data.Char
import Data.Maybe
import Func0
import Display
import Getfn
import Data.List
import System.Posix.Files
import System.IO
import System.IO.Error
import System.Directory (doesDirectoryExist, doesFileExist,
			 getCurrentDirectory, getDirectoryContents)
import Ffi
import System.Environment
import System.Cmd
import Control.Monad
import qualified Control.Exception as E

import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Control.Exception as E

---------------------------------------------------------
-- New - not really a file but no better place for it
---------------------------------------------------------
newf :: Global -> IO Global
newf g = chk_winsize initGlobal
    {zmsg="",zlist=emptyZlist,zlines=1,zhistory=zhistory g,zkplist=zkplist g}

---------------------------------------------------------
-- Swap
---------------------------------------------------------
swapf n g = do
    let fn = fnHistory n g
    if fn /= "" then loadfn fn g -- loadfn will checkupd
    else return g{zmsg="No swap file "++ (show n)}

---------------------------------------------------------
-- Load
---------------------------------------------------------
access :: String -> Global -> IO Global
access fn g = do
    b'' <- fileExist fn
    if not b'' then return g{zaccess= -1}
    else do
	b' <- fileAccess fn True True False
	if b' then return g{zaccess=3}
	else do
	    b <- fileAccess fn True False False
	    if b  then return g{zaccess=1}  else return g{zaccess=0}

getsuffix :: String -> String -> String
getsuffix [] y = y
getsuffix ('.':xs) y = getsuffix xs xs
getsuffix (x:xs) y = getsuffix xs y

loadf :: Global -> IO Global
loadf g = do
    fn  <- hed_request "Enter file name: " g
    --let fn="temp"
    if null fn
	then do
	    --fn' <- getfn [".hs","makefile"]
	    fn' <- getfn (zmaxy g) [getsuffix (zfn g) []]
	    if null fn'
		then return g{zmsg="Load cancelled",zpager=True}
		else loadfn fn' g
	else loadfn fn g

loadfn fn g = do
    (rc2,fn') <- h_readlink fn -- rc>0 for symlink ; we do nothing with this info
    rc <- E.try (loadfn2 fn' g) :: IO (Either E.SomeException Global)
    case rc of
	Left e -> return g{zmsg=show e}
	Right g' -> return g'

loadfn2 fn g = do
    g1 <- (addHistory g >>= checkupd  >>= access fn)
    let hs = if (zaccess g1) <= 0  then rmHistory fn (zhistory g1)
	     else zhistory g1
    if (zaccess g1) > 0  then load2 fn g1
    else if null $ zfn g1 then do
	g2 <- newf g1
	return g2{zpager=True,zmsg="Unable to access file "++fn,zfn=fn}
    else return g1{zmsg="Unable to access file "++fn,zhistory=hs}

-- readFile function holds open lock on fn preventing updates even
-- when done reading so I use hGetContents. -  Learn how to close???
load2 fn g = E.bracket (openFile fn ReadMode) hClose $ \h -> do
    let msg = if zaccess g == 1 then fn ++ " READ ONLY" else fn
    recs <- (fmap (T.lines) $ T.hGetContents h)
    p <- getFileMode fn
    chk_winsize initGlobal{zfn=fn,zmsg=msg,zlist=toZlist recs,
		zlines=length recs,
		zaccess=zaccess g,zhistory=zhistory g,zpager=True,
		zfind=zfind g, zchange=zchange g,
		zkplist=zkplist g,zstmode=p}
	>>= fromHistory  >>= addHistory >>= chkBottom >>= chktype >>= gline

chktype :: Global -> IO Global
chktype g
    | isSuffixOf ".py" (zfn g) = return g{ztabcompress=False}
    | otherwise = return g{ztabcompress=True}

chkBottom g = if zy g < zlines g then return g
	      else bottom g

---------------------------------------------------------
-- Checkupd
---------------------------------------------------------
checkupd g
    | zupd2 g == 0 = return g
    | otherwise = do
	s <- hed_requestx "Do you want to save current file (y/n)? " 1 g
	case s of
	    "y" -> savef g
	    "n" -> return g
	    _ -> checkupd g

---------------------------------------------------------
-- Save
---------------------------------------------------------
savef :: Global -> IO Global
savef g
    | zupd2 g == 0 = return g
    | zaccess g == 1 = return g{zmsg="FILE IS READ ONLY"}
    | zfn g == "" = do
	s <- hed_request "Enter file name to save: " g
	if s=="" then  return g{zmsg="File not saved"}
		 else savef' g{zfn=s}
    | otherwise = catchIOError
	(saveorig g >>= savef')
	(\e->return g{zmsg=show e}) -- (e::E.IOException)})


saveorig g
    | zfnsaved g == 1 = return g
    | otherwise = do
	homefn <- homeFile "hed.bak"
	case homefn of
	    "" -> return  g
	    _  -> do
		let cmd2 = "mv " ++ zfn g ++ " " ++ homefn ++ " 2>/dev/null"
		system cmd2
		return g{zfnsaved=1}

savef' g = do
    g' <- pline g
    T.writeFile (zfn g') (T.unlines $ fromZlist (zlist g'))
    if (zstmode g') /= 0
	then Ffi.setFileMode (zfn g') (zstmode g') 
	else return 0
    return g{zmsg="file saved",zupd2=0}

-------------------------------------
-- HOMEFILE
-------------------------------------
homeFile fn = do
    home <- catchIOError (getEnv "HOME")
			 (\e -> return "")
    case home of
	"" -> return ""
	_  -> do
	    system $ "mkdir -p " ++ home ++ "/.hed"
	    return ( home ++ "/.hed/" ++ fn)

----------------------------
-- HISTORY
----------------------------
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

getHistoryFn = do
    cdir <- getCurrentDirectory
    let fn = map (\x -> if x == '/' then '_' else x) cdir
    homeFile fn

addHistory g = do
    let h = mkHistory g
    let hs = if (zfn g) /= ""  then h:rmHistory (zfn g) (zhistory g)
	     else zhistory g
    return g{zhistory=hs}

mkHistory :: Global -> History
mkHistory g =  History{hx=zx g,hy=zy g,hoff=zoff g,htop=ztop g,hfn=zfn g}

fromHistory :: Global -> IO Global
fromHistory g = do
    let h' = filter (\h -> (hfn h) == zfn g) (zhistory g)
    case h' of
	[] -> return g
	(h:hs) -> return g{zx=hx h,zy=hy h,zoff=hoff h,ztop=htop h}

rmHistory :: String -> [History] -> [History]
rmHistory fn [] = []
rmHistory fn hs = filter (\h -> (hfn h) /= fn) hs

fnHistory :: Int -> Global -> String
fnHistory n g = s where
    hs = fnsHistory g
    s = if length hs > n then hs !! n else ""

fnsHistory g = map (\h -> hfn h) (zhistory g)

--
-- Input / Output History
--
readHistory :: Global -> IO Global
readHistory g = catchIOError
    (do
	fn <- getHistoryFn
	h <- openFile fn  ReadMode
	s <- hGetContents h
	seq s (hClose h)
	let hist = maybeRead s 
	    hs = case hist of
		    Just hist' -> hist'
		    Nothing  -> []
	return g{zhistory=hs}
    )
    (\e -> return g{zhistory=[]})

writeHistory :: Global -> IO Global
writeHistory g = do
    fn <- getHistoryFn
    catchIOError
	(writeFile fn (show (zhistory g)) >> return g)
	(\e -> return g)

--
-- GETHISTORY - Alt R Command
--
getHistory :: Global -> IO Global
getHistory g = do
    let fns = map (\h -> (hfn h)) (take 20 (zhistory g))
    if fns == [] then return g  else geth fns g

geth fns g = do
    let l = length fns
	sx = zipWith (\x y -> (show x) ++ ". " ++ y) [1..] (take 10 fns)
    clrscr g
    --print $ zhistory g
    putStrLn ""
    mapM_ putStrLn sx
    putStr "\nSelect file: "
    hFlush stdout
    kc <- getkb
    hFlush stdout
    geth2 kc g

geth2 (KeyChar k) g = if k>='0' && k<='9'
	then swapf (ord k - ord '0' - 1) g{zpager=True}
	else return g{zmsg="got char",zpager=True}
geth2 _ g = return g{zpager=True}

