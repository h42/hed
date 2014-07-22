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

import Display
import Ffi
import Func1
import Func0
import Getfn
import Global
import HTerm

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import System.Directory (getCurrentDirectory)
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Posix.User
import System.Process

---------------------------------------------------------
-- New - not really a file but no better place for it
---------------------------------------------------------
newf :: Global -> IO Global
newf g = chk_winsize initGlobal
    {zmsg="",zlist=[B.empty],zlines=1,zhistory=zhistory g,zkplist=zkplist g}

---------------------------------------------------------
-- Swap
---------------------------------------------------------
swapf :: Int -> Global -> IO Global
swapf n g = do
    let fn = fnHistory n g
    if fn /= "" then loadfn fn g -- loadfn will checkupd
    else return g{zmsg="No swap file "++ (show n)}

---------------------------------------------------------
-- Load
---------------------------------------------------------
access :: String -> IO Int
access fn = do
    b'' <- fileExist fn
    if not b'' then return (-1)
    else do
	b' <- fileAccess fn True True False
        if b' then return 3
	else do
	    b <- fileAccess fn True False False
            if b  then return 1 else return 0

getsuffix :: String -> String -> String
getsuffix [] y = y
getsuffix ('.':xs) _ = getsuffix xs xs
getsuffix (_:xs) y = getsuffix xs y

loadf :: Global -> IO Global
loadf g = do
    fn  <- hed_request "Enter file name: " g
    --let fn="temp"
    if null fn
	then do
	    fn' <- getfn (zmaxy g) [getsuffix (zfn g) []]
	    if null fn'
		then return g{zmsg="Load cancelled",zpager=True}
		else loadfn fn' g
	else loadfn fn g

loadfn :: String -> Global -> IO Global
loadfn fn g = do
    (_,fn') <- h_readlink fn -- rc>0 for symlink ; we do nothing with this info
    rc <- E.try (loadfn2 fn' g) :: IO (Either E.SomeException Global)
    case rc of
	Left e -> return g{zmsg=show e}
	Right g' -> return g'

loadfn2 :: String -> Global -> IO Global
loadfn2 fn g = do
    g1 <- addHistory g >>= checkupd
    acc <- access fn
    let hs = if (acc) <= 0  then rmHistory fn (zhistory g1)
	     else zhistory g1
    if (acc) > 0  then load2 fn g1{zro=if acc==1 then True else False}
    else if null $ zfn g1 then do
	g2 <- newf g1
	return g2{zpager=True,zmsg="Unable to access file "++fn,zfn=fn}
    else return g1{zmsg="Unable to access file "++fn,zhistory=hs}

-- readFile function holds open lock on fn preventing updates even
-- when done reading so I use hGetContents. -  Learn how to close???
load2 :: FilePath -> Global -> IO Global
load2 fn g = E.bracket (openFile fn ReadMode) hClose $ \h -> do
    bigrec <- B.hGetContents h
    let msg = if zro g then fn ++ " READ ONLY" else fn
    let ro = B.any (\c->badchar c) bigrec
        recs = if not ro then B.lines bigrec
               else B.lines (B.map (\c->if badchar c then '~' else c) bigrec)
    p <- getFileMode fn
    chk_winsize initGlobal{zfn=fn,zmsg=msg,zlist=recs,
		zlines=length recs,
                zhistory=zhistory g,zpager=True,
		zfind=zfind g, zchange=zchange g,
                zkplist=zkplist g, zstmode=p, zro=if ro then True else zro g}
	>>= fromHistory  >>= addHistory >>= chkBottom >>= chktype >>= gline

badchar :: Char -> Bool
badchar c
    | (c>=' ' && ord c < 128) || c=='\n' || c=='\r' || c=='\t'  = False
    | otherwise = True

chktype :: Global -> IO Global
chktype g
    | any ((zfn g)==) ["makefile","Makefile"] = return g{ztabcompress=True}
    | otherwise = return g{ztabcompress=False}

chkBottom :: Global -> IO Global
chkBottom g = if zy g < zlines g then return g
	      else bottom g

---------------------------------------------------------
-- Checkupd
---------------------------------------------------------
checkupd :: Global -> IO Global
checkupd g
    | zupd2 g == 0 = return g
    | otherwise = do
	s <- hed_requestx "Do you want to save current file (y/n)? " 1 g
	case s of
	    "y" -> savef g
	    "n" -> return g
            _   -> checkupd g

---------------------------------------------------------
-- Save
---------------------------------------------------------
savef :: Global -> IO Global
savef g
    | zupd2 g == 0 = return g
    | zro g = return g{zmsg="FILE IS READ ONLY"}
    | zfn g == "" = do
	s <- hed_request "Enter file name to save: " g
	if s=="" then  return g{zmsg="File not saved"}
		 else savef' g{zfn=s}
    | otherwise = catchIOError
	(saveorig g >>= savef')
	(\e->return g{zmsg=show e}) -- (e::E.IOException)})

saveorig :: Global -> IO Global
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

savef' :: Global -> IO Global
savef' g = do
    g' <- pline g
    B.writeFile (zfn g') (B.unlines (zlist g'))
    if (zstmode g') /= 0
	then Ffi.setFileMode (zfn g') (zstmode g') 
	else return 0
    return g{zmsg="file saved",zupd2=0}

-------------------------------------
-- HOMEFILE
-------------------------------------
homeFile :: [Char] -> IO [Char]
homeFile fn = do
    home <- catchIOError (
        fmap homeDirectory $ getEffectiveUserID >>= getUserEntryForID )
        (\_e -> return "")
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

getHistoryFn :: IO [Char]
getHistoryFn = do
    cdir <- getCurrentDirectory
    let fn = map (\x -> if x == '/' then '_' else x) cdir
    homeFile fn

addHistory :: Global -> IO Global
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
        (h:_) -> return g{zx=hx h,zy=hy h,zoff=hoff h,ztop=htop h}

rmHistory :: String -> [History] -> [History]
rmHistory _ [] = []
rmHistory fn hs = filter (\h -> (hfn h) /= fn) hs

fnHistory :: Int -> Global -> String
fnHistory n g = s where
    hs = fnsHistory g
    s = if length hs > n then hs !! n else ""

fnsHistory :: Global -> [String]
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
    (\_ -> return g{zhistory=[]})

writeHistory :: Global -> IO Global
writeHistory g = do
    fn <- getHistoryFn
    catchIOError
	(writeFile fn (show (zhistory g)) >> return g)
        (\_ -> return g)

--
-- GETHISTORY - Alt R Command
--
getHistory :: Global -> IO Global
getHistory g = do
    let fns = map (\h -> (hfn h)) (take 20 (zhistory g))
    if fns == [] then return g  else geth fns g

geth :: [[Char]] -> Global -> IO Global
geth fns g = do
    let sx = zipWith (\x y -> (show x) ++ ". " ++ y) ([1..] :: [Int]) (take 10 fns)
    clrscr g
    putStrLn ""
    mapM_ putStrLn sx
    putStr "\nSelect file: "
    hFlush stdout
    kc <- getkb
    hFlush stdout
    geth2 kc g

geth2 :: KeyCode -> Global -> IO Global
geth2 (KeyChar k) g = if k>='0' && k<='9'
	then swapf (ord k - ord '0' - 1) g{zpager=True}
	else return g{zmsg="got char",zpager=True}
geth2 _ g = return g{zpager=True}

