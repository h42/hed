module Glob (
    Glob 
    ,Globs
    ,compGlob
    ,matchGlob
    ,matchGlobx
    ,searchGlob
--    ,dglob
    ,Sglob
    ,putGlob
    ,getGlob
    ,fnGlob
) where

import Data.Char
import Data.List
import System.IO.Unsafe
import Data.IORef
import System.Directory (doesDirectoryExist, doesFileExist,
			 getCurrentDirectory, getDirectoryContents)
import qualified Data.ByteString.Char8 as B

data Glob = GlobC Char
	    | GlobSet B.ByteString
	    | GlobNot B.ByteString
	    | WildDot
	    | WildStar
	    | GlobErr
	    | GlobBrace [[Glob]]
	deriving (Show,Eq)

type Globs = [Glob]

data Sglob = Sglob {
    globlist :: Globs
}
    
--
-- static Glob
--
staticGlob :: IORef Sglob
staticGlob = unsafePerformIO $newIORef (Sglob [])
putGlob :: Globs -> IO ()
putGlob gl = writeIORef staticGlob (Sglob gl)
getGlob :: IO Globs
getGlob = readIORef staticGlob >>= (\sg -> return (globlist sg))

-----------------------------
-- FNGLOB
-----------------------------
fnGlob :: String -> IO [String]
fnGlob fnpat = do
    let (d,p) = splitPat fnpat
	dir = if d=="" then "." else d
	pat = compGlob p 0
    gotdir <- doesDirectoryExist dir
    if gotdir then fnGlob2 dir pat else return []

fnGlob2 :: String -> Globs -> IO [String]
fnGlob2 dir glob = do
    dc <- getDirectoryContents dir
    return $ 
      map (\x -> if dir /= "."   then dir ++ "/" ++ x   else x) 
	(filter (\f -> notElem f [".",".."] && matchGlob glob (B.pack f) == 0) dc)

splitPat s = (d,p) where
    ps = elemIndices '/' s
    (d,p) = if null ps then ("",s) 
	    else (take (last ps + 0) s, drop (last ps +1) s)

-----------------------------
-- COMPILE AND MATCH
-----------------------------
--dglob :: String -> String -> Int
--dglob px sx = matchGlob (compGlob px 0) sx

--
-- SEARCH
--
searchGlob :: Globs -> B.ByteString -> (Int,Int)
searchGlob zgls zxs = (x,res) where
    (x,res) = search' zgls zxs 0
    l = B.length zxs
    search' gls xs start
	| xs==""  = (-1,-1)
	| rc == -1 = search' gls (B.tail xs)  (start+1)
	| otherwise = (start, l-rc)
      where rc = matchGlobx gls (xs) 1

--
-- MATCH
--
-- returns -1 for fail, 0 for full match, +n for partial match (SHORTEST)
-- flag parm  for partial match - used by search
--

matchGlob g s = matchGlobx g s 0

matchGlobx :: Globs -> B.ByteString -> Int -> Int
matchGlobx [] "" _ = -1
matchGlobx mgls mxs flag = glob2 mgls mxs where
    glob2 :: Globs -> B.ByteString -> Int
    glob2 [] "" = 0
    glob2 [WildStar] _ = 0   -- comes here for trailing double *
    glob2 (WildStar:gs) (xs) = glob3 gs xs
    glob2 ((GlobBrace bl):gs) (xs) = glob4 bl gs xs
    glob2 _ "" = -1
    glob2 [] xs = if flag == 1 then B.length xs else -1
    glob2 ((GlobC c):gs) (xs) = if (c == B.head xs)
				    then glob2 gs (B.tail xs) else -1
    glob2 (WildDot:gs) (xs) = glob2 gs (B.tail xs)
    glob2 ((GlobSet s):gs) (xs) = if B.elem (B.head xs) s 
				      then glob2 gs (B.tail xs) else -1
    glob2 ((GlobNot s):gs) (xs) = if B.notElem (B.head xs) s 
				      then glob2 gs (B.tail xs) else -1

    -- WildStar
    glob3 gs xs
	| B.null xs =  glob2 gs ""
	| otherwise = if rc >= 0  then rc else glob3 gs (B.tail xs)
      where rc = glob2 gs (xs)

    -- GlobBrace
    glob4 :: [[Glob]] -> [Glob] -> B.ByteString -> Int
    glob4 [] _ _ = -1
    glob4 (b:bs) gs xs = if rc >= 0 then rc else glob4 bs gs xs
	where rc = glob2 (b++gs) xs

---------------------
--    COMPILE 
---------------------
compGlob :: String -> Int -> Globs
compGlob pat icase = reverse (comp2 pat False []) where

    comp2 :: String -> Bool -> [Glob] -> [Glob]
    --       Input  -> Escaped Char -> Ans -> Ans
    comp2 [] _ ans = ans
    comp2 ('*':xs) False gs = comp2 xs False (WildStar:gs)
    comp2 ('?':xs) False gs = comp2 xs False (WildDot:gs)
    comp2 ('[':xs) False gs = comp2 ys False gs' where
        (ys,bs) = bracket xs []
        gs' = if bs==GlobErr then [] else bs:gs
    
    comp2 ('{':xs) False gs = comp2 ys False gs' where
        (ys,bs) = brace xs
        gs' = if bs==GlobErr then [] else bs:gs
    
    comp2 ('\\':xs) False gs = comp2 xs True gs

    comp2 (x:xs) _ gs = comp2 xs False ( (ignoreCase x) : gs)
    
    ignoreCase x 
	| isAlpha x && icase == 1 = GlobSet (B.pack [toLower x,toUpper x])
	| otherwise = GlobC x

    bracket :: String -> String -> (String,Glob)
    bracket [] bs = ([],GlobErr)
    bracket (']':xs) bs = (xs,gs) where
	bs2 = B.pack $ reverse bs
	gs = if (B.head bs2) == '!' then GlobNot (B.tail bs2) else GlobSet bs2
    bracket (x:xs) bs = bracket xs 
	(if isAlpha x && icase == 1 then toLower x:toUpper x:bs else x:bs)
    
    brace :: String -> (String,Glob)
    brace xs = brace2 xs [] []
    
    brace2 :: String -> String -> [[Glob]] -> (String,Glob)
    brace2 [] tl glls = ([],GlobErr)
    brace2 ( '}':xs ) tl glls =  (xs,glls') where
	tempglob = compGlob (reverse tl) icase
        glls' = if tempglob == [] then GlobErr else (GlobBrace (glls ++ [tempglob]))
    brace2 (',':xs) tl glls =
        if tempglob == [] then ([],GlobErr) else brace2 xs [] (glls ++ [tempglob])
	where tempglob = compGlob (reverse tl) icase
    brace2 (x:xs) tl glls = brace2 xs (x:tl) glls


