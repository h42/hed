module Glob (
    Glob 
    ,Globs
    ,compGlob
    ,matchGlob
    ,matchGlobx
    ,searchGlob
    ,dglob
    ,Sglob
    ,putGlob
    ,getGlob
    ,fnGlob
) where

import Data.Char
import Data.List
import Foreign
import Data.IORef
import System.Directory (doesDirectoryExist, doesFileExist,
			 getCurrentDirectory, getDirectoryContents)

data Glob = GlobC Char
	    | GlobSet String
	    | GlobNot String
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
	(filter (\f -> notElem f [".",".."] && matchGlob glob f == 0) dc)

splitPat s = (d,p) where
    ps = elemIndices '/' s
    (d,p) = if null ps then ("",s) 
	    else (take (last ps + 0) s, drop (last ps +1) s)

-----------------------------
-- COMPILE AND MATCH
-----------------------------
dglob :: String -> String -> Int
dglob px sx = matchGlob (compGlob px 0) sx

--
-- SEARCH
--
searchGlob :: Globs -> String -> (Int,Int)
searchGlob zgls zxs = (x,res) where
    (x,res) = search' zgls zxs 0
    l = length zxs
    search' _ [] _ = (-1,-1)
    search' gls (x2:xs) start
	| rc == -1 = search' gls xs  (start+1)
	| otherwise = (start, l-rc)
      where rc = matchGlobx gls (x2:xs) 1

--
-- MATCH
--
-- returns -1 for fail, 0 for full match, +n for partial match (SHORTEST)
-- flag parm  for partial match - used by search
--
matchGlob g s = matchGlobx g s 0

matchGlobx :: Globs -> String -> Int -> Int
matchGlobx [] [] _ = -1
matchGlobx mgls mxs flag = glob2 mgls mxs where
    glob2 [] [] = 0
    glob2 [WildStar] _ = 0   -- comes here for trailing double *
    glob2 (WildStar:gs) (xs) = glob3 gs xs
    glob2 ((GlobBrace bl):gs) (xs) = glob4 bl gs xs
    glob2 _ [] = -1
    glob2 [] xs = if flag == 1 then length xs else -1
    glob2 ((GlobC c):gs) (x:xs) = if (c == x) then glob2 gs xs else -1
    glob2 (WildDot:gs) (x:xs) = glob2 gs xs
    glob2 ((GlobSet s):gs) (x:xs) = if elem x s then glob2 gs xs else -1
    glob2 ((GlobNot s):gs) (x:xs) = if notElem x s then glob2 gs xs else -1

    -- WildStar
    --glob3 gs [] rc = False
    glob3 gs [] = glob2 gs []
    glob3 gs (x:xs) = if rc >= 0  then rc else glob3 gs xs
	where rc = glob2 gs (x:xs)

    -- GlobBrace
    glob4 :: [[Glob]] -> [Glob] -> String -> Int
    glob4 [] _ _ = -1
    glob4 (b:bs) gs xs = if rc >= 0 then rc else glob4 bs gs xs
	where rc = glob2 (b++gs) xs

---------------------
--    COMPILE 
---------------------
compGlob :: String -> Int -> Globs
compGlob pat icase = reverse (comp2 pat []) where
    comp2 :: String -> [Glob] -> [Glob]
    comp2 [] ans = ans
    comp2 ('*':xs) gs = comp2 xs  (WildStar:gs)
    comp2 ('?':xs) gs = comp2 xs (WildDot:gs)
    comp2 ('[':xs) gs = comp2 ys gs' where
        (ys,bs) = bracket xs []
        gs' = if bs==GlobErr then [] else bs:gs
    
    comp2 ('{':xs) gs = comp2 ys gs' where
        (ys,bs) = brace xs
        gs' = if bs==GlobErr then [] else bs:gs
    
    comp2 (x:xs) gs = comp2 xs ( (ignoreCase x) : gs)
    
    ignoreCase x 
	| isAlpha x && icase == 1 = GlobSet [toLower x,toUpper x]
	| otherwise = GlobC x

    bracket :: String -> String -> (String,Glob)
    bracket [] bs = ([],GlobErr)
    bracket (']':xs) bs = (xs,gs) where
        bs2 = reverse bs
        gs = if (head bs2) == '!' then GlobNot (tail bs2) else GlobSet bs2
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


