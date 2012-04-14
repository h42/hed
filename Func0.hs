module Func0 (
    gline
    ,glineup
    ,gline2
    ,pline
    ,pline2
    ,tabexpand
    ,tabcomp
) where

import Data.List
import Global

tabexpand :: Int -> String -> String
tabexpand i [] = []
tabexpand i (x:xs)
    | x == '\x09' =  replicate n ' ' ++ tabexpand (i+n) xs
    | otherwise = x : (tabexpand (i+1) xs)
  where n = (8 - mod i 8)

space8 = "        " :: String

tabcomp :: String -> String
tabcomp xs
    | isPrefixOf space8 xs  = '\x09' :  (tabcomp $ drop 8 xs)
    | otherwise = xs

trim :: String -> String
trim bs = take l' bs where
    (x',l') = foldl
	(\(x,l) c -> if c==' '||c=='\x09' then (x+1,l) else (x+1,x+1)) (0,0) bs

-- PLINE
--updlist val pos ls = take pos ls ++ [val] ++  drop (pos+1) ls

pline :: Global -> IO Global
pline g
    | zcur g < 0 || zupd g /= 1   = return g
    | otherwise = return $ updrow (zcur g) (tabcomp.trim $ zbuf g) g{zupd=0}

pline2 :: Int -> String -> Global -> Global
pline2 y b g = updrow y (tabcomp b) g

-- GLINE

gline2 :: Int -> Global -> String
gline2 y g = tabexpand 0 (getrow y g)

glineup g = gline g{zupd=1,zupd2=1}

gline :: Global -> IO Global
gline g
    | zy g == zcur g  =  return g
    | otherwise  = return $ gline' g{zcur = zy g} (getrow (zy g) g)

gline' g b  = g' where
    b' = tabexpand 0 b
    g' = g {zbuf = b', zbufl = length b'}

