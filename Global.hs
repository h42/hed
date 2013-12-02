module Global (
    Global(..)
    ,initGlobal
    ,getByteRows
    ,getrows
    ,getrow
    ,insrow
    ,insrows
    ,insByteRows
    ,updrow
    ,delrow
    ,delrows
    ,inkblk
    ,inkblkc
    ,History(..)
    ) where

import System.IO
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Ffi

--maybeRead :: Read a => String -> Maybe a
--maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

data Global = Global {
    zaccess :: Int
    ,zbuf :: String
    ,zbufl :: Int
    ,zchange :: String
    ,zcur :: Int
    ,zfind :: String
    ,zfindl :: Int
    ,zfn :: String
    ,zfnsaved :: Int
    ,zglobals :: [Global]
    ,zgo :: (Int,Int)
    ,zhistory :: [History]
    ,zindentnl :: Bool
    ,zins :: Bool
    ,zkh :: Int
    ,zkplist :: [B.ByteString]
    ,zkx1 :: Int
    ,zkx2 :: Int
    ,zky1 :: Int
    ,zky2 :: Int
    ,zlines :: Int
    ,zlist :: [B.ByteString]
    ,zmaxy :: Int
    ,zmaxx :: Int
    ,zmsg :: String
    ,zoff :: Int
    ,zpager :: Bool
    ,zrc :: Int
    ,zstmode :: Int
    ,ztabcompress :: Bool
    ,ztop :: Int
    ,zupd :: Int
    ,zupd2 :: Int
    ,zx :: Int
    ,zy :: Int
} deriving (Show)

initGlobal = Global {
    zaccess = 0
    ,zbuf = ""
    ,zbufl = 0
    ,zchange = ""
    ,zcur = -1
    ,zfind = ""
    ,zfindl = -1
    ,zfn = ""
    ,zfnsaved = 0
    ,zglobals = []
    ,zgo = (0,0)
    ,zhistory = []
    ,zindentnl = True
    ,zins = True
    ,zkh = 0
    ,zkplist = []
    ,zkx1 = -1
    ,zkx2 = -1
    ,zky1 = -1
    ,zky2 = -1
    ,zlist = []
    ,zlines = 0
    ,zmaxy = 24
    ,zmaxx = 80
    ,zmsg = ""
    ,zoff = 0
    ,zpager = False
    ,zrc = 0
    ,zstmode = 0
    ,ztabcompress = True
    ,ztop = 0
    ,zupd = 0
    ,zupd2 = 0
    ,zx = 0
    ,zy = 0
}

getrows :: Int -> Int -> Global -> [String]
getrows  y l g = map B.unpack (take l $ drop (y) (zlist g))

getByteRows :: Int -> Int -> Global -> [B.ByteString]
getByteRows  y l g = take l $ drop y (zlist g)

getrow :: Int -> Global -> String
getrow  y g = B.unpack $ (zlist g) !! y

updrow :: Int -> String -> Global -> Global
updrow y s g = g{zlist=take y (zlist g) ++ (B.pack s : drop (y+1) (zlist g))}

insrow :: Int -> String -> Global -> Global
insrow y s g = insrows y 1 [s] g

insrows :: Int -> Int -> [String] -> Global -> Global
insrows y cnt ks g = g{zlist=take y (zlist g) ++ (map B.pack ks)
			 ++ drop y (zlist g)  ,zlines=zlines g + cnt}

insByteRows :: Int -> Int -> [B.ByteString] -> Global -> Global
insByteRows y cnt ks g = g{zlist=(take (y) (zlist g)) ++ ks
			     ++ (drop y (zlist g)), zlines=zlines g + cnt}

delrow :: Int -> Global -> Global
delrow y g = delrows y 1 g

delrows :: Int -> Int -> Global -> Global
delrows y cnt g = delrows2
    g{zlist= take y (zlist g) ++ drop (y+cnt) (zlist g)
      ,zlines=zlines g - cnt}

delrows2 g =
    if zlines g > 0  then g
    else g{zx=0,zy=0,zoff=0,ztop=0,zlist=[B.empty] ,zlines=1,zpager=True}

--
-- KBLK
--
maxcol = 999999999 :: Int

inkblk :: Int -> Global -> (Int,Int)
inkblk l g
    | zkh g == 0 || zky1 g > l || zky2 g < l = (-1,-1)
    | zkh g /= 2 && zky1 g == l && zky2 g == l = (zkx1 g,zkx2 g)
    | otherwise = (0,maxcol)

inkblkc :: Int -> Int -> Global -> Bool
inkblkc x y g = if x>=x1 && x<=x2 then True else False where
    (x1,x2) = inkblk y g

-----------------------
-- HISTORY
-----------------------
data History = History {
    hx :: Int
    ,hy :: Int
    ,hoff :: Int
    ,htop :: Int
    ,hfn :: String
} deriving (Show, Read)

instance Eq History where
    h1 == h2 = (hfn h1) == (hfn h2)

instance Ord History where
    h1 < h2  = (hfn h1) < (hfn h2)
    h1 <= h2 = (hfn h1) <= (hfn h2)
    h1 >= h2 = (hfn h1) >= (hfn h2)
    h1 > h2  = (hfn h1) > (hfn h2)

