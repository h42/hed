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
    ,toZlist
    ,fromZlist
    ,emptyZlist
    ,lengthZlist
    ) where

import System.IO
import Data.Maybe
import qualified Data.ByteString.UTF8 as U
import qualified Data.Sequence as S
import Data.Foldable
import Ffi

-- make rest of program agnostic to zlist type
fromZlist :: S.Seq U.ByteString -> [U.ByteString]
fromZlist s = toList s

toZlist :: [U.ByteString] -> S.Seq U.ByteString
toZlist ts = S.fromList ts

emptyZlist = S.singleton (U.fromString "") -- use singleton to avoid index error
lengthZlist s = S.length s

--maybeRead :: Read a => String -> Maybe a
--maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

data Global = Global {
    zx :: Int
    ,zy :: Int
    ,zrc :: Int
    ,zbuf :: String
    ,zbufl :: Int
    ,zlist :: S.Seq U.ByteString
    ,zkplist :: S.Seq U.ByteString
    ,zcur :: Int
    ,zupd :: Int
    ,zupd2 :: Int
    ,zlines :: Int
    ,zins :: Bool
    ,ztop :: Int
    ,zoff :: Int
    ,zmaxy :: Int
    ,zmaxx :: Int
    ,zmsg :: String
    ,zindentnl :: Bool
    ,zaccess :: Int
    ,zfn :: String
    ,zfnsaved :: Int
    ,zstmode :: Int
    ,zglobals :: [Global]
    ,zhistory :: [History]
    ,zkx1 :: Int
    ,zkx2 :: Int
    ,zky1 :: Int
    ,zky2 :: Int
    ,zkh :: Int
    ,zpager :: Bool
    ,zfind :: String
    ,zfindl :: Int
    ,zchange :: String
} deriving (Show)

initGlobal = Global {
    zx = 0
    ,zy = 0
    ,zrc = 0
    ,zbuf = ""
    ,zbufl = 0
    ,zlist = S.empty
    ,zkplist = S.empty
    ,zcur = -1
    ,zupd = 0
    ,zupd2 = 0
    ,zlines = 0
    ,zins = True
    ,ztop = 0
    ,zoff = 0
    ,zmaxy = 24
    ,zmaxx = 80
    ,zmsg = ""
    ,zindentnl = True
    ,zaccess = 0
    ,zfn = ""
    ,zfnsaved = 0
    ,zstmode = 0
    ,zglobals = []
    ,zhistory = []
    ,zkx1 = -1
    ,zkx2 = -1
    ,zky1 = -1
    ,zky2 = -1
    ,zkh = 0
    ,zpager = False
    ,zfind = ""
    ,zfindl = -1
    ,zchange = ""
}

getrows :: Int -> Int -> Global -> [String]
getrows  y l g = map U.toString (toList $ S.take l $ S.drop (y) (zlist g))

getByteRows :: Int -> Int -> Global -> S.Seq U.ByteString
getByteRows  y l g = S.take l $ S.drop (y) (zlist g)

getrow :: Int -> Global -> String
getrow  y g = U.toString $ S.index (zlist g) y

updrow :: Int -> String -> Global -> Global
updrow y s g = g{zlist= S.update y (U.fromString s) (zlist g) }

insrow :: Int -> String -> Global -> Global
insrow y s g = insrows y 1 [s] g

insrows :: Int -> Int -> [String] -> Global -> Global
insrows y cnt ks g = g{zlist=(S.take (y) (zlist g))
			 S.>< S.fromList (map U.fromString ks)
			 S.>< (S.drop y (zlist g))  ,zlines=zlines g + cnt}

insByteRows :: Int -> Int -> S.Seq U.ByteString -> Global -> Global
insByteRows y cnt ks g = g{zlist=(S.take (y) (zlist g)) S.>< ks
			   S.>< (S.drop y (zlist g)), zlines=zlines g + cnt}

delrow :: Int -> Global -> Global
delrow y g = delrows y 1 g

delrows :: Int -> Int -> Global -> Global
delrows y cnt g = g{zlist= (S.take (y) (zlist g)) S.>< (S.drop (y+cnt) (zlist g))
	      ,zlines=zlines g - cnt}

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

