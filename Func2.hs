module Func2 (
    cntl_k
    ,k_ins_char
    ,k_del_char
    ,k_del_line
    ,k_ins_line
    ,k_join_line
    ,k_split_line
) where

import Data.Char
import System.IO
import Global
import GetKB
import Display
import Func0

cntl_k :: Global -> IO Global
cntl_k g = do
    k <- getkb
    case k of
	KeyChar c -> cntl_k' c g
	KeyCntl c -> cntl_k' c g
	_ -> return g{zmsg="Unknown k function"}

cntl_k' c g = do
    case toLower c of
	'b' -> cntl_kb g
	'c' -> cntl_kc g
	'd' -> cntl_kd g
	'h' -> cntl_kh g
	'k' -> cntl_kk g
	'l' -> cntl_kl g
	'p' -> cntl_kp g
	's' -> cntl_ks g
	'v' -> cntl_kv g
	'>' -> cntl_kadj 1 g
	'<' -> cntl_kadj (-1) g
	'?' -> cntl_kdisp g
	_   -> return g{zmsg="Unknown k function"}

maxcol = 999999999 :: Int

goodblk g
    | goodk g = return g{zkh=1,zmsg="Block on"}
    | otherwise = return g{zkh=0,zmsg="Block off"}

goodk g
    | zky1 g < 0 || zkx1 g < 0 || zky1 g > zky2 g = False
    | zky1 g == zky2 g && zkx1 g > zkx2 g = False
    | otherwise = True

---------------------
-- Func2 functions
---------------------
cntl_kl g = return g{zmsg="Block on",zkh=2,zkx1=0,zky1=zy g,
    zkx2=maxcol,zky2=zy g,zpager=True}
cntl_kb g = goodblk g{zkx1=zx g,zky1=zy g,zpager=True}
cntl_kk g = goodblk g{zkx2=zx g,zky2=zy g,zpager=True}
cntl_kh g
    | zkh g /= 0 || not (goodk g) = return g{zkh=0,zpager=True,zmsg="Block off"}
    | zky1 g /= zky2 g || zkx1 g > 0 || zkx2 g > zbufl g
	 = return g{zkh=1,zmsg="Block on",zpager=True}
    | otherwise = return g{zkh=2,zmsg="Block on",zpager=True}

-----------
-- CNTL_KADJ
-----------
cntl_kadj :: Int -> Global -> IO Global
cntl_kadj d g
    | zkh g == 0 = return g{zmsg="Block off"}
    | otherwise = do
	s <- hed_request "Enter number to adjust: " g
	if s == "" then
	    return g{zmsg="cancelled"}
	else if not $ all isDigit s then
	    return g{zmsg="bad adjustment number"}
	else do
	    let x = d * (read s :: Int)
	    pline g{zupd2=1,zpager=True,zmsg=(show x)} >>= kadj2 x

kadj2 x g = do
    let l = zky2 g - zky1 g + 1
	ks = getrows (zky1 g) l g
	ks' | x > 0 = map
	     (\k -> (tabcomp . ((replicate x ' ') ++) . (tabexpand 0)) k) ks
	    | x < 0 = map (\k -> (tabcomp . drop (-x) . tabexpand 0) k) ks
	    | otherwise = ks
	g' = (insrows (zky1 g) l ks' . delrows (zky1 g) l) g
    gline g'{zcur= -1}

-----------
-- CNTL_KC
-----------
cntl_kc g
    | zkh g == 0 = return g{zmsg="No block"}
    | zkh g == 1 && zky1 g == zky2 g = pline g >>= glineup >>= cntl_kc1
    | otherwise = pline g{zupd2=1} >>= cntl_kc2 >>= gline

cntl_kc1 g = do
    let l = zkx2 g - zkx1 g + 1
	b = take l (drop (zkx1 g) (gline2 (zky1 g) g))
	-- repeat ' ' necessary for zx > zbufl
	b2 = take (zx g) (zbuf g ++ repeat ' ') ++ b ++ (drop (zx g) (zbuf g))
    gline g{zbuf=b2,zkx1=zx g,zkx2=zx g+l-1,zky1=zy g,zky2=zy g,
	    zbufl=zbufl g+l,zpager=True}

cntl_kc2 g = do
    let l = zky2 g - zky1 g + 1
	ks = getrows (zky1 g) l g
	g' = insrows (zy g + 1) l ks g
    return g'{zpager=True,zky1=zy g+1,zky2=zy g + l }

-----------
-- CNTL_KP / CNTL_KS 
-----------
cntl_ks g
    | zkh g == 0 = return g{zmsg="No block"}
    | otherwise = do
	let l = zky2 g - zky1 g + 1
	pline g{zmsg="block saved",zkplist=getByteRows (zky1 g) l g}

cntl_kp g
    | zkplist g == [] = return g{zmsg="nothing to paste"}
    | otherwise = do
	let l = length $ zkplist g
	    g' = insByteRows (zy g + 1) l (zkplist g) g
	return g'{zpager=True,zky1=zy g+1,zky2=zy g + l,
		  zupd2=1,zmsg="block pasted" }

------------
-- CNTL_KD
------------
cntl_kd g    
    | zkh g == 0 = return g{zmsg="No block"}
    | zkh g == 1 && zky1 g == zky2 g  = pline g >>= glineup >>= cntl_kd1
    | otherwise  = pline g{zupd2=1} >>= cntl_kd2 >>= gline

cntl_kd1 g = do
    let b = gline2 (zky1 g) g
	b2 = (take (zkx1 g) b) ++ (drop (zkx2 g+1) b)
	g2 = pline2 (zky1 g) b2 g
    gline g2{zkh=0,zkx1= -1,zkx2= -1,zpager=True,zcur= -1}
-- cntl_kd2 g = do

cntl_kd2 g = do
    let l = zky2 g - zky1 g + 1
	y | zy g <= zky1 g = zy g
	  | zy g <= zky2 g = zky1 g
	  | otherwise      = zy g - l
	g' = delrows (zky1 g) l g
    return g'{zy=y,zkh=0,zkx1= -1,zkx2= -1,zky1= -1, zky2= -1,
	      zpager=True,zcur= -1}

-------------
-- CNTL_KV
-------------
cntl_kv g
    | zkh g == 0 = return g{zmsg="No block"}
    | zkh g == 2 || zky1 g /= zky2 g = pline g{zupd2=1} >>= cntl_kv2 >>= gline
    | zky1 g == zky2 g && zky1 g == zy g && (zx g >= zkx1 g && zx g <= zkx2 g) =
	 return g{zmsg="Block not moved"}

    | zky1 g == zky2 g && zy g == zky1 g = glineup g >>= cntl_kv1a
    | zky1 g == zky2 g = pline g >>= glineup >>= cntl_kv1b

    | zy g >= zky1 g -1 && zy g <= zky2 g = return g{zmsg="block not moved"}
    | otherwise = return g{zmsg="block not moved"}

cntl_kv1a g = do
    let l = zkx2 g - zkx1 g + 1
	x = if zx g < zkx1 g then zx g else zx g - l
	kblk = take l (drop (zkx1 g) (zbuf g))
	blk0 = take (zkx1 g) (zbuf g) ++ drop (zkx2 g + 1) (zbuf g)
	b = take x (blk0 ++ repeat ' ') ++ kblk ++ (drop x blk0)
    return g{zbuf=b, zbufl=length b, zx=x, zkx1=x, zkx2=x+l-1, zpager=True}

cntl_kv1b g = do
    let b = gline2 (zky1 g) g
	l = zkx2 g - zkx1 g + 1
	kblk = take l (drop (zkx1 g) b)
	b2 = (take (zkx1 g) b) ++ (drop (zkx2 g+1) b)
	g2 = pline2 (zky1 g) b2 g
	-- repeat ' ' necessary for zx > zbufl
	buf = take (zx g) (zbuf g ++ repeat ' ') ++ kblk ++ (drop (zx g) (zbuf g))
    return g2{zbuf=buf, zbufl=length buf,
	zky1=zy g, zky2=zy g, zkx1=zx g, zkx2=zx g + l - 1,zpager=True}

cntl_kv2 g = do
    let l = zky2 g - zky1 g + 1
	y | zy g <= zky1 g = zy g + 1
	  | otherwise      = zy g - l + 1
	ks = getrows (zky1 g) l g
	g' = insrows y l ks (delrows (zky1 g) l g)
    return g'{zy=y,zky1=y,zky2=y+l-1,zpager=True,zcur= -1}

cntl_kdisp g = return g{zmsg=s} where
    s = "x1=" ++ (show (zkx1 g)) ++ "  x2=" ++ (show (zkx2 g)) ++
	"  y1=" ++ (show (zky1 g)) ++ "  y2=" ++ (show (zky2 g)) ++
	"  zkh=" ++ (show (zkh g))
---------------------
-- Func 1 assist
---------------------
k_init g = return g{zkh=0,zkx1= -1,zky1= -1,zkx2= -1,zky2= -1}

tmsg g= "x1=" ++ show (zkx1 g) ++ " y1=" ++ show (zky1 g)
	++ " x2=" ++ show (zkx2 g) ++ " y2=" ++ show (zky2 g)

k_split_line xadd g =
    if zkh g == 0 || zy g > zky2 g then  return g
    else if zy g == zky2 g && zx g >= zbufl g then return g
    else if zy g < zky1 g || zy g == zky1 g && zx g == 0 then
	return g{zky1=zky1 g +1,zky2=zky2 g +1}
    else if zky1 g /= zky2 g  then return g{zky2=zky2 g+1}
    else if zx g <= zkx1 g then do
	let x1 = xadd + zkx1 g - zx g
	    x2 = x1 + zkx2 g - zkx1 g
	return g{zkx1=x1,zkx2=x2,zky1=zky1 g +1,zky2=zky2 g +1}
    else if zx g > zkx2 g then return g
    else return g{zkh=0}

k_join_line g =
    if zkh g == 0 || zy g >= zky2 g then  return g
    else if zkh g == 2 || zky1 g /= zky2 g then do
	if zy g < zky1 g then return g{zky1=zky1 g -1, zky2= zky2 g -1}
	else if zy g >= zky1 g && zy g < zky2 g then return g{zky2=zky2 g -1}
	else if zy g >= zky1 g && zy g == zky2 g then return g
	else return g
    else if zy g +1 == zky1 g then do
	let xadd = zx g
	return g{zky1=zky1 g -1, zky2= zky2 g -1,
		 zkx1=zkx1 g +xadd, zkx2=zkx2 g + xadd}
    else if zy g < zky1 g then return g{zky1=zky1 g -1, zky2= zky2 g -1}
    else return g{zkh=0}

k_ins_line y g
    | zkh g == 0 || zky1 g > zky2 g || (zky1 g == zky2 g && zkx1 g > zkx2 g)
	= return g
    | y > zky2 g = return g
    | y < zky1 g = return g{zky1=zky1 g +1, zky2=zky2 g +1}
    | y > zky1 g && y <= zky2 g = return g{zky2=zky2 g +1}
    | y == zky1 g = return g{zky1=zky1 g +1, zky2=zky2 g +1}

k_del_line y g
    | zkh g == 0 || zky1 g > zky2 g
	|| y == zky1 g && y == zky2 g  = k_init g
    | y > zky2 g = return g
    | y < zky1 g = return g{zky1=zky1 g -1,zky2=zky2 g -1}
    | otherwise  = return g{zky2=zky2 g -1}

k_del_char x y g
    | zkh g /= 2 && y == zky1 g && zky1 g == zky2 g && zkx1 g <= x
	&& zkx2 g >= x  = return g{zkx2=zkx2 g - 1}
    | otherwise = return g

k_ins_char x y g
    | zkh g /= 2 && y == zky1 g && zky1 g == zky2 g && zkx1 g <= x
	&& zkx2 g >= x  = return g{zkx2=zkx2 g + 1}
    | otherwise = return g

