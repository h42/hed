module Display (
    clrscr
    ,goto
    ,status
    ,dispchar
    ,displine
    ,disppage
    ,hed_request
    ,hed_requestx
    ,chk_winsize
    )  where

import Control.Monad
import System.IO
import Ffi
import Func0
import Global
import HTerm

chk_winsize g = do
    (rc,rows,cols) <- h_winsize
    if rc==0 && (rows/=zmaxy g || cols/=zmaxx g)
	then return g{zmaxy=rows,zmaxx=cols,zpager=True}
	else return g

clrscr :: Global -> IO Global
clrscr g = do
    tclrscr
    return g

goto g = tgoto (zy g - ztop g) (zx g - zoff g)

dispchar :: Char -> Int -> Int -> Global -> IO Global
dispchar c y x g
    | x>=(zoff g) && x-(zoff g)<(zmaxx g) = do
	let att = inkblkc x y g
	if att then tattr Cyan else return ()
	tputchar c (y-ztop g) (x - (zoff g))
	if att then tattr Norm else return ()
	return g
    | otherwise = return g

displine :: String -> Int -> Int -> Global -> IO Global
displine s y x g = do
    let (s',x') = visableline g s x
    let (kx1,kx2) = inkblk y g
	(s1,s2,s3) = getksegs s' (x'+zoff g) kx1 kx2 [] [] []
    tgoto (y-ztop g) x'
    tclreol
    let l1 = length s1
	l2 = length s2
    if s1 /= "" then tputs s1 (y-ztop g) x' else return ()
    if s2 /= "" then do
		    tattr Cyan
		    tputs s2 (y-ztop g) (x'+l1)
		    tattr Norm
		else return ()
    if s3 /= "" then tputs s3 (y-ztop g) (x'+l1+l2) else return ()
    return g

getksegs [] _ _ _ s1 s2 s3 = (reverse s1,reverse s2,reverse s3)
getksegs (s:sx) x kx1 kx2 s1 s2 s3
    | x<kx1 = getksegs sx (x+1) kx1 kx2 (s:s1) s2 s3
    | x>kx2 = getksegs sx (x+1) kx1 kx2 s1 s2 (s:s3)
    | otherwise = getksegs sx (x+1) kx1 kx2 s1 (s:s2) s3

visableline :: Global -> String -> Int -> (String,Int)
visableline g s x
    | (x >= (zoff g + zmaxx g)) || (x + l <= zoff g) = ("",0)
    | otherwise = ( (take (x2-x1+1) (drop (x1 - x) s)) , x1-zoff g)
  where
	l = length s
	x1 = max x (zoff g)
	x2 = min (zoff g + zmaxx g -1) (x+l)

-- PLINE REQUIRED BEFORE DISPPAGE
disppage :: Global -> IO Global
disppage g = do
    g' <- (clrscr g >>= pline)
    let ls = getrows (ztop g') (zmaxy g' -1) g'
    forM_ (zip ls [ztop g ..]) (\(s,i)-> displine (tabexpand 0 s) i 0 g)
    return g

status g = do
    let s = show (zy g +1) ++ "," ++ show (zx g+1)
	    -- ++ " - " ++ (show (zoff g))
	x = zmaxx g - length s
    tgoto (zmaxy g -1) 0
    tclreol
    if not $ null (zmsg g)
     then  do
	 tattr Cyan
	 putStr $ zmsg g
	 tattr Norm
     else return ()
    if x >= 0
     then do
	tgoto (zmaxy g -1) x
	putStr s
     else return ()

----------------------------------------------
-- REQUEST
----------------------------------------------
hed_request s g = hed_requestx s 0 g

hed_requestx s flag g = do
    tgoto (zmaxy g -1) 0
    putStr (s)
    tclreol
    trequest [] 0 (zmaxy g -1) (length s + 1) flag

