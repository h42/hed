module Func1 (
    ender
    ,enter
    ,go
    ,homer
    ,add_char
    ,bs_char
    ,ins_char
    ,del_char
    ,del_line
    ,erase_eol
    ,ins_line
    ,indent
    ,tab_char
    ,btab_char
    ,right
    ,left
    ,up
    ,down
    ,scroll
    ,pgup
    ,pgdown
    ,bottom
    ,top
    ,ins_key
    ,initFind
    ,hedFind
    ,initChange
    ,hedChange
    ,cntl_x
) where

import Data.List
import Data.Char
import Data.Maybe
import Global
import Func0
import Func2
import Hterm
import GetKB
import Display
import Glob
import Debug.Trace

-- UPOFFX
upoff :: Global -> IO Global
upoff g
    | x < zoff g  = return g{zoff=x,zpager=True}
    | x >= zoff g + zmaxx g = return g{zoff=(x - zmaxx g + 5),zpager=True}
    | otherwise = return g
  where x = zx g

-- ADJ_BUF
adj_buf :: Global -> Global
adj_buf g = g{zbufl=l, zbuf=take l buf} where
    f i a [] = a
    f i a (x:xs) = f (i+1) (if x/=' ' then i else a) xs
    buf = take (zbufl g) (zbuf g)
    l = f 0 (-1) buf + 1

-- INDENT
firstnb_r (-1) g = 0
firstnb_r y g = x where
    buf = if y == zcur g then zbuf g else  gline2 y g
    x = case (findIndex (/=' ') buf) of
	Just x' -> x'
	Nothing -> firstnb_r (y-1) g

firstnb y g = x where
    buf = if y == zcur g then zbuf g else  gline2 y g
    x = case (findIndex (/=' ') buf) of  Nothing -> 0 ; Just x' -> x'


indent :: Global -> IO Global
indent g = gline g >>= \g' -> return g'{zx=x} where
    x = if (zy g - 1)==0 then 0 else firstnb_r (zy g) g

-- ENDER
ender g = upoff g{zx=zbufl g}

-- ENTER
enter g'
    | zins g == False  = homer g >>= down
    | zx g >= zbufl g = ins_line g
    | otherwise = split_line g
  where g = adj_buf g'

split_line g = do
    let s = take (zx g) (zbuf g)
	xadd = firstnb (zy g) g
	s2 = replicate xadd ' ' ++ drop (zx g) (zbuf g)
    g2 <- k_split_line xadd g -- must come before pline
    pline g2{zbuf=s,zbufl=length s,zupd=1,zupd2=1,zpager=True}
	>>= (return . insrow (zy g2+1 ) s2) >>= homer  >>= down
	>>= \g' -> return g'{zx=xadd}

-- GO
go g' =  do
    g <- gline g'
    s <- hed_request "Enter line number: " g
    if s=="" || all isDigit s == False
	then return g
	else vupd g { zy = min ((read s :: Int) - 1) (zlines g - 1) }

-- HOMER
homer g = upoff g{zx=0}

-- ADD_CHAR
add_char c g = glineup g >>= add_char2 c
add_char2 c g
    | c == ' ' && (zx g >= zbufl g) = upoff g{zx=zx g +1}
    | zx g > zbufl g = do
	let buf = zbuf g ++ replicate (zx g - zbufl g) ' '
	add_char c g{zbuf=buf,zbufl=zx g}
    | otherwise = do
	let x = zx g
	    buf = zbuf g
	    buf' = take x buf ++ [c] ++ (drop (x+1) buf)
	    l = if x >= zbufl g then x+1 else zbufl g
	dispchar c (zy g) (zx g) g
	upoff g{zbuf = buf',zx=x+1,zbufl=l}

-- INS_CHAR
ins_char c g = glineup g >>= k_ins_char (zx g) (zy g)  >>= ins_char' c
ins_char' c g
    | c == ' ' && (zx g >= zbufl g) = upoff g{zx=zx g +1}
    | zx g > zbufl g = do
	let buf = zbuf g ++ replicate (zx g - zbufl g) ' '
	ins_char c g{zbuf=buf,zbufl=zx g}
    | otherwise = do
	let xd = if c=='}' && all (==' ') (zbuf g)
		then (find_open (zy g - 1) 0 g)
		else -1
	let x = if xd<0 then (zx g)   else xd
	    l = zbufl g
	    buf = zbuf g
	    xbuf = c : (drop x buf)
	    buf' = take x buf ++ xbuf
	displine xbuf (zy g) (x) g
	upoff g{zbuf = buf',zx=x+1,zbufl=l+1}

find_open y st g
    | y <= 0 = 0
    | st < 0    = fnb
    | otherwise = find_open (y-1) (trace "hey" st2) g
  where buf = gline2 y g
	(f,l,fnb) = get_fl (' ',' ', -1) buf
	l2 = if l=='{' then 1 else 0
	f2 = if f=='}' then 1 else 0
	st2 = st - l2 + f2

get_fl (f,l,pos) [] = (f,l,pos)
get_fl (f,l,pos) (x:xs) = get_fl (f',l',pos') xs where
    (f',pos')
	| f==' ' && x/=' '  = (x,pos+1)
	| f==' ' && x==' '  = (' ',pos+1)
	| otherwise =  (f,pos)
    l' = if x/=' ' then x else l

-- BS_CHAR
bs_char :: Global -> IO Global
bs_char g
    | zx g > zbufl g = left g
    | zx g > 0 = del_char g{zx=zx g - 1}
    | zy g <= 0 =return g
    | otherwise = pline g >>= up >>= ender >>= del_char

-- DEL_CHAR
del_char :: Global -> IO Global
del_char g
    | zx g < zbufl g = glineup g >>= k_del_char (zx g) (zy g) >>= del_char'
    | zy g == zlines g - 1 = return g
    | otherwise = join_line g

del_char' g = do
    let x = zx g
	b = take x (zbuf g) ++ (drop (x+1) (zbuf g))
    displine (drop x b) (zy g) x g
    return g{zbuf=b,zbufl=zbufl g-1}

erase_eol :: Global -> IO Global
erase_eol g = displine " " (zy g) (zx g) 
    g{zbuf = take (zx g) (zbuf g), zbufl=zx g, zupd=1, zupd2=1}

-- JOIN_LINE
join_line gin = do
    g <- glineup gin >>= k_join_line
    let s2 = gline2 (zy g + 1) g
	l = (zx g + 0) + length s2
	s = zbuf g ++ (replicate ( (zx g) + 0 - (zbufl g)) ' ') ++ s2
    return $ delrow (zy g + 1) g{zbuf=s,zbufl=l,zpager=True}

-- DEL_LINE
del_line g = k_del_line (zy g) g >>= del_line'
del_line' g
    | zlines g == 1 = do
	clrscr g
	glineup g{zx=0,zy=0,zoff=0,zbuf="",zbufl=0}
    | otherwise = do
	let y = if (zy g >= zlines g -2) then zlines g -2 else zy g
	let t = if (ztop g >= zlines g -2) then zlines g -2 else ztop g
	let g' = g{zcur = -1,zpager=True}
	return  (delrow (zy g' ) g'){ztop=t,zupd=0,zupd2=1,zy=y} >>= gline

-- INS_LINE
ins_line g = pline g >>= k_ins_line (zy g + 1) >>= ins_line2
ins_line2 g = do
    let g' = insrow (zy g+1 ) "" g
	x = if (zindentnl g)  then firstnb (zy g) g + brace else 0
	brace = if zbufl g>0 && last (zbuf g) =='{' then 4 else 0
    vupd g'{zy=zy g + 1,zx=x,zoff=0,zpager=True}  >>= glineup

-- TAB_CHAR
tab_char g =  upoff g{zx = 4 - mod (zx g) 4 + (zx g)}

-- BTAB_CHAR
btab_char g = upoff g{zx = (zx g) - if m>0 then m else 4} where
    m = mod (zx g) 4

-- INS_KEY
ins_key g = return $ if zins g then g{zins=False} else g{zins=True}

-- RIGHT
right :: Global -> IO Global
right g = upoff g{zx = zx g + 1}

-- LEFT
left :: Global -> IO Global
left g = upoff g{zx = x'} where
    x = (zx g) - 1
    x' = if x < 1 then 0 else x

-- UP DOWN ---------------------
up :: Global -> IO Global
up g
    | zy g <= 0 =  return g
    | otherwise = do
	g' <- pline g
	vupd g'{zy=zy g' -1} >>= gline

down :: Global -> IO Global
down g
    | zy g >= zlines g - 1 = return g
    | otherwise = do
	g' <- pline g
	vupd g'{zy=zy g' +1} >>= gline

scroll :: Int -> Global -> IO Global
scroll dir g 
    | dir /= 0 = pline g >>= scroll' dir >>= gline
    | otherwise = return g

scroll' dir g = do
    let t = max (min (ztop g + dir) (zlines g - 1)) 0
	y  | (zy g) >= t + (zmaxy g) -1 =  max (zy g - 1) 0
	   | (zy g) < t = t
	   | otherwise = (zy g)
    return g{ztop=t,zy=y,zpager=True}

vupd g
    | zy g < ztop g = return g{ztop=zy g,zpager=True}
    | zy g -ztop g >= zmaxy g - 1 = do
	let t = (zy g) - (zmaxy g -1) + 1
	return g{ztop=t,zpager=True}
    | otherwise = return g

-- PG UP / DOWN
pgup :: Global -> IO Global
pgup g = pline g >>= pgup2 >>= gline
pgup2 g
    | ztop g <= 0 = return g{ztop=0,zy=0}
    | ztop g <= zmaxy g = return g{ztop=0,zy=0,zpager=True}
    | otherwise = do
	let t = max (ztop g - zmaxy g + 1) 0
	    y = max (zy g - zmaxy g + 1) 0
	return g{ztop=t,zy=y,zpager=True}

pgdown :: Global -> IO Global
pgdown g = pline g >>= pgdown2 >>= gline
pgdown2 g
    | ztop g >= zlines g - 1 = return g{zy=zlines g -1}
    | otherwise = do
	let t = min (ztop g + zmaxy g - 1) (zlines g -1)
	    y = min (zy g + zmaxy g - 1) (zlines g -1)
	return g{ztop=t,zy=y,zpager=True}

-- BOTTOM / TOP
bottom :: Global -> IO Global
bottom g = pline g >>= bottom2 >>= gline
bottom2 g = do
    let t = max 0 (zlines g - (zmaxy g - 1))
	y = zlines g - 1
    return g{zx=0,zy=y,ztop=t,zoff=0,zmsg="Bottom",zpager=True}

top :: Global -> IO Global
top g = pline g >>= top2 >>= gline
top2 g = return g{zx=0,zy=0,ztop=0,zoff=0,zpager=True}

----------------------
-- Find / Update
----------------------
initFind icase g = do
    s <- hed_request "Enter search string: " g
    if s == "" then return g
    else do
	let gl = compGlob s icase
	if (gl) == [] then return g{zmsg="Invalid search string",zfind=""}
	else do
	    putGlob gl
	    gline g{zfind=s} >>= hedFind

hedFind g = pline g >>= hedFind1

hedFind1 g' = do
    g <- gline g'
    gl <- getGlob
    if gl /= [] then do
	let (start,len) = searchGlob gl sx
	    sx = drop (zx g+1) (zbuf g)
	if start >= 0 then
	    return g{zx=zx g + 1 + start,zfindl=len}
	else hedFind2 gl (zy g+1) g
    else return g{zmsg="Find not primed"}

hedFind2 gl y g = do
    if y >= zlines g then return g{zmsg="Not found"}
    else do
	let (start,len) = searchGlob gl sx
	    sx = gline2 y g
	if start >= 0 then gline g{zx=start,zy=y,zfindl=len} >>= vupd
	else hedFind2 gl (y+1) g

initChange g = do
    s <- hed_request "Enter search string: " g
    if s == "" then return g
    else do
	let gl = compGlob s 0
	if gl == [] then return g{zmsg="Invalid search string",zfind=""}
	else do
	    putGlob gl
	    cs <- hed_request "Enter replace string: " g
	    if cs == "" then return g{zmsg="Change cancelled"}
	    else return g{zfind=s,zchange=cs}

hedChange g'
    | zfind g' == "" || zchange g' == ""  =  return g'{zmsg="Change not primed"}
    | otherwise = do
	g <- gline g'
	gl <- getGlob
	let s = drop (zx g) (zbuf g)
	    rc = matchGlobx gl s 1
	if rc < 0 then return g{zmsg="Find required"}
	else do
	    let clen = length s - rc
		buf = take (zx g) (zbuf g) ++ (zchange g) ++ drop (zx g + clen) (zbuf g)
	    displine (drop (zx g) buf) (zy g) (zx g)
		g{zbuf=buf,zbufl=length buf,zmsg="Changed"}
		>>= glineup

toggleCase g  = glineup g >>= toggleCase'
toggleCase' g  
    | zx g >= zbufl g = return g
    | otherwise = do
	let c  = (zbuf g) !! (zx g)
	add_char (if isLower c then toUpper c else toLower c) g 

toggleIndent g = return g{zindentnl=ind, zmsg=msg} where
    ind = not $ zindentnl g
    msg = "indent set to " ++ show ind

-------------------------------
-- CNTL_X
-------------------------------
cntl_x :: Global -> IO Global
cntl_x g = do
    k <- getkb
    case k of
	KeyChar c -> cntl_x' c g
	KeyCntl c -> cntl_x' c g
	_ -> return g{zmsg="Unknown k function"}

cntl_x' c g = do
    case toLower c of
	'c' -> initChange g
	'f' -> initFind (if isLower c then 1 else 0) g
	'i' -> toggleIndent g
	'x' -> toggleCase g
	_   -> return g{zmsg="Unknown x function"}

