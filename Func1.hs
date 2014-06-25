module Func1 (
     add_char
    ,bottom
    ,bs_char
    ,btab_char
    ,bword
    ,del_bword
    ,del_char
    ,del_line
    ,del_word
    ,down
    ,ender
    ,enter
    ,erase_eol
    ,go
    ,homer
    ,indent
    ,ins_char
    ,ins_key
    ,ins_line
    ,left
    ,pgup
    ,pgdown
    ,right
    ,scroll
    ,tab_char
    ,top
    ,undo
    ,up
    ,vupd
    ,word
) where

import Data.Char
import Data.List
import Data.Maybe

import Display
import Global
import Func0
import Func2

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
    x = firstnb_r (zy g) g

-- ENDER
ender g = gline g{zx=zbufl g} >>= upoff

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
    if s == "g" then upoff g{zx=fst $zgo g, zy=snd $zgo g,zpager=True} >>= vupd
    else if s=="" || all isDigit s == False then return g
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
	let xd = if c=='}' && all (==' ') (zbuf g) -- all sets empty list true
		then (find_open (zy g - 1) 0 g)
		else -1
	let x = if xd<0 then (zx g)   else xd
	    l = zbufl g
	    buf = zbuf g
            (n,xbuf) = ins_char2 c x buf
	    buf' = take x buf ++ xbuf
	displine xbuf (zy g) (x) g
        upoff g{zbuf = buf',zx=x+1,zbufl=l+n}

ins_char2 c x buf
    | x == 0 = (1, c : buf)
    -- | c == '"' && c0 /= '"' = (2, '"' : '"' : xbuf)
    -- | c == '(' = (2, '(': ')' : xbuf)
    | otherwise = (1, c : xbuf)
  where (c0:xbuf) = drop (x-1) buf

find_open y st g
    | y < 0 = 0
    | st2 < 0    = fnb
    | otherwise = find_open (y-1) st2 g
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
    | ztop g + zmaxy g >= zlines g + 1 = return g{zy=zlines g -1, zx=0}
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

undo g
    | length (zglobals g) < 2 = return g
    | otherwise = return (head $ tail (zglobals g)){zpager=True}

-- WORD / BWORD
word :: Global -> IO Global
word g = case (findIndex (==' ') (drop ((zx g)+1) (zbuf g))) of
             Just x' -> word' (x'+(zx g)+1) g
             Nothing -> word2 g
word' x g = case (findIndex (/=' ') (drop x (zbuf g))) of
                Just x' -> upoff g{zx=x'+x}
                Nothing -> word2 g
word2 g = if (zy g)+1 >= zlines g then return g
          else homer g >>= down >>= word3
word3 g = if (not $ null (zbuf g)) && head (zbuf g) == ' ' then word g
          else return g

-- WORD / BWORD
bword :: Int -> Global -> IO Global
bword n g
    | (zx g) > 0 && take (zx g) (zbuf g) /= take (zx g) (repeat ' ')
        = upoff g{zx=length xs}
    | zy g > 0 && n/=0 = up g >>= ender >>= bword (n-1)
    | otherwise = upoff g{zx=0}
  where
    xs = dropWhile (/=' ') $ dropWhile (==' ') $ reverse $ take (zx g) (zbuf g)

-- DEL WORD
del_word :: Global -> IO Global
del_word g = glineup g >>= del_word2
del_word2 g
    | zx g >= zbufl g = return g
    | otherwise = do
        let b = take (zx g) (zbuf g) ++ (dropWhile isSpace $ dropWhile (not.isSpace) $ drop (zx g) (zbuf g))
        displine (drop (zx g) b) (zy g) (zx g) g{zbuf=b,zbufl=length b}

del_bword :: Global -> IO Global
del_bword g = glineup g >>= del_bword2
del_bword2 g
    | zx g <= 0 = return g
    | otherwise = do
        let l = reverse $ dropWhile (not.isSpace) $ dropWhile (isSpace) $ reverse $ take (zx g) (zbuf g)
            b = l ++ (drop (zx g) (zbuf g))
        displine b (zy g) 0 g{zx=length l, zbuf=b, zbufl=length b}
