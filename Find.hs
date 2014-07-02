module Find (
     cntl_x
    ,hedChange
    ,hedFind
    ,initChange
    ,initFind
) where

import qualified Data.ByteString.Char8 as B
import Data.Char
import Glob
import HTerm
import Global
import Display
import Func0
import Func1

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

cntl_x' :: Char -> Global -> IO Global
cntl_x' c g = do
    case toLower c of
	'c' -> initChange g
	'f' -> initFind (if isLower c then 1 else 0) g
	'i' -> toggleIndent g
        'g' -> return g {zgo=(zx g,zy g)}
	'x' -> toggleCase g
	_   -> return g{zmsg="Unknown x function"}

----------------------
-- Find / Update
----------------------
initFind :: Int -> Global -> IO Global
initFind icase g = do
    s <- hed_request "Enter search string: " g
    if s == "" then return g
    else do
	let gl = compGlob s icase
	if (gl) == [] then return g{zmsg="Invalid search string",zfind=""}
	else do
	    putGlob gl
	    gline g{zfind=s} >>= hedFind

hedFind :: Global -> IO Global
hedFind g = pline g >>= hedFind1 >>= gline

hedFind1 :: Global -> IO Global
hedFind1 g' = do
    g <- gline g'
    gl <- getGlob
    if gl /= [] then do
	let (start,len) = searchGlob gl sx
	    sx = drop (zx g+1) (zbuf g)
	if start >= 0 then
	    return g{zx=zx g + 1 + start,zfindl=len}
        else hedFind2 gl (zy g + 1) (drop (zy g + 1) (zlist g)) g
    else return g{zmsg="Find not primed"}

hedFind2 :: Globs -> Int -> [B.ByteString] -> Global -> IO Global
hedFind2 _ _ [] g = return g{zmsg="Not Found"}
hedFind2 gl y (l:ls) g = do
	let (start,len) = searchGlob gl sx
            sx = glineByte l
	if start >= 0 then gline g{zx=start,zy=y,zfindl=len} >>= vupd
        else hedFind2 gl (y+1) ls g

initChange :: Global -> IO Global
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

hedChange :: Global -> IO Global
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

toggleCase :: Global -> IO Global
toggleCase g'  = glineup g' >>= toggleCase'  where
    toggleCase' g  
        | zx g >= zbufl g = return g
        | otherwise = do
            let c  = (zbuf g) !! (zx g)
            add_char (if isLower c then toUpper c else toLower c) g 

toggleIndent :: Global -> IO Global
toggleIndent g = return g{zindentnl=ind, zmsg=msg} where
    ind = not $ zindentnl g
    msg = "indent set to " ++ show ind
