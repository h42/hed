module HTerm (
    Attribute (..),
    tattr,
    tclreol,
    tclreos,
    tclrscr,
    tdelline,
    tgoto,
    tinsline,
    tputblk,
    tputchar,
    tputs,
    tsetbg,
    tscroll,

    KeyCode (..),
    closekb,
    getkb,
    openkb,
    trequest

) where

import Foreign.C.Types
import Data.Char
import System.IO
import HTermDefs
import System.Posix.IO
import System.Posix.Terminal

data Attribute = Norm | Black | Red | Green | Yellow | Blue
	     | Magenta | Cyan | White | Under | Bold | Reverse | Blink
	     deriving (Eq)

tout :: [Char] -> IO ()
tout = putStr

----------------------------------------
-- API
----------------------------------------
tattr :: Attribute -> IO ()
tattr Norm = tout $ t_fg hterm
tattr Black = tout $ t_fg0 hterm
tattr Red = tout $ t_fg1 hterm
tattr Green = tout $ t_fg2 hterm
tattr Yellow = tout $ t_fg3 hterm
tattr Blue = tout $ t_fg4 hterm
tattr Magenta = tout $ t_fg5 hterm
tattr Cyan  = tout $ t_fg6 hterm
tattr White = tout $ t_fg7 hterm
tattr _ = return ()

tclreol = tout $ t_eol hterm

tclreos = tout $ t_eos hterm

tclrscr = do
    tattr Norm
    tout $ t_clrscr hterm

tdelline = tout $ t_del_line hterm

tgoto :: Int -> Int -> IO ()
tgoto y x = tout $ "\x1b[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"

tinsline = tout $ t_ins_line hterm

tputblk s y x n = do
    if y>=0 then tgoto y x else return ()
    tout $ take n s

tputchar c y x = do
    if y>=0 then tgoto y x else return ()
    putChar c

tputs s y x = do
    if y>=0 then tgoto y x else return ()
    tout s

tsetbg = tout $ t_bg hterm

tscroll x = do
    tgoto 0 0
    if x < 0 then tdelline else tinsline


----------------------------------------
--  GETKB
----------------------------------------
type Kitem = (String,KeyCode)
type Kdict = [Kitem]

foreign import ccall "getchar"  getchar :: IO CInt

getkb :: IO KeyCode
getkb = do
    -- use ffi to prevent exception for alt chars (>=128) on
    -- archlinux xterm
    ci<-fmap fromIntegral getchar --getchar is c getchar
    if ci == -1 then return KeyNone -- SIGWINCH could kill getchar
    else if ci > 127 then return $ KeyAlt $ chr (ci - 128 )
    else do
	let c = chr ci
	case c of
	    '\x1B' -> getkb2 fkey "\x1b"
	    _    -> return $ mkchar c

getkb2 fs sx = do
    ci <-fmap fromIntegral getchar  -- c getchar
    -- ignore alt chars here - should not happen
    if ci >= 128 then getkb2 fs sx
    else do
        let c = chr ci
        let sx' = sx++[c]
        let (fs',match) = checkFkey fs sx' []
        case (fs',match) of
            ([],_)  ->  do
                if length sx' == 2  then return $ KeyAlt c
                else return KeyNone
            (_,KeyNone)  ->  getkb2 fs' (sx')
            _  ->  return match
    
data Checkrc = GOODRC | BADRC | MAYBERC
checkFkey :: Kdict -> String -> Kdict -> (Kdict,KeyCode)
checkFkey [] _ newdict = (newdict,KeyNone)
checkFkey (k:ks) xs newdict = case rc of
        GOODRC ->  ((k:newdict),snd k)
        BADRC -> checkFkey ks xs newdict
        MAYBERC -> checkFkey ks xs (newdict++[k])
    where rc = check2 (fst k) xs

-- we should not need "check2 [] _" as xs grows from [x] in call from checkFkey
check2 :: String -> String -> Checkrc
check2 [] [] = GOODRC
check2 _ [] = MAYBERC
check2 (k:ks) (x:xs) = if k == x then check2 ks xs else BADRC
check2 _ _ = undefined

mkchar c
    | oc == 127 = KeyBs
    | c >= ' ' = KeyChar c
    | otherwise = KeyCntl c' where
	oc = ord c
	c' = if oc>=1 && oc<=26 then chr (oc + ord 'a' -1) else ' '

openkb = do
    hSetBuffering stdin NoBuffering
    tc <- getTerminalAttributes stdInput
    -- let tc' = (withoutMode tc KeyboardInterrupts)
    -- let tc'' = (withoutMode tc' EnableEcho)
    let tc' = (withoutMode (withoutMode tc EnableEcho) KeyboardInterrupts)
    let tc'' = (withoutCC (withoutCC tc' Stop) Start)
    setTerminalAttributes stdInput tc'' Immediately
    return tc

closekb tc = setTerminalAttributes stdInput tc Immediately


trequest :: String -> Int -> Int -> Int -> Int -> IO String
trequest s x y z flag = do
    tgoto y z
    putStr s
    tclreol
    tgoto y (z+x)
    hFlush stdout
    kc <- getkb
    case kc of
	KeyBs -> backspace
	KeyChar c -> do
	    if flag==1 then return [c]
	    else trequest (take x s ++ [c] ++ drop x s) (x+1) y z flag
	KeyCntl 'j' -> return s
	KeyCntl 'h' -> backspace
	KeyLeft -> trequest s (if x>0 then x-1 else x) y z flag
	KeyRight -> trequest s (if x<length s then x+1 else x) y z flag
	KeyHome -> trequest s 0 y z flag
	KeyEnd -> trequest s (length s) y z flag
        -- if cntl char then return just char for go function marking
        KeyCntl c -> return [c]
	_ -> trequest s x y z flag
  where backspace =
	    if x>0 then trequest (take (x-1) s ++ drop x s) (x-1) y z flag
	    else trequest s x y z flag

