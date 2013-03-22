module GetKB (
    KeyCode (..),
    getkb,
    openkb,
    closekb
    ) where

import Data.Char
import System.IO
import System.IO.Unsafe
import System.Posix.IO
import System.Posix.Terminal
import System.Timeout
import Foreign.C.Types

foreign import ccall "getchar"  getchar :: IO CInt

data KeyCode = KeyChar Char | KeyFunc Int | KeyIns | KeyDel | KeyHome | KeyEnd
	      | KeyPgup | KeyPgdown | KeyBtab | KeyUp | KeyDown
	      | KeyRight | KeyLeft | KeyBs | KeyCntl Char | KeyAlt Char
	      | KeyNone
	      deriving (Show,Eq)

type Kitem = (String,KeyCode)
type Kdict = [Kitem]

fkey = [
    ("\x1bOP",KeyFunc 1)    -- F1
    ,("\x1bOQ",KeyFunc 2)   -- F2
    ,("\x1bOR",KeyFunc 3)   -- F3
    ,("\x1bOS",KeyFunc 4)   -- F4
    ,("\x1b[15~",KeyFunc 5) -- F5
    ,("\x1b[17~",KeyFunc 6) -- F6
    ,("\x1b[18~",KeyFunc 7) -- F7
    ,("\x1b[19~",KeyFunc 8) -- F8
    ,("\x1b[20~",KeyFunc 9) -- F9
    ,("\x1b[21~",KeyFunc 10) -- F10
    ,("\x1b[23~",KeyFunc 11) -- F11
    ,("\x1b[24~",KeyFunc 12) -- F12
    ,("\x1b[2~",KeyIns)      -- ins
    ,("\x1b[3~",KeyDel)      -- del
    ,("\x1b[H",KeyHome)      -- home
    ,("\x1b[F",KeyEnd)       -- end
    ,("\x1b[5~",KeyPgup)     -- page up
    ,("\x1b[6~",KeyPgdown)   -- page down
    ,("\x1b[Z",KeyBtab)      -- btab
    ,("\x1b[A",KeyUp)        -- up
    ,("\x1b[B",KeyDown)      -- down
    ,("\x1b[C",KeyRight)     -- right
    ,("\x1b[D",KeyLeft)      -- left
    ,("\x1bOH",KeyHome)      -- home2
    ,("\x1bOF",KeyEnd)       -- end2
 ]


-- we should not need "check2 [] _" as xs grows from [x] in call from checkFkey
check2 [] [] = 1
check2 _ [] = 0
check2 (k:ks) (x:xs) = if k == x then check2 ks xs else -1

checkFkey :: Kdict -> String -> Kdict -> (Kdict,KeyCode)
checkFkey [] xs newdict = (newdict,KeyNone)
checkFkey (k:ks) xs newdict = case rc of
	1 ->  ((k:newdict),snd k)
	-1 -> checkFkey ks xs newdict
	0 -> checkFkey ks xs (newdict++[k])
    where rc = check2 (fst k) xs

getkb2 fs sx = do
    ci <-fmap fromIntegral getchar  -- c getchar
    if ci >= 128 then getkb2 fs sx -- ignore alt chars here - should not happen
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

clearout = do
    getchar
    clearout

mkchar c
    | oc == 127 = KeyBs
    | c >= ' ' = KeyChar c
    | otherwise = KeyCntl c' where
	oc = ord c
	c' = if oc>=1 && oc<=26 then chr (oc + ord 'a' -1) else ' '

getkb = do
    -- use ffi to prevent exception for alt chars (>=128) on
    -- archlinux xterm
    ci<-fmap fromIntegral getchar --getchar is c getchar
    if ci > 127 then return $ KeyAlt $ chr (ci - 128 )
    else do
	let c = chr ci
	case c of
	    '\x1B' -> getkb2 fkey "\x1b"
	    _    -> return $ mkchar c

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

------------------------------------------------
{-
mainloop = do
    kc <- getkb
    case kc of
	KeyChar 'q' -> putStrLn "all done"
	KeyChar c -> do
		putStrLn $ "got one - " ++ show c ++ " " ++ (show $ ord c)
		mainloop
	_ -> do
	    print kc
	    mainloop

main = do
    bracket openkb closekb (\_ -> mainloop)
-}
