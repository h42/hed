module Hterm (
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
    trequest,
    tsetbg,
    tscroll
) where

import Data.Array
import System.IO
import GetKB

data Hterm = Hterm {
    t_rows  :: Int
    ,t_cols  :: Int
    ,t_clrscr   :: String
    ,t_cup      :: String
    ,t_eol      :: String
    ,t_eos      :: String
    ,t_del_line :: String
    ,t_ins_line :: String
    ,t_clr_scr  :: String
    ,t_bg       :: String
    ,t_fg       :: String
    ,t_fg0      :: String
    ,t_fg1      :: String
    ,t_fg2      :: String
    ,t_fg3      :: String
    ,t_fg4      :: String
    ,t_fg5      :: String
    ,t_fg6      :: String
    ,t_fg7      :: String
    ,at_reverse :: String
    ,at_bold    :: String
    ,at_underline :: String
    ,at_blink   :: String
    ,at_exit    :: String
}

data Attribute = Norm | Black | Red | Green | Yellow | Blue
	     | Magenta | Cyan | White | Under | Bold | Reverse | Blink
	     deriving (Eq)

xterm = Hterm {
     t_rows = 24
    ,t_cols = 80
    ,t_clrscr = "\x1b[H\x1b[2J"
    ,t_cup  =   "\x1b[%d;%dH"
    ,t_eol      = "\x1b[K"
    ,t_eos      = "\x1b[J"
    ,t_del_line = "\x1b[M"
    ,t_ins_line = "\x1b[L"
    ,t_clr_scr  = "\x1b[H\x1b[2J"
    ,t_bg       = "\x1b[0;40m"
    ,t_fg       = "\x1b[m" -- exit att mode
    ,t_fg0      = "\x1b[1;30m"
    ,t_fg1      = "\x1b[0;31m"
    ,t_fg2      = "\x1b[0;32m"
    ,t_fg3      = "\x1b[0;33m"
    ,t_fg4      = "\x1b[0;34m"
    ,t_fg5      = "\x1b[0;35m"
    ,t_fg6      = "\x1b[0;36m"
    ,t_fg7      = "\x1b[0;30m"
    ,at_reverse = "\x1b[7m"
    ,at_bold    = "\x1b[1m"
    ,at_underline = "\x1b[4m"
    ,at_blink   = "\x1b[5m"
    ,at_exit    = "\x1b[m" -- exit att mode
    --,aaa        = listArray (1,4) ["aaa","bbb","ccc","ddd"]
    }

tout :: [Char] -> IO ()
tout = putStr

----------------------------------------
-- API
----------------------------------------
tattr :: Attribute -> IO ()
tattr Norm = tout $ t_fg xterm
tattr Black = tout $ t_fg0 xterm
tattr Red = tout $ t_fg1 xterm
tattr Green = tout $ t_fg2 xterm
tattr Yellow = tout $ t_fg3 xterm
tattr Blue = tout $ t_fg4 xterm
tattr Magenta = tout $ t_fg5 xterm
tattr Cyan  = tout $ t_fg6 xterm
tattr White = tout $ t_fg7 xterm
tattr _ = return ()

tclreol = tout $ t_eol xterm

tclreos = tout $ t_eos xterm

tclrscr = do
    tattr Norm
    tout $ t_clrscr xterm

tdelline = tout $ t_del_line xterm

tgoto :: Int -> Int -> IO ()
tgoto y x = tout $ "\x1b[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"

tinsline = tout $ t_ins_line xterm

tputblk s y x n = do
    if y>=0 then tgoto y x else return ()
    tout $ take n s

tputchar c y x = do
    if y>=0 then tgoto y x else return ()
    putChar c

tputs s y x = do
    if y>=0 then tgoto y x else return ()
    tout s

tsetbg x = tout $ t_bg xterm

tscroll x = do
    tgoto 0 0
    if x < 0 then tdelline else tinsline


----------------------------------------
--  GETKB
----------------------------------------
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
	_ -> trequest s x y z flag
  where backspace =
	    if x>0 then trequest (take (x-1) s ++ drop x s) (x-1) y z flag
	    else trequest s x y z flag



----------------------------------------
{-
main = do
    putStrLn $ (t_clrscr xterm) ++ "hey"
    let s1 = [ concat $ replicate 12 (show x) | x <- [1..10] ]
    mapM_ putStrLn s1
    getLine
    scroll $ -1
    scroll $ -1
    attr Red
    puts "I am red" 14 0
    attr Magenta
    puts "I am magenta" 15 0
    attr Norm
    hFlush stdout
    getLine
    puts "puts" 10 0
    hFlush stdout
    getLine
    clrscr
-}
