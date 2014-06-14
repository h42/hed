module HTermDefs (
    fkey
    ,Hterm (..)
    ,hterm
    ,KeyCode (..)
    ) where

import System.Environment
import System.IO.Unsafe

(fkey,hterm) = case maybeTerm of
    Just "linux" -> (linux_key,linux_term)
    _     -> (xterm_key,xterm_term)

maybeTerm = unsafePerformIO $ lookupEnv "TERM"


data KeyCode = KeyChar Char | KeyFunc Int | KeyIns | KeyDel | KeyHome | KeyEnd
	      | KeyPgup | KeyPgdown | KeyBtab | KeyUp | KeyDown
	      | KeyRight | KeyLeft | KeyBs | KeyCntl Char | KeyAlt Char
	      | KeyNone
	      deriving (Show,Eq)

xterm_key = [
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

linux_key = [
    ("\x1b[[A",KeyFunc 1)    -- F1
    ,("\x1b[[B",KeyFunc 2)   -- F2
    ,("\x1b[[C",KeyFunc 3)   -- F3
    ,("\x1b[[D",KeyFunc 4)   -- F4
    ,("\x1b[[E",KeyFunc 5) -- F5
    ,("\x1b[17~",KeyFunc 6) -- F6
    ,("\x1b[18~",KeyFunc 7) -- F7
    ,("\x1b[19~",KeyFunc 8) -- F8
    ,("\x1b[20~",KeyFunc 9) -- F9
    ,("\x1b[21~",KeyFunc 10) -- F10
    ,("\x1b[23~",KeyFunc 11) -- F11
    ,("\x1b[24~",KeyFunc 12) -- F12
    ,("\x1b[2~",KeyIns)      -- ins
    ,("\x1b[3~",KeyDel)      -- del
    ,("\x1b[1~",KeyHome)      -- home
    ,("\x1b[4~",KeyEnd)       -- end
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

xterm_term = Hterm {
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
  }

linux_term = Hterm {
     t_rows = 25
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
  }

