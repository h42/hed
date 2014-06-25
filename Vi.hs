module Vi (
    cntl_v
) where

import System.IO

import Data.Char
import HTerm
import Global
import Func1
import Func0 (pline, updGlobals)
import Display (disppage,status,goto)

cntl_v g | zvi g = getcmd 0 g >>= cntl_v
         | otherwise = return g

getcmd :: Int -> Global ->IO Global
getcmd cnt g = do
    status g
    k <- getkb
    if k == KeyCntl 'v' then return g{zvi=False, zmsg=leaving}
    else if k == KeyCntl 'u' then  undo g >>= endcmd
    else do
        g' <- case k of
            KeyChar c ->
                if c=='0' then execCmd 0 '0' g{zmsg=""}
                else if isDigit c then getcmd (cnt*10 + (read [c] :: Int)) g
                else execCmd cnt c g{zmsg=""}
            _ -> return g{zmsg="Unknown V function"}
        endcmd g'

execCmd cnt' cmd g = do
    let cnt = if cnt'>0 then cnt' else 1
    case cmd of
        'a' -> return g{zins=True,zvi=False,zx=(zx g)+1, zglobals=updGlobals g}
        'd' -> delcmd cnt g
        'h' -> commander cnt left g
        'i' -> return g{zins=True,zvi=False,zmsg=leaving, zglobals=updGlobals g}
        'l' -> commander cnt right g
        'j' -> commander cnt down g
        'k' -> commander cnt up g
        'w' -> commander cnt word g
        'b' -> commander cnt (bword 1) g
        '0' -> commander 1 homer g
        '$' -> commander 1 ender g
        _   -> return g{zmsg="Unknown V function"}

commander 0 _ g = return g{zglobals=updGlobals g}
commander cnt f g = do
    g' <- f g
    commander (cnt-1) f g'

delcmd cnt g = do
    k <- getkb
    case k of
        KeyChar 'b' -> commander cnt del_bword g
        KeyChar 'd' -> commander (minlines) del_line g
        KeyChar 'w' -> commander cnt del_word g
        _ -> return g{zmsg="Unknown delete subcommand"}
  where minlines = min cnt (zlines g - zy g)

endcmd g = do
    g' <- if zpager g then pline g >>= disppage
          else return g
    return g'{zpager=False}

leaving = "Leaving VI"

