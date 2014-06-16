module Vi (
    cntl_v
) where

import System.IO

import Data.Char
import HTerm
import Global
import Func1
import Func0 (pline)
import Display (disppage,status,goto)

cntl_v g = if (zvi g) then getcmd0 0 "" g >>= cntl_v
           else return g{zvi=False}

getcmd0 :: Int -> String -> Global ->IO Global
getcmd0 cnt cmd g = do
    status g
    k <- getkb
    if k `elem` [KeyCntl 'v'] then
        return g{zvi=False, zmsg="leaving vi mode"}
    else do
        g' <- case k of
            KeyChar c ->
                if c=='0' then getcmd 0 "0" g{zmsg=""}
                else if isDigit c then getcmd0 (cnt*10 + (read [c] :: Int)) cmd g
                else getcmd cnt [c] g{zmsg=""}
            _ -> return g{zmsg="Unknown V function"}
        endcmd g'

getcmd cnt' cmd g = do
    let cnt = if cnt'>0 then cnt' else 1
    case cmd of
        "h" -> commander cnt left g
        "l" -> commander cnt right g
        "j" -> commander cnt down g
        "k" -> commander cnt up g
        "w" -> commander cnt word g
        "b" -> commander cnt (bword 1) g
        "0" -> commander cnt homer g
        "$" -> commander cnt ender g
        _ -> return g{zmsg="Unknown V function"}

commander 0 _ g = return g
commander cnt f g = do
    g' <- f g
    commander (cnt-1) f g'

endcmd g = do
    g' <- if zpager g then pline g >>= disppage
          else return g
    return g'{zpager=False}

