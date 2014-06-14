import Data.Char
import Control.Exception
import System.IO
import System.Environment
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Process
import System.Timeout
import Debug.Trace

-- Leave import order alone
import Global
import HTerm
import Display
import Func0
import Func1
import Func2
import Vi
import File

main = do
    g' <- readHistory initGlobal
    args <- getArgs
    g <- if length args > 0 then
	     loadfn (head args) g'
	 else if fnHistory 0 g' /= "" then
	     loadfn (fnHistory 0 g') g'
	 else newf g'
    disppage g

    -- Catch winsize change signal which causes getchar to return -1 which
    -- is trapped in getkb and returns KeyNone which updates screen.
    installHandler sigWINCH (Catch (return ())) Nothing

    bracket openkb closekb (\_ -> mainloop g)

mainloop :: Global -> IO ()
mainloop g = do
    if zpager g then pline g >>= disppage else return g
    status g
    goto g
    hFlush stdout
    kc <- getkb
    mainloop' kc g{zmsg="",zglobals=take 32 (g:zglobals g),zpager=False}


undo g
    | length (zglobals g) < 2 = return g
    | otherwise = return (head $ tail (zglobals g)){zpager=True}

mainloop' kc g = do
    --kc <- getkb
    if zro g then do
        case kc of
            KeyEnd  -> ender g >>= mainloop
            KeyHome -> homer g >>= mainloop
            KeyRight -> right g >>= mainloop
            KeyLeft -> left g >>= mainloop
            KeyUp   -> up g >>= mainloop
            KeyDown -> down g >>= mainloop
            KeyPgup -> pgup g >>= mainloop
            KeyPgdown -> pgdown g >>= mainloop
            KeyCntl 'b' -> bottom g >>= mainloop
            KeyCntl 't' -> top g >>= mainloop
            KeyFunc 8   ->  swapf 2 g >>=  mainloop
            KeyFunc 9   ->  swapf 1 g >>=  mainloop
            KeyFunc 10  ->  loadf g >>= mainloop
            KeyFunc 12  ->  cleanup g
            KeyAlt 'f'  ->  loadf g >>= mainloop
            KeyAlt 'h'  ->  loadf g >>= mainloop
            KeyAlt 'n'  ->  checkupd g >>= newf >>= addHistory
                                       >>= disppage >>= mainloop
            KeyAlt 'q'  -> cleanup g

            KeyNone     -> chk_winsize g >>= mainloop
            _ -> mainloop g{zmsg="File is read only - not allowed"}

    else do
        case kc of
            KeyChar c -> mainloop =<<
                if zins g  then ins_char c g  else add_char c g
            KeyEnd  -> ender g >>= mainloop
            KeyHome -> homer g >>= mainloop
            KeyDel  -> del_char g >>= mainloop
            KeyRight -> right g >>= mainloop
            KeyLeft -> left g >>= mainloop
            KeyUp   -> up g >>= mainloop
            KeyDown -> down g >>= mainloop
            KeyIns  -> ins_key g >>= mainloop
            KeyPgup -> pgup g >>= mainloop
            KeyPgdown -> pgdown g >>= mainloop
            KeyBtab -> btab_char g >>= mainloop
            KeyBs   -> bs_char g >>= mainloop
    
            KeyCntl 'a' -> indent g >>= mainloop
            KeyCntl 'b' -> bottom g >>= mainloop
            KeyCntl 'd' -> del_line g >>= mainloop
            KeyCntl 'e' -> erase_eol g >>= mainloop
            KeyCntl 'f' -> scroll (1) g >>= mainloop
            KeyCntl 'r' -> scroll (-1) g >>= mainloop
            KeyCntl 'g' -> go g >>= mainloop
            KeyCntl 'h' -> bs_char g >>= mainloop
            KeyCntl 'i' -> tab_char g >>= mainloop
            KeyCntl 'j' -> enter g >>= mainloop
            KeyCntl 'k' -> cntl_k g >>= mainloop
            KeyCntl 'n' -> ins_line g >>= mainloop
            KeyCntl 'q' -> cleanup g
            KeyCntl 't' -> top g >>= mainloop
            KeyCntl 'u' -> undo g >>= mainloop
            KeyCntl 'v' -> cntl_v g >>= mainloop
            KeyCntl 'x' -> cntl_x g >>= mainloop
    
            KeyFunc 5   ->  hedFind g >>=  mainloop
            KeyFunc 6   ->  hedChange g >>=  mainloop
            KeyFunc 8   ->  swapf 2 g >>=  mainloop
            KeyFunc 9   ->  swapf 1 g >>=  mainloop
            KeyFunc 10  ->  loadf g >>= mainloop
            --KeyFunc 11  ->  savef g >>= mainloop
            KeyFunc 12  ->  cleanup g
    
            KeyAlt 'o'  ->  loadf g >>= mainloop
            KeyAlt 'h'  ->  help g >>= mainloop
            KeyAlt 'm'  ->  savef g{zpager=True} >>= make >>= mainloop
            KeyAlt 'n'  ->  checkupd g >>= newf >>= addHistory
                                       >>= disppage >>= mainloop
            KeyAlt 'q'  -> cleanup g
            KeyAlt 'r'  -> getHistory g >>=  mainloop
            KeyAlt 's'  -> savef g >>= mainloop
            KeyAlt 't'  -> tester g >>= mainloop
            KeyAlt 'w'  -> swapf 1 g >>= mainloop

            KeyNone     -> chk_winsize g >>= mainloop
            _           -> mainloop g

cleanup :: Global -> IO ()
cleanup g = do
    addHistory g >>= writeHistory >>= checkupd >>= clrscr
    return ()

help g = do
    tclrscr
    putStr $ zfn g
    hFlush stdout
    getChar
    disppage g

make g = do
    clrscr g
    hFlush stdout
    homefn <- homeFile "hed.out"
    --system $ "make &> " ++ homefn
    system $ "make 2>&1 | tee " ++ homefn
    loadfn homefn g

tester g = do
    {-
    fn <- getHistoryFn
    clrscr g
    putStrLn fn
    getkb
    -}
    h <- homeFile "xxx"
    return $ g{zmsg=h}


