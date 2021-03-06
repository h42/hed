import Control.Exception
import System.IO
import System.Environment
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Process

-- Leave import order alone
import Global
import HTerm
import Display
import Find
import Func0
import Func1
import Func2
import Vi
import File

main :: IO ()
main = do
    g' <- readHistory initGlobal
    args <- getArgs
    g <- if length args > 0 then loadfn (head args) g'
         else if fnHistory 0 g' /= "" then loadfn (fnHistory 0 g') g'
	 else newf g'
    disppage g

    -- Catch winsize change signal which causes getchar to return -1 which
    -- is trapped in getkb and returns KeyNone which updates screen.
    installHandler sigWINCH (Catch (return ())) Nothing

    bracket openkb closekb (\_ -> mainloop g)

mainloop :: Global -> IO ()
mainloop g = do
    if zpager g  then pline g >>= disppage  else return g
    status g -- does Hflush
    kc <- getkb
    mainloop' kc g{zmsg="", zglobals=updGlobals g, zpager=False}

mainloop' :: KeyCode -> Global -> IO ()
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
            KeyCntl 'o' -> loadf g >>= mainloop
            KeyCntl 'q' -> cleanup g
            KeyCntl 's' -> savef g >>= mainloop
            KeyCntl 't' -> top g >>= mainloop
            KeyCntl 'u' -> undo g >>= mainloop
            KeyCntl 'v' -> cntl_v g{zvi=True,zmsg="vi mode"} >>= mainloop
            KeyCntl 'x' -> cntl_x g >>= mainloop
            KeyCntl 'w' -> word g >>= mainloop
    
            KeyFunc 1   ->  help g >>=  mainloop
            KeyFunc 5   ->  hedFind g >>=  mainloop
            KeyFunc 6   ->  hedChange g >>=  mainloop
            KeyFunc 8   ->  swapf 2 g >>=  mainloop
            KeyFunc 9   ->  swapf 1 g >>=  mainloop
            KeyFunc 12  ->  cleanup g
    
            KeyAlt '1'  ->  help g >>=  mainloop
            KeyAlt '5'  ->  hedFind g >>=  mainloop
            KeyAlt '6'  ->  hedChange g >>=  mainloop
            KeyAlt '8'  ->  swapf 2 g >>=  mainloop
            KeyAlt '9'  ->  swapf 1 g >>=  mainloop
            KeyAlt 'o'  ->  loadf g >>= mainloop
            KeyAlt 'h'  ->  help g >>= mainloop
            KeyAlt 'm'  ->  savef g{zpager=True} >>= make >>= mainloop
            KeyAlt 'n'  ->  checkupd g >>= newf >>= addHistory
                                       >>= disppage >>= mainloop
            KeyAlt 'q'  -> cleanup g
            KeyAlt 'r'  -> getHistory g >>=  mainloop
            KeyAlt 's'  -> savef g >>= mainloop
            KeyAlt 't'  -> tester g >>= mainloop

            KeyNone     -> chk_winsize g >>= mainloop
            _           -> mainloop g

cleanup :: Global -> IO ()
cleanup g = do
    addHistory g >>= writeHistory >>= checkupd >>= clrscr
    return ()

help :: Global -> IO Global
help g = do
    tclrscr
    putStrLn $ "HED V1.0\n"
    putStrLn $ "Current file = " ++ zfn g
    hFlush stdout
    getChar
    disppage g

make :: Global -> IO Global
make g = do
    clrscr g
    hFlush stdout
    homefn <- homeFile "hed.out"
    --system $ "make &> " ++ homefn
    system $ "make 2>&1 | tee " ++ homefn
    loadfn homefn g

tester :: Global -> IO Global
tester g = do
    del_bword g


