import System.IO
import HTerm
import Data.Char
import Foreign
import Foreign.C.Types
import Text.Printf

foreign import ccall "getchar"  getchar :: IO CInt

-- Test GetKB

main = do
    putStrLn "Enter 'q' to quit"
    tc <- openkb
    main3
    closekb tc

main3 = do
    ci<-fmap fromIntegral getchar
    if ci > 128 || ci<0 || chr ci /='q' then do
	printf "%x %d\n" ci ci
	main3
    else do
	let c = chr ci
	case c of
	    'q' -> return ()
	    _ -> do
		putChar c
		hFlush stdout
		main3

main2 = do
    kc <- getkb
    case kc of
	KeyChar c -> putStrLn $ "Char " ++ (show $ ord c)
	KeyCntl c -> putStrLn $ "Cntl " ++ (show $ ord c)
	_ -> print kc
    if kc /= (KeyChar 'q') then main else return ()
