import GetKB
import Data.Char

-- Test GetKB

main = do
    putStrLn "Enter 'q' to quit"
    tc <- openkb
    main2
    closekb tc

main2 = do
    kc <- getkb
    case kc of
	KeyChar c -> putStrLn $ "Char " ++ (show $ ord c)
	KeyCntl c -> putStrLn $ "Cntl " ++ (show $ ord c)
	_ -> print kc
    if kc /= (KeyChar 'q') then main else return ()
