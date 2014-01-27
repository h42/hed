import System.IO
import Data.Char
import Data.List

adjust otab ntab gs = tabcomp $ expand gs 0  where
    expand [] _ = []
    expand (x:xs) i
	| x /= '\t' = x : expand xs (i+1)
	| otherwise = replicate n ' ' ++ expand xs (i+n)
      where n = otab - mod i otab

    tabcomp :: String -> String
    tabcomp xs
	| isPrefixOf spaces xs  = '\x09' :  (tabcomp $ drop ntab xs)
	| otherwise = xs

    spaces = replicate ntab ' '

main = do
    --s <- readFile "temp.dat"
    s <- getContents
    let s2 = unlines $
	  map (adjust 4 8) (lines s)
    --putStr s
    putStr s2


