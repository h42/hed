import System.Environment
import Data.List

main = do
    args <- getArgs
    case args of
	[fn] -> doit fn
	[fn,"fix"] -> fixit fn
	_ -> print "Usage: chktabs <fn>"

doit fn = do
    ls <- fmap lines $ readFile fn
    print $ filter (\(x,_) -> x) (zip (map (any (=='\t')) ls) [1..])

fixit fn = do
    ls <- fmap lines $ readFile fn
    let ls' = map (tabexpand 0) ls
    writeFile "temp" (unlines ls')
    print "file written to <temp>"

tabexpand :: Int -> String -> String
tabexpand i [] = []
tabexpand i (x:xs)
    | x == '\x09' =  replicate n ' ' ++ tabexpand (i+n) xs
    | otherwise = x : (tabexpand (i+1) xs)
  where n = (8 - mod i 8)
