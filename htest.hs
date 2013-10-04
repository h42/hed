--import Data.List
import Control.Monad
import Glob

testdata = unlines $ map td2 [1..25]

td2 i= "<" ++ show i ++ ">" ++ replicate 50 'x'

main = do
    writeFile "temp3" testdata
    let gl = compGlob "ron" 0
    if gl == []
	then putStrLn "Bad glob"
	else do
	    print $ searchGlob gl "environment"

    --let x=dglob "abcde" ['a'..'z']
    --print x
    -- dog cat mouse
