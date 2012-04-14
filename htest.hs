import Glob

main = do
    let gl = compGlob "ron" 0
    if gl == []
	then putStrLn "Bad glob"
	else do
	    print $ searchGlob gl "environment"

    --let x=dglob "abcde" ['a'..'z']
    --print x
    -- dog cat mouse
