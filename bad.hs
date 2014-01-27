import qualified  Data.ByteString.Char8 as B

main = B.writeFile "temp" $ B.pack "abcd\x01\x02"

