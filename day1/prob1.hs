import System.IO

rmPlus str
  | head str == '+' = tail str
  | otherwise = str

toInt ls = map (readInt . rmPlus) ls
    where readInt = read :: String -> Int

main = do
    inputFile <- openFile "input" ReadMode
    content <- hGetContents inputFile
    let lin = lines content
        nums = toInt lin
        sums = sum nums
    putStr . show $ sums
    hClose inputFile
