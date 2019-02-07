import System.IO
import Result

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
    putStrLn (resultString 1 1 547)
    putStrLn . show $ sums
    hClose inputFile
