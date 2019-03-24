import System.IO
import qualified Data.Set as Set
-- import Result

rmPlus :: String -> String
rmPlus str
  | head str == '+' = tail str
  | otherwise = str

toInt :: [String] -> [Int]
toInt ls = map (readInt . rmPlus) ls
    where readInt = read :: String -> Int

probA :: IO ()
probA = do
    inputFile <- openFile "input" ReadMode
    content <- hGetContents inputFile
    let lin = lines content
        nums = toInt lin
        sums = sum nums
    -- putStrLn (resultString 1 1 547)
    putStrLn . show $ sums
    hClose inputFile

findDuplicateFreq (x:xs) set
    | Set.member x set = x
    | otherwise = findDuplicateFreq xs (Set.insert x set)

probB :: IO ()
probB = do
    inputFile <- openFile "input" ReadMode
    content <- hGetContents inputFile
    let lin = lines content
        nums = toInt lin
        dupFreq = findDuplicateFreq (scanl (+) 0 (cycle nums)) Set.empty
    putStrLn "Problem B"
    putStrLn . show $ dupFreq
