import System.IO

main = withFile "input.txt" ReadMode $ \handle -> do
    ints <- map readSignedInt . words <$> hGetContents handle

    -- part one
    let endFreq = foldl (+) 0 ints
    print endFreq

    -- part two
    let firstDup = findFirstDupFreq [0] (cycle ints)
    print firstDup


readSignedInt :: String -> Int
readSignedInt ('+':numberStr) = read numberStr
readSignedInt numberStr = read numberStr


findFirstDupFreq :: [Int] -> [Int] -> Int
findFirstDupFreq acc (x:xs) =
    let lastSum = head acc
        currentSum = x + lastSum
    in
    if currentSum `elem` acc
        then currentSum
        else findFirstDupFreq (currentSum:acc) xs
