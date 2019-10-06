import System.IO
import qualified Data.Set as Set

main = withFile "input.txt" ReadMode $ \handle -> do
    ints <- map readSignedInt . words <$> hGetContents handle

    -- part one
    let endFreq = foldl (+) 0 ints
    print endFreq

    -- part two
    let firstDup = findFirstDupFreq (cycle ints)
    print firstDup


readSignedInt :: String -> Int
readSignedInt ('+':numberStr) = read numberStr
readSignedInt numberStr = read numberStr


findFirstDupFreq :: [Int] -> Int
findFirstDupFreq ints =
    f 0 (Set.singleton 0) ints

    where
    f :: Int -> Set.Set Int -> [Int] -> Int
    f lastSum sums (x:xs) =
        let currentSum = x + lastSum

        in
        if Set.member currentSum sums
            then currentSum
            else f currentSum (Set.insert currentSum sums) xs
