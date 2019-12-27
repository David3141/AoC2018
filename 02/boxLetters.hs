import System.IO
import Data.List

main = withFile "input.txt" ReadMode $ \handle -> do
    ids <- words <$> hGetContents handle

    -- part one
    putStrLn "The checksum is:"
    print $ checksum ids
    putStrLn ""

    -- part two
    let similarIds = findSimilarIds ids
    putStrLn "The two similar IDs are: "
    print similarIds

    putStrLn "Omitting the one unqequal letter leaves us with:"
    print $ omitUnequalChars similarIds


checksum :: [String] -> Int
checksum ids =
    let
        idsWithGroupedLetters = map (group . sort) ids

        containsExactly2 = length
            . filter (any ((==2) . length))
            $ idsWithGroupedLetters

        containsExactly3 = length
            . filter (any ((==3) . length))
            $ idsWithGroupedLetters
    in
        containsExactly2 * containsExactly3


numOfDifferingChars :: String -> String -> Int
numOfDifferingChars text1 text2 = countDiffering text1 text2 0
    where
        countDiffering _ "" nDiffering = nDiffering
        countDiffering "" _ nDiffering = nDiffering
        countDiffering (x:xs) (y:ys) nDiffering = countDiffering xs ys
            (if x /= y then nDiffering + 1 else nDiffering)


omitUnequalChars :: (String, String) -> String
omitUnequalChars (text1, text2) = omitUnequal (text1, text2) ""
    where
        omitUnequal ("", _) result = result
        omitUnequal (_, "") result = result
        omitUnequal (x:xs, y:ys) result =
            omitUnequal (xs, ys) (if x == y then result ++ [x] else result)


findSimilarIds :: [String] -> (String, String)
findSimilarIds ids = head
    [ (ids!!x, ids!!y)
    | x <- [0 .. length ids - 1]
    , y <- [x .. length ids - 1]
    , numOfDifferingChars (ids!!x) (ids!!y) == 1
    ]
