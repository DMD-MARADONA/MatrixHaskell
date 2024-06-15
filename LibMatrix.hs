module LibMatrix (
    Matrix,
    showMatrix,
    showMatrik,
    makeMatrix,
    identityMatrix,
    addMatrix,
    subMatrix,
    mulMatrix,
    transposeMatrix,
    scalarMulMatrix
) where
import Data.List

type Matrix = [[Int]]

identityMatrix :: Int -> [[Int]]
identityMatrix n = [[if x == y then 1 else 0 | x <- [1..n]] | y <- [1..n]]

addMatrix :: Matrix -> Matrix -> Matrix
addMatrix = zipWith (zipWith (+))

subMatrix :: Matrix -> Matrix -> Matrix
subMatrix = zipWith (zipWith (-))

transposeMatrix :: Matrix -> Matrix
transposeMatrix b = transpose b

scalarMulMatrix :: Int -> Matrix -> Matrix
scalarMulMatrix scalar = map (map (* scalar))

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix ma mb = [[ sum $ zipWith (*) a b | b <- (transpose mb)] | a <- ma]

showMatrix :: Matrix -> IO()
showMatrix = mapM_ (putStrLn . unwords . map show)

showMatrik :: [[Int]] -> IO()
showMatrik x = putStrLn (show x)

makeMatrix :: Int -> Int -> [Int] -> [[Int]]
makeMatrix rows cols _elements
    | length _elements /= rows * cols = error "Unexpected Number of elements"
    | otherwise = take rows $ map (take cols) $ iterate (drop cols) _elements