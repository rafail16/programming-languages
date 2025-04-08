import Data.Maybe (isNothing)
import qualified Data.Map.Lazy as Maps               
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

getMultipleLines :: Int -> Int -> Int -> IO ([(Int, [(Int, Int)])], Int)
getMultipleLines n k sum
    | n <= 0 = return ([], 0)
    | otherwise = do
        [b, parent] <- readText
        (xs, sum') <- getMultipleLines (n-1) k sum
        return (((parent,[(b, (k-n))]):xs), sum' + b)
        
readText = map parse . T.words <$> T.getLine
  where parse s = let Right (n, _) = T.decimal s in n

minim:: Int -> Int -> Int -> Int -> (Int, Int)
minim x1 b1 x2 b2 = 
  if x1 > x2 then (x2, b2)
  else if x1 < x2 then (x1, b1)
  else if b1 < b2 then (x1, b1)
  else (x2, b2)

result3 :: (a, b, d) -> d
result3 (_, _, x) = x

mayb:: Maybe [(Int, Int)] -> (Int,Int)
mayb Nothing = (0,0)
mayb (Just []) = (0,0)
mayb (Just (x:_)) = x

wiring :: Maps.Map Int [(Int, Int)] -> Int -> Int -> Int -> Int --((Tree (Int, Int)), Int, Int, Int)
wiring myMap number minS minN = result3 (aux myMap t number minS minN)
  where t = mayb (Maps.lookup 0 myMap)
        aux :: Maps.Map Int [(Int, Int)] -> (Int, Int) -> Int -> Int -> Int -> (Int, Int, Int)
        aux myMap (k, x) number min1 min2 = (sum, minSum', minNode')
          where (res', sum', minSum, minNode) = auxs myMap 0 (Maps.lookup x myMap) number min1 min2
                sum = sum' + k
                res = max res' (number-sum)
                (minSum', minNode') = minim minSum minNode res x
        auxs :: Maps.Map Int [(Int, Int)] -> Int -> Maybe [(Int, Int)] -> Int -> Int -> Int -> (Int, Int, Int, Int)
        auxs _ k Nothing _ minSum minNode = (k, 0, minSum, minNode)
        auxs _ k (Just []) _ minSum minNode = (k, 0, minSum, minNode)
        auxs myMap k (Just (t : ts)) number minSum minNode = (res, sum, minSum', minNode')
          where (sum1, minS1, minN1) = aux myMap t number minSum minNode
                (res', sum2, minS2, minN2) = auxs myMap k (Just ts) number minSum minNode
                sum = sum1 + sum2
                res = max sum1 res'
                (minSum', minNode') = minim minS2 minN2 minS1 minN1


main :: IO ()
main = do
    line <- getLine
    let numLines = read line :: Int
    (inputs, sum) <- getMultipleLines numLines (numLines+1) 0
    let mapped = Maps.fromListWith (++) inputs
    let new = wiring mapped sum (maxBound::Int) (numLines+1)
    print new