import Data.Char

data Array e = Empty | Node [e] deriving(Eq, Show, Ord)
data PairOfData e = None | Code ([e], [(e -> e -> e)])
type Matrix e = [Array e]

generateArray :: Int -> Int -> Array Int -> Array Int
generateArray 0 a x = x
generateArray n a (Node x) = generateArray (n-1) a (Node (a:x))

buildMatrix :: Int -> Array Int -> Matrix Int -> Matrix Int
buildMatrix 0 a y = reverse y
buildMatrix n a y = buildMatrix (n-1) a (a:y)


get :: Matrix Int -> (Int, Int) -> Int
get ((Node (y:ys)):xs) (0,0) = y
get ((Node (y:ys)):xs) (0,b) = get ((Node ys):xs) (0,b-1)
get (x:xs) (a, b) = get xs (a-1,b)

set :: Matrix Int -> (Int, Int) -> Int -> Matrix Int
set ((Node x):xs) (0, b) n = (Node (setInArray x (b,n)):xs)
set (x:xs) (a, b) n = (x:(set xs (a-1, b) n))

setInArray :: [Int] -> (Int,Int) -> [Int]
setInArray (x:xs) (0, n) = (n: xs)
setInArray (x:xs) (b, n) = (x:(setInArray xs (b-1, n)))

parseString :: String -> Int -> (PairOfData Int, [String]) -> (PairOfData Int, [String])
parseString (x:xs) accum ((Code (a, b)), s) | isDigit x = parseString xs (10*accum + digitToInt x) ((Code (a, b)), s)
                                            | otherwise = parseString xs 0 ((Code ((accum:a), (c:b))), (z:s))
                                        where
                                            (c,z) = checkOperation x
parseString _ accum ((Code (a, b)),s) = ((Code (reverse (accum:a), reverse b)), reverse s)                                    
checkOperation :: Char -> ((Int -> Int -> Int), String)
checkOperation x | x == '-' = ((-), "-")
                 | x == '+' = ((+), "+")
                 | otherwise = ((*), "*")                    
                                       
addPars :: String -> String
addPars s = (restore (0, ((length x) - 1)) 0 (y, operS) maxM (maxM, minM)) -- ++ " = " ++ (intToString (get maxM (0, ((length x) - 1))) "")
    where 
        ((Code (x, y)), operS) = parseString s 0 ((Code ([],[])), [])
        (maxM, minM) = dynamics (length x, 0, 1) y ((doMatrix x (-999)),(doMatrix x 999))

doMatrix :: [Int] -> Int -> Matrix Int
doMatrix a b = initialization a 0 (buildMatrix n (generateArray n b (Node [])) [])
    where
        n = length a
initialization :: [Int] -> Int -> Matrix Int -> Matrix Int
initialization (x:xs) i b = initialization xs (i+1) (set b (i, i) x)
initialization _ i b = b


dynamics :: (Int,Int,Int) -> [(Int -> Int -> Int)] -> (Matrix Int,Matrix Int) -> (Matrix Int, Matrix Int)
dynamics (n, step, k) oper (maxMatrix,minMatrix) | k < n = dynamics (n,step,k+1) oper (stepOfDynamics (n, 0, k) oper (maxMatrix,minMatrix))
                                                 | otherwise = (maxMatrix, minMatrix)

stepOfDynamics :: (Int, Int, Int)  -> [(Int -> Int -> Int)] -> (Matrix Int, Matrix Int) -> (Matrix Int, Matrix Int)
stepOfDynamics (n, i, k) oper  (maxM, minM) 
                    | i < n-k = stepOfDynamics (n, i+1, k) oper (change (i, i+k) oper 0 (maxM, minM))
                    | otherwise = (maxM, minM)
change :: (Int, Int) -> [(Int -> Int -> Int)] -> Int -> (Matrix Int, Matrix Int) -> (Matrix Int, Matrix Int)
change (l, r) oper i (maxM, minM)
                    | l+i+1 > r = (maxM, minM)                 
                    | otherwise = change (l, r) oper (i+1) (set maxM (l,r) maxLocal,
                                                            set minM (l,r) minLocal)
            where 
                maxmax = ((!!) oper (l+i)) (get maxM (l, l+i)) (get  maxM (l+i+1, r))
                minmax = ((!!) oper (l+i)) (get minM (l, l+i)) (get  maxM (l+i+1, r))
                maxmin = ((!!) oper (l+i)) (get maxM (l, l+i)) (get  minM (l+i+1, r))
                minmin = ((!!) oper (l+i)) (get minM (l, l+i)) (get  minM (l+i+1, r))
                maxLocal = (max (max (max maxmax minmax) (max maxmin minmin)) (get maxM (l,r)))
                minLocal = (min (min (min maxmax minmax) (min maxmin minmin)) (get minM (l,r)))
                
intToString :: Int -> String -> String
intToString 0 s = s
intToString a s = intToString c ((intToDigit b):s)
                where   
                    c = quot a 10
                    b = a - c*10
restore :: (Int, Int) -> Int -> ([(Int -> Int -> Int)], [String]) -> Matrix Int -> (Matrix Int, Matrix Int) -> String
restore (l, r) i (oper,operS) matrix (maxM, minM)
                    | l == r = (intToString (get matrix (l, r)) "")
                    | local == maxmax  = "(" ++ (restore (l, l+i) 0 (oper,operS) maxM (maxM, minM))   ++ ((!!) operS (l+i)) ++   (restore (l+i+1,r) 0 (oper,operS) maxM (maxM,minM)) ++ ")"
                    | local == minmax  = "(" ++ (restore (l, l+i) 0 (oper,operS) minM (maxM, minM))   ++ ((!!) operS (l+i)) ++   (restore (l+i+1,r) 0 (oper,operS) maxM (maxM,minM)) ++ ")"
                    | local == maxmin  = "(" ++ (restore (l, l+i) 0 (oper,operS) maxM (maxM, minM))   ++ ((!!) operS (l+i)) ++   (restore (l+i+1,r) 0 (oper,operS) minM (maxM,minM)) ++ ")"
                    | local == minmin  = "(" ++ (restore (l, l+i) 0 (oper,operS) minM (maxM, minM))   ++ ((!!) operS (l+i)) ++   (restore (l+i+1,r) 0 (oper,operS) minM (maxM,minM)) ++ ")"
                    | otherwise = (restore (l, r) (i+1) (oper,operS) matrix (maxM, minM))
            where
                maxmax = ((!!) oper (l+i)) (get maxM (l, l+i)) (get  maxM (l+i+1, r))
                minmax = ((!!) oper (l+i)) (get minM (l, l+i)) (get  maxM (l+i+1, r))
                maxmin = ((!!) oper (l+i)) (get maxM (l, l+i)) (get  minM (l+i+1, r))
                minmin = ((!!) oper (l+i)) (get minM (l, l+i)) (get  minM (l+i+1, r))
                local = get matrix (l, r)
                
main' :: [String]                
main' = [
      (addPars "12+23*2"),
      (addPars "12*3-14*6"),
      (addPars "3*12+13-15*17"),
      (addPars "32*13-11-23*2"),
      (addPars "12+43*2-15+13*2+13*6+12*43+1-12+23")
      ]



               
                
                
                
                