solutionsCount :: Double -> Double -> Double -> Int
solutionsCount a b c =
 if a == 0 then if b == 0 then if c == 0 then -1
			       else 0
		else 1
 else if d > 0 then 2
      else if d == 0 then 1
      else 0
 where
  d = b*b - 4 * a * c   

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
 | x >= y && x >= z = x
 | y >= z           = y
 | otherwise        = z

factorial :: Integer -> Integer
factorial n =
 if n == 0 then 1
 else n * (factorial (n-1))

fib :: Integer -> Integer
fib n
 | n == 0 || n == 1 = 1
 | otherwise        = fib (n-1) + fib (n-2)

linearFib :: Integer -> Integer
linearFib n = fibIter 1 1 0
 where
  fibIter a b i = if (i == n) then a
                  else fibIter b (a+b) (i+1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrimeHelper 2
 where
  isPrimeHelper i
   | (n `mod` i) == 0 = False
   | fromIntegral i > s	      = True
   | otherwise 	      = isPrimeHelper (i+1)
   where
    s = (sqrt (fromIntegral n))

repeatChar :: Char -> Integer -> String
repeatChar ch n
 | n == 0      = ""
 | otherwise   = ch : (repeatChar ch (n-1))

isPrefix :: String -> String -> Bool
isPrefix small big
 | length small > length big = False
 | small == ""		     = True
 | head small /= head big    = False
 | otherwise		     = isPrefix (tail small) (tail big)

countSubstrings :: String -> String -> Integer
countSubstrings small big
 | length small > length big = 0
 | isPrefix small big	     = 1 + countSubstrings small (tail big)
 | otherwise		     = countSubstrings small (tail big)

findMin :: [Double] -> Double
findMin numbers
 | length numbers == 1 = head numbers
 | otherwise           = min (head numbers) (findMin (tail numbers))

findMax :: [Double] -> Double
findMax numbers
 | length numbers == 1 = head numbers
 | otherwise	       = max (head numbers) (findMax (tail numbers))

findMinAndMax :: [Double] -> (Double, Double)
findMinAndMax numbers = (findMin numbers, findMax numbers)
