ones :: [Integer]
ones = 1 : ones

twos = 2 : twos

combine ::(a->b->c)->[a]->[b]->[c]
combine op l1 l2 = [op x y | (x, y) <- zip l1 l2]

nats :: [Integer]
nats = 0 : (combine (+) ones nats)

fromN :: Integer -> [Integer]
fromN n = n : fromN (n+1)

indirectNats = fromN 0

fibs :: [Integer]
fibs = 1 : 1 : (combine (+) fibs (tail fibs))

powsOfTwo :: [Integer]
powsOfTwo = 1 : (combine (*) twos powsOfTwo)

isPrime :: (Integer) -> Bool
isPrime 1 = False
isPrime x = [ 1 | i <- [2 .. floor $ sqrt $ fromIntegral x], x `mod` i == 0] == []

indirectPrimes :: [Integer]
indirectPrimes = filter isPrime (tail nats)

sieve (x:xs) = x : sieve [ y | y <- xs, y `mod` x /= 0]
primes = sieve [2 .. ]


aFewNats = take 15 nats
aFewFibs = take 10 fibs
aLotOfPrimes = take 30 primes