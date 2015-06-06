
import Text.ParserCombinators.ReadP

data Point = Point Double Double
 deriving (Show, Eq, Ord, Read)

data Shape =
 Circle Point Double |
 Rectangle Point Double Double
 deriving (Show)

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
 deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Box a =
 Empty |
 Holder a
 deriving (Show, Eq)

unpack :: [Box a] -> [a]
unpack [] = []
unpack (Empty:xs) = unpack xs
unpack ((Holder value):xs) = value : (unpack xs)

class YesNo a where
	yesNo :: a -> Bool

instance YesNo [a] where
	yesNo [] = False
	yesNo _ = True

instance YesNo Int where
	yesNo 0 = False
	yesNo _ = True

instance YesNo Double where
	yesNo 0 = False
	yesNo _ = True

instance YesNo Integer where
	yesNo 0 = False
	yesNo _ = True

instance YesNo Point where
	yesNo (Point 0 0) = False
	yesNo _ = True

instance (YesNo a) => YesNo (Box a) where
	yesNo Empty = False
	yesNo (Holder value) = yesNo value

filterValues :: (Int->Int) -> [Int] -> [Int]
filterValues f l = filter (\x -> elem (f x) l) l

countContainingVowels :: [String] -> Int
countContainingVowels words = length $ filter (\word -> [letter | letter <- word, elem letter allVowels] /= "") words
 where allVowels = "aeiou"

isInverse :: (Int->Int) -> (Int->Int) -> (Int, Int) -> Bool
isInverse f g (a, b) = 
 (length $ filter (\x -> (f.g) x == x && (g.f) x == x) interval) == size
  where
   interval = [a .. b]
   size = length interval
