data Point = Point Double Double

getX :: Point -> Double
getX (Point x _) = x

getY :: Point -> Double
getY (Point _ y) = y

data Person = Person
			  {
				name::String,
				pin::String,
				address::String,
				age::Integer
			  }

data Shape =
	Rectangle Point Point |
	Circle Point Double

rect = Rectangle (Point 0 0) (Point 10 2)
circ = (Circle (Point 3 3) 20)
	
print' :: Shape -> String
print' (Rectangle (Point tx ty) (Point bx by)) =
	"[(" ++ (show tx) ++ ", " ++ (show ty) ++ "), (" ++ (show bx) ++ ", " ++ (show by) ++ ")]"
print' (Circle (Point x y) r) = "((" ++ (show x) ++ ", " ++ (show y) ++ "), " ++ (show r) ++ ")"

width :: Shape -> Double
width (Rectangle tl br) = getX br - getX tl
width (Circle _ r) = 2 * r

height :: Shape -> Double
height (Rectangle tl br) = getY br - getY tl
height (Circle _ r) = 2 * r

area :: Shape -> Double
area (Circle _ r) = pi * r * r
area (Rectangle tl br) = (width rect) * (height rect)
 where rect = Rectangle tl br
 
boundingBox :: Shape -> Shape
boundingBox (Rectangle tl br) = Rectangle tl br
boundingBox (Circle (Point x y) r) =
 Rectangle (Point (x - r) (y - r)) (Point (x + r) (y + r))
