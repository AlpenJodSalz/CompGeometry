import Data.List

data Point = Point Double Double deriving(Show, Eq)

p0 = Point 1 3
p1 = Point 2 4
p2 = Point 2 3
p3 = Point 2 2
p4 = Point 3 5
p5 = Point 3 4
p6 = Point 3 2
p7 = Point 3 1
p8 = Point 4 4
p9 = Point 4 3
p10 = Point 4 2
p11 = Point 5 3

--random order
points = [p7, p11, p5 , p1 , p4 , p8, p9, p0, p10, p2, p3, p6]

orderPointClockwise :: Point -> Point -> Ordering
orderPointClockwise (Point x1 y1) (Point x2 y2) | x1 < x2 = LT
						| x1 > x2 = GT
						| (x1 == x2) && (y1 > y2) = LT
						| (x1 == x2) && (y1 < y2) = GT
						| (x1 == x2) && (y1 == y2) = EQ


isrightTurn :: Point -> Point -> Point -> Bool
isrightTurn (Point x1 y1) (Point x2 y2) (Point x y) | (x-x1)*(y2-y1)-(y-y1)*(x2-x1) < 0 = False
						    | otherwise = True
valconvexHull :: [Point] -> [Point]
valconvexHull (a:b:c:[]) = if isrightTurn a b c then a:b:c:[] else a:c:[]
valconvexHull (a:b:c:xs) = if isrightTurn a b c then a: valconvexHull (b:c:xs) else valconvexHull (a:c:xs)

convexHullhalf :: [Point] -> [Point]
convexHullhalf x    | x == valconvexHull x  = x
		   | otherwise = convexHullhalf $ valconvexHull x

convexHull :: [Point] -> [Point]
convexHull x = ( init . convexHullhalf $ sortBy orderPointClockwise x)  ++  (init . convexHullhalf . reverse $ sortBy orderPointClockwise x)
