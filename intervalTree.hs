import Data.List
import Data.Function
data Point = Point Double Double deriving(Show, Eq)
data Line = Line Point Point deriving(Show, Eq)
data Intervaltree = Intervaltree Double [Line] [Line] Intervaltree Intervaltree  | Empty deriving(Show, Eq)

l1 =  Line (Point 1 1) (Point 5 2)
l2 =  Line (Point 2 0.5) (Point 3 2)
l3 =  Line (Point 4 2.5) (Point 10 4)
l4 =  Line (Point 6 2.5) (Point 8 0.5)
l5 =  Line (Point 7 4.5) (Point 14 2.5)
l6 =  Line (Point 9 2.5) (Point 12 1.5)
l7 = Line (Point 11 4.5) (Point 13 4.5)

testlines = [l1, l2, l3, l4, l5, l6, l7]

constructIntervaltree :: [Line] -> Intervaltree
constructIntervaltree [] = Empty
constructIntervaltree list = let ileft = [ x |x <- list,  (maxXval x) < (xmid list)]
				 imid = [x | x <- list,(minXval x)<=(xmid list) && (xmid list)<=(maxXval x)]
				 iright = [ x |x <- list, (xmid list)<(minXval x)]
				in Intervaltree (xmid list) (sortBy (compare `on` minXval) imid ) (sortBy (compare `on` maxXval) imid ) (constructIntervaltree ileft) (constructIntervaltree iright)

xmid :: [Line] -> Double
xmid ls = median . concat $ map (\(Line (Point x1 y1) (Point x2 y2))-> [x1, x2]) ls

minXval :: Line -> Double
minXval (Line (Point x1 y1) (Point x2 y2)) = min x1 x2

maxXval :: Line -> Double
maxXval (Line (Point x1 y1) (Point x2 y2)) = max x1 x2

median :: [Double] -> Double
median x = x !! (div (length x) 2)

queryintervaltree :: Double -> Intervaltree -> [Line]
queryintervaltree q Empty = []
queryintervaltree q (Intervaltree v lleft lright childl childr) | q < v = (filter (containsX q) lleft) ++ queryintervaltree q childl
								|  q > v = (filter (containsX q) lright) ++ queryintervaltree q childr
								| otherwise = lleft

containsX :: Double -> Line-> Bool
containsX q (Line (Point x1 y1) (Point x2 y2)) | ((x1 < q) && (q < x2)) || ((x2 < q) && (q < x2)) = True
					       | otherwise = False
