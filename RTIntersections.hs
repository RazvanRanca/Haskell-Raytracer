module RTIntersections where

import Declarations
import Data.List

vectorPointCenter :: Point -> Object -> Vector
vectorPointCenter p (o,r,c,i,n) = sub o p

distIntersect1 :: Point -> Vector -> Object -> Double
distIntersect1 p v (o,r,c,i,n) 
	| hVector > r^2 = 0.0
	| otherwise = lengthI 
		where
		lengthV = (dot (vectorPointCenter p (o,r,c,i,n)) v) -- length from vector origin to point where vector intersects with the perpendicular line which passes through the sphere's origin
		hVector = abs((mag (vectorPointCenter p (o,r,c,i,n)))^2 - lengthV^2) -- square of the distance between our vector and the sphere's origin
		lengthI = lengthV - (sqrt(r^2 - hVector))

distIntersect :: Point -> Vector -> Scene -> Maybe(Double,Object) -- gets distance to closest object and the object itself, if distance is maxDist, there is no object in the ray's path
distIntersect p v (s,l) 
	| val == [] = Nothing   
	| otherwise = Just (minimum val)
	where
	val = (filter (\(x,y) -> x>minDist) (map (\x -> (distIntersect1 p v x , x)) s))

-- Note distances stored are distances from the vector's origin to the sphere
