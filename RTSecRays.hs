module RTSecRays where

import Declarations
import Data.List
import RTIntersections
import RTShade
import TextureMap
import CubicEnvironment

initial :: Point -> Vector -> Object -> Double -> Scene -> CubeMap ->Colour
initial p v o@(c,_,_,_,_) d (os,l) cube =  add colour (xMult (mn coef) (1,1,1))
	where
	apply :: Colour -> Colour -- apply may be better suited for more realistic images, but for single color objects it creates unrealistic specular reflections
	apply (r,g,b) = (r*coef,b*coef,g*coef)

	colour = secMain p v (Just (d,o)) (os,l) 0 cube
	coef = (specValue (getPointInt p v d) l c)^genSpec
	mn :: Double -> Double
	mn c
		| c<0 = 0
		| otherwise = c

secMain :: Point -> Vector -> Maybe (Double,Object) -> Scene -> Int -> CubeMap -> Colour
secMain p v (Just(d,obj@(o,r,c,i,n))) s nr cube = add  (col) (xMult i (reflex col pnt vec dob s nr cube))
	where
	pnt = getPointInt p v d
	vec = refVec v (getNormal pnt o)
	dob = new pnt vec s
	col = getColour p v obj d s

reflex :: Colour -> Point -> Vector -> Maybe (Double,Object) -> Scene -> Int -> CubeMap -> Colour 
reflex col pnt vec Nothing s nr cube = environment vec cube -- (0,0,0)
reflex col pnt vec (Just (dist,obj@(o,r,c,i,n))) s nr cube
	| nr == maxRec || i==0 = (0,0,0)
	| otherwise = secMain pnt vec (Just(dist,obj)) s (nr+1) cube
                    

getPointInt :: Point -> Vector -> Double -> Point
getPointInt p v d = add p (xMult d v) 

getColour :: Point -> Vector -> Object -> Double -> Scene  -> Colour -- point of origin of vector, but object it is pointing to
getColour p v obj@(o,r,c,i,n) d s = xMult (shadeCoef p v obj d s) (getTexel (getPointInt p v d) obj)

getNormal :: Point -> Point -> Vector -- two points are point of incidence and origin of sphere
getNormal i o= norm (sub i o)

refVec :: Vector -> Vector -> Vector -- incident and normal vectors
refVec vi vn = norm(sub vi (xMult (2*(dot vn vi)) vn))

new :: Point -> Vector -> Scene -> Maybe(Double, Object)
new p v s = distIntersect p v s 

halfVector :: Point -> Light -> Vector-- point of intersection and light source
halfVector pi l = norm(add (sub eye pi) (sub l pi))

specValue :: Point -> Light -> Point -> Double -- point of intersection, light, center of sphere
specValue pi l po = dot (getNormal pi po) (halfVector pi l)
