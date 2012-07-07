module RayTracer where

import Declarations
import Data.Char
import Data.List
import RTIntersections
import RTSecRays
import TextureMap
import CubicEnvironment
import Values



rayTrace :: Scene -> CubeMap ->  Point -> Colour 
rayTrace s cube p= colour p vec alf s cube
                                       where
                                         vec = getVector p
                                         
                                         alf = distIntersect p vec s
                                       
getVector :: Point -> Vector 
getVector p = norm(sub p eye)

colour :: Point -> Vector -> Maybe(Double,Object) -> Scene -> CubeMap -> Colour
colour p v Nothing s cube= environment v cube -- noScene p v s cube (distIntersect1 p v anObj) anObj -- noSpheres p v cube (planeInter p v exPlane)  -- to get rid of environment map as to just have a background color (increases performance quite a bit) just replace everything after = with "bgColour"
colour p v (Just (d,o)) s cube= expToneMap (initial p v o d s cube)
	
expToneMap :: Colour -> Colour -- exp and avg must be adjusted manually depending on the type of image on which it is applied -- I couldn't find any way to avoid this :(
expToneMap (x,y,z) = ( (1 - (exp ** (((-1)* x)/avg))), (1 - (exp ** (((-1)*y)/avg))) , (1 - (exp ** (((-1)*z)/avg))))
                where
                  avg = 1
		  exp = 4

{-noScene :: Point -> Vector -> Scene -> [[Int]] -> Double -> Object -> Colour
noScene p v s cube 0.0 obj= environment v cube
noScene p v s cube d obj@(o,r,cs,i,n)= lastSphere  pointI reflexV (distIntersect pointI reflexV s) cube
	where	
	pointI = (getPointInt p v d)
	norm = getNormal pointI o	
	reflexV = refVec pointI norm

lastSphere :: Point -> Vector -> Maybe (Double,Object) -> [[Int]] -> Colour
lastSphere p v Nothing cube = environment v cube
lastSphere p v (Just (d,o)) cube = bgColour -}

{-noSpheres :: Point -> Vector -> [[Int]] -> Maybe (Double,Plane) -> Colour
noSpheres p v cube Nothing = bgColour--environment v cube
noSpheres p v cube (Just (d,(n,dist,c)))= refPlane p v cube d n --helpMe (polygonInter p v exTriangle) v cube
                                             
helpMe :: Maybe (Double,Triangle) -> Vector -> [[Int]] -> Colour
helpMe Nothing v cube= bgColour--environment v cube
helpMe (Just (d,(pl,p1,p2,p3,c))) v cube= c-}
	
