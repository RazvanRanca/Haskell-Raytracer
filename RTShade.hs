module RTShade where

import Declarations
import Data.List
import RTIntersections

normalVector :: Point -> Vector -> Object -> Double -> Vector -- calculates normal vector at point of intersection with first object in ray's path
normalVector p v (o,r,c,i,n) d = norm (sub (add p (xMult d v)) o)

lightVector :: Point -> Vector -> Double -> Light -> Vector -- calculates unit vector from the point of intersection towards light source = vector from origin to light - vector from origin to point of intersection
lightVector p v d l = norm(sub l (add p (xMult d v)))

objectsInPath :: Point -> Vector -> Scene -> Double -> Bool -- return true if there are any objects in the path of the light ray from the intersection point to light source
objectsInPath p v (s,l) d  
              | dist == Nothing = False
              | otherwise = True
                            where
                              dist = distIntersect (add p (xMult d v)) (lightVector p v d l) (s,l) 
                            
                         
lambert :: Point -> Vector -> Object -> Double -> Light -> Double
lambert p v o d l = dot (lightVector p v d l) (normalVector p v o d)

shadeCoef :: Point -> Vector -> Object -> Double -> Scene -> Double
shadeCoef p v o d (s,l)
           | lam<=0  || (objectsInPath p v (s,l) d) = minLight
           | otherwise = minLight + ((1-minLight) * lam)
                         where
                           lam=lambert p v o d l
