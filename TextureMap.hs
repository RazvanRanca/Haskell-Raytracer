module TextureMap where

import Declarations
import Data.List
-- Needs object and the point on the object to be texture mapped (represented as vector from origin)

getTexel :: Vector -> Object -> Colour
getTexel v obj@(o,r,c,i,n) = helpTexel (round(fst exDisplay)) (round(snd exDisplay)) coord c
	where
	coord = getCoord v obj

	helpTexel :: Int -> Int -> (Int,Int) -> [Colour] -> Colour
	helpTexel _ _ (_,_) [] = error "Texel"
	helpTexel w h (a,b) cs = cs !! ((mi*100-(b+1))*w + a)

getCoord :: Vector -> Object -> (Int,Int)
getCoord  v o = ( truncate((long vPoint vY vZ)*(fst exDisplay)) , truncate((nLat vPoint vY) * (snd exDisplay)))
	where
	vPoint = vP v o

vY,vZ :: Vector -- The vectors that point from the center of the sphere to the north pole and equator, aka a normal vector in the direction of Y and Z respectively

vY = (0.0,1.0,0.0)
vZ = (0.0,0.0,1.0)

vP :: Vector -> Object -> Vector -- calculates unit vector from center of sphere to point we are colouring from incidence vector 
vP v (o,_,_,_,_) = norm(sub v o)

lat :: Vector -> Vector -> Double
lat vp vy = (acos ( (-1)*(dot vy vp)))  -- latitude 

nLat :: Vector -> Vector -> Double -- squashed lat for latter scalling
nLat vp vy = (lat vp vy) / pi

long :: Vector -> Vector -> Vector -> Double -- squashed longitude
long vp vy vz
	| (dot (vMult vy vz) vp) > 0 = cc
	| otherwise = 1 - cc
		where
		cc = (acos ((dot vp vz) / (sin (lat vp vy)))) / (2*pi)
	 
