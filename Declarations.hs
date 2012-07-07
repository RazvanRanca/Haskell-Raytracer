module Declarations where

import Data.List
import Data.Char
import qualified Data.Map

m= 5

e :: Double
e = 2.71828183

black,white,red,blue,green,gblue, purple, yellow :: Colour
black = (0.0,0.0,0.0)
white = (1.0,1.0,1.0)
red = (1.0,0.0,0.0)
blue = (0.0,1.0,0.0)
green = (0.0,0.0,1.0)
gblue = (0.0,1.0,1.0)
purple = (1.0,0.0,1.0)
yellow = (1.0,1.0,0.0)

exDisplay = (m*100.0,m*100.0)
exFile = "blab.ppm"
textFile = "texture.ppm"

genSpec :: Int -- general specularity, replaces shininess, light and material specular coeficient for now
genSpec = 25

maxRec :: Int
maxRec = 3

minLight :: Double
minLight = 0.4

bgColour :: Colour
bgColour = (0.0,0.0,0.0)

eye :: Point
eye = (m*50,m*50,m*(-20))

minDist,maxDist :: Double
minDist = 0.1
maxDist = 999999999

checkBoard :: Int -> [Colour]
checkBoard 0 = []
checkBoard x = concat(zipWith (\c1 -> \c2 -> [c1,c2]) (replicate ((mi*100)`div`2) (first x)) (replicate ((mi*100)`div`2) (second x))) ++ checkBoard (x-1)

	where
	first :: Int -> Colour
	first x
		| x`mod`2 == 0 = ft
		| otherwise = sd
	second :: Int -> Colour
	second x
		| x`mod`2==0 = sd
		| otherwise = ft
	ft = (1.0,0.0,0.0)
	sd = (0.0,0.0,1.0)

bigCheckBoard :: Int -> Colour -> Colour -> [Colour]
bigCheckBoard 0 c1 c2= []
bigCheckBoard x c1 c2= concat(zipWith (++) (replicate (mi*10) (replicate 10 (first ((x-1)`div`5)))) (replicate (mi*10) (replicate 10 (second ((x-1)`div`5))))) ++ bigCheckBoard (x-1) c1 c2
	where
	first :: Int -> Colour
	first x
		| x`mod`2 == 0 = ft
		| otherwise = sd
	
	second :: Int -> Colour
	second x
		| x`mod`2==0 = sd
		| otherwise = ft
	ft = c1
	sd = c2
exPlane :: Plane
exPlane = (norm(0.0,1.0,0.0),10.0,(1.0,1.0,0.0))

exTriangle = (exPlane, (10.0,10.0,5.0),(20.0,10.0,15.0),(25.0,10.0,40.0),(1.0,0.0,1.0))	

exScene2 :: Scene
exScene1 = ([((75.0,70.0,120.0),40.0,(1.0,0.0,0.0),0.5,0.0),((75.0,35.0,35.0),30.0,(0.0,0.0,1.0),0.0,0.0)],(100,400,200))

{-exScene3 = ([((m*34.4436, m*65.5563, m*30.0),m*20.0,(makeColours),0.4,0.0), ((m*50.0,m*35.0,m*30.0),m*20.0,(replicate ((mi*100)^2) yellow),0.4,0.0),((m*65.5563,m*65.5563,m*30.0),m*20.0,(replicate ((mi*100)^2) gblue),0.4,0.0)],(m*(-10.0), m*80.0, m*(-5.0)))-}

-- replicate ((mi*100) ^ 2) (1.0,1.0,0.0)

mi = round m
mit :: Integer
mit = round m
exScene2 = ([((m*34.4436, m*65.5563, m*30.0),m*20.0,(bigCheckBoard(mi*100) red blue),0.4,0.0), ((m*50.0,m*35.0,m*30.0),m*20.0,(bigCheckBoard(mi*100) yellow gblue),0.4,0.0),((m*65.5563,m*65.5563,m*30.0),m*20.0,(bigCheckBoard(mi*100) purple green),0.4,0.0)],(m*(-10.0), m*80.0, m*(-5.0))) -- 100 80 5

-- number of pixels in the display
type Display = (Double,Double)

type CubeMap = [Data.Map.Map Int Int]

type Plane = (Vector,Double,Colour) -- the planes normal vector and the distance from the plane to the origin + colour

type FileName = String

type Point = (Double,Double,Double) -- 3D Coordinates x y z

type Triangle = (Plane,Point,Point,Point,Colour) -- the plane it's situated on and it's vertices

type Object = (Point,Double,[Colour],Double,Double) -- Only spheres for now, centre, radius, colour, reflectivity , refraction index

type Colour = (Double,Double,Double) -- RGB will be scaled to between 0 and 1

type ScaledColour = (Int,Int,Int) -- RGB between 0 and 255 -- only done before printing

type View = [Colour] -- Colour of every pixel

type Scene = ([Object],Light)

type Vector = Point -- 3D vector, projections on x y z

type Light = Point -- light is punctiform and unique for now

add :: Vector -> Vector -> Vector
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

sub :: Vector -> Vector -> Vector
sub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

xMult :: Double -> Vector -> Vector
xMult x (a,b,c) = (a*x,b*x,c*x)

vMult :: Vector -> Vector -> Vector
vMult (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)

dot :: Vector -> Vector -> Double
dot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

mag :: Vector -> Double
mag v1 = sqrt(abs(dot v1 v1))

combine :: Colour -> Colour -> Colour
combine (x,y,z) (a,b,c) = (x*a,b*y,c*z)

norm :: Vector -> Vector
norm v
	| magnitude == 0 = (0.0,0.0,0.0)
	| otherwise = xMult (1/magnitude) v
		where
		magnitude = mag v

neg :: Vector -> Vector
neg (a,b,c) = (-a,-b,-c)



