module CubicEnvironment where

import Declarations
import Data.List
import Data.Map

-- gets point on screen and normal viewing vector
-- how i unfold the cube :
--left=1
--front=2
--up=3
--down=4
--right=5
--back=6

sizeU,sizeV :: Int
sizeU = 500 -- these should be changed for a different texture
sizeV = 500
tot :: Int
tot = sizeU*sizeV*3 - 1 -- biggest index of our string (counting from 0 ! - lost 30 minutes figuring that out)


environment :: Vector -> CubeMap-> Colour
environment v cube= readCube (detectPlace v) cube

detectPlace :: Vector -> (Int,Double,Double) -- returns the face and the coordonates where our texel is
detectPlace (x,y,z)
           | ax>=ay && ax>=az && x>0= (1, (1.0 - (z/x + 1)*0.5),(1.0 - (y/x +1)*0.5))
           | ax>=ay && ax>=az = (5, (1.0 - (z/x + 1)*0.5),((y/x+1)*0.5) )
           | ay >= ax && ay >= az && y>0 = (3,((x/y +1)*0.5),(1.0 - (z/y +1) * 0.5))
           | ay>=ax && ay>=az = (4,(1.0 - (x/y+1)*0.5),((z/y + 1)*0.5))
           | az >= ax && az>= ay && z>0 = (2, ((x/z + 1)*0.5),((y/z + 1)*0.5))
           | az >=ax && az>=ay = (6, ((x/z+1)*0.5),(1.0 - (y/z+1)*0.5))
           | otherwise = error "detectFace"
               where
                 ax = abs x
                 ay = abs y
                 az = abs z

readCube :: (Int,Double,Double) -> CubeMap -> Colour -- does bilinear interpolation on our coordinates to get texel
readCube (f,u,v) cube = ((1 - vcoef) `xMult` ((1 - ucoef) `xMult`(getCuble f (minU + sizeU*minV) cube) `add` (ucoef `xMult` (getCuble f (maxU + sizeU * minV) cube)))) `add`(vcoef `xMult`(((1 - ucoef)`xMult`(getCuble f ( minU + sizeU * maxV ) cube)) `add` (ucoef `xMult` (getCuble f (maxU + sizeU * maxV) cube ))))
          where
           auxU = ((fromIntegral sizeU) * (abs u))
           auxV = ((fromIntegral sizeV) * (abs v))
           umin,vmin,umax,vmax,maxU,maxV,minU,minV :: Int
           auxU,auxV,ucoef,vcoef :: Double
           umin = truncate auxU
           vmin = truncate auxV
           umax = umin + 1
           vmax = vmin + 1
           ucoef = abs (auxU - (fromIntegral umin))
           vcoef = abs (auxV - (fromIntegral vmin))
           minU = clampAgain umin (sizeU -1)
           minV = clampAgain vmin (sizeV -1)
           maxU = clampAgain umax (sizeU -1)
           maxV = clampAgain vmax (sizeV -1)

cub :: [[Int]]
cub =  [[],[],[1,2,3,4,5,6],[7,8,9,0,1,2,3,5,6,7,8,9],[4,5,6,17,8,9,5,6,7,8,9]]

clampAgain :: Int -> Int -> Int
clampAgain x max
           | x<0 = 0
           | x>max = max
           | otherwise = x

getCuble :: Int -> Int -> CubeMap -> Colour
getCuble f x cube = ((fromIntegral(face ! (tot - 3*x-2))/255),(fromIntegral(face ! (tot - 3*x-1))/255),(fromIntegral(face ! (tot - 3*x))/255))
			where	
				face = (cube !! (f-1))  
				
	
