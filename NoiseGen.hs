module NoiseGen where

import Declarations
import Data.Bits

col1,col2,col3,col4 :: (Int,Int,Int)
col1 = (25,131,59)
col2 = (229,180,48)
col3 = (236,56,48)
col4 = (170,48,218)
coef = 57
pow = 13
-- numberF=5
persistence= 0.1 -- lower values makes the image more diluted

primes :: [(Integer,Integer,Integer)]
primes = [(141157,50423677,1100030189),(32341,78381871,1300033981),(549379,81338461,1274003363),(991261,62684581,999005563),(497093,610125119,1567800161)]

-- maxInt=2147483647

noise :: Integer -> Integer -> (Integer,Integer,Integer) -> Double -- random number generator
noise x y (p1,p2,p3)= 1.0-((fromIntegral nnn)/1073741824.0)
	where
        n :: Integer
	n = x +  y*57
	nn = (n`shift`pow)`xor`n
	nnn = (nn*(nn*nn*p1+p2)+p3).&.0x7fffffff

smooth :: Integer -> Integer ->(Integer,Integer,Integer) -> Double
smooth x y p= corners + sides + center
             where
               corners = ((noise (x-1) (y-1) p) + (noise (x+1) (y-1) p) + (noise (x-1) (y+1) p) + (noise (x+1) (y+1) p))/16
               sides = ((noise (x-1) y p) + (noise (x+1) y p) + noise x (y+1) p)/8
               center = (noise x y p)/4

cosInterpolate :: Double -> Double -> Double -> Double
cosInterpolate a b x = a*(1.0-f) + b*f
               where
                 aux = x*pi
                 f = (1.0 - (cos aux))*0.5

interpNoise :: Double -> Double -> (Integer,Integer,Integer) -> Double
interpNoise x y p = cosInterpolate i1 i2 fy
            where
              
              ix = truncate x
              fx = x-(fromIntegral ix)
              iy = truncate y
              fy = y-(fromIntegral iy)
              v1 = smooth ix iy p
              v2 = smooth (ix+1) iy p
              v3 = smooth ix (iy+1) p
              v4 = smooth (ix+1) (iy+1) p
              i1 = cosInterpolate v1 v2 fx  --(noise ix iy p) (noise (ix+1) iy p) fx  -- uncomment these to remove smooth & increase performance quite a bit
              i2 = cosInterpolate v3 v4 fx  --(noise ix (iy+1) p) (noise (ix+1) (iy+1) p) fx

noise2D :: Double -> Double -> Double
noise2D x y = helpNoise x y 0

                
helpNoise :: Double -> Double -> Int -> Double
helpNoise _ _ 5 = 0 -- total number of octaves
helpNoise x y nr = (interpNoise (x* frequency) (y*frequency) (primes !! nr)) * amplitude*(5 / (2^(nr+1))) + helpNoise x y (nr+1)
                                   where
                                     frequency = 2^nr
                                     amplitude = persistence^nr
    
makeNoise ::[Double]
makeNoise = [ (noise2D (x- k) (y-q)) | k<- [0 .. (x-1)], q <- [0 .. (y-1)]]
            where
              x = (fst exDisplay)
              y = (snd exDisplay)
              
makeColours1 :: (Double -> Double) -> ScaledColour -> [Colour]  -- can be used w/ diluted and leaf functions
makeColours1 f c= map (fillIn c) (map f makeNoise )
	where
		fillIn :: ScaledColour -> Double -> Colour
		fillIn (r,g,b) x = ((((fromIntegral r)/255.0)*rX), (((fromIntegral g)/255.0)*rX), (((fromIntegral b)/255.0)*rX))
			where
			rX = clamp(x*0.5+0.5)

makeColours2 :: ([Double] -> [Double] -> [Double]) -> ScaledColour ->[Colour] -- can be used with the fractal functions
makeColours2 f c= map (fillIn c) (f [1 .. fst exDisplay] [1 .. snd exDisplay] )
	where
		fillIn :: ScaledColour -> Double -> Colour
		fillIn (r,g,b) x = ((((fromIntegral r)/255.0)*rX), (((fromIntegral g)/255.0)*rX), (((fromIntegral b)/255.0)*rX))
			where
			rX = clamp(x*0.5+0.5)

makeColours3 :: ([Double] -> Double -> [Double]) -> ScaledColour -> [Colour]
makeColours3 f c = map (fillIn c) (f makeNoise 1)
	where
		fillIn :: ScaledColour -> Double -> Colour
		fillIn (r,g,b) x = ((((fromIntegral r)/255.0)*rX), (((fromIntegral g)/255.0)*rX), (((fromIntegral b)/255.0)*rX))
			where
			rX = clamp(x*0.5+0.5)

clamp :: Double -> Double
clamp x
	| x<0.0 = 0.0
	| x>255.0 = 255.0
	| otherwise = x 

-- Some Pattern generating functions 

diluted :: Double -> Double -- used w/ makeColour1
diluted x = 0.5*sin(x) +0.5

slines :: [Double] -> Double -> [Double] 
slines [] _ = []
slines (d:ds) x
	| x==(fst exDisplay) = cos(x+d) : slines ds 1
	| otherwise = cos(x+d) : slines ds (x+1) 

leaf :: Double -> Double -- used w/ map and makeNoise in makeColour
leaf d = v - (fromIntegral (truncate v))
	where
	v= d*20
--Try to implement fractals with sin and cos, should be more realistic than above

fractal1 :: [Double] -> [Double] -> [Double] -- avoids makeNoise,used directly in makeColour ex : makeColours = map (fillIn col) (fractal [1 .. fst exDisplay] [1 .. snd exDisplay])
fractal1 [] _ = [] -- looks more similar to the original Perlin noise
fractal1 _ [] = []
fractal1 xs ys = concat[map (\y -> (0.5*noise2D (0.5*x) (0.5*y)) + (0.25*noise2D (0.25*x) (0.25*y)) + (0.125* noise2D (0.125*x)  (0.125*y))) ys | x<-xs]

fractal2 :: [Double] -> [Double] -> [Double] -- avoids makeNoise,used directly in makeColour w/ [1..fst.disp][1..snd.disp]
fractal2 [] _ = [] -- has an odd bumpy look, i have no idea why 
fractal2 _ [] = []
fractal2 xs ys = concat[map (\y -> (0.5*abs(noise2D (0.5*x) (0.5*y))) + (0.25*abs(noise2D (0.25*x) (0.25*y))) + (0.125* abs(noise2D (0.125*x)  (0.125*y)))) ys | x<-xs]

fractalSin :: [Double] -> [Double] -> [Double] -- same as other fractals
fractalSin [] _ = [] -- just makes lines apparently
fractalSin _ [] = []
fractalSin xs ys = concat[map (\y -> sin(x+(0.5*abs(noise2D (0.5*x) (0.5*y))) + (0.25*abs(noise2D (0.25*x) (0.25*y))) + (0.125* abs(noise2D (0.125*x)  (0.125*y))))) ys | x<-xs]

dilutedFractal :: [Double] -> [Double] -> [Double] -- same
dilutedfractal [] _ = [] 
dilutedFractal _ [] = []
dilutedFractal xs ys = concat[map (\y -> (0.5*sin(0.5*abs(noise2D (0.5*x) (0.5*y))) + (0.25*abs(noise2D (0.25*x) (0.25*y))) + (0.125* abs(noise2D (0.125*x)  (0.125*y)))) + 0.5) ys | x<-xs]

marbleHopefully :: [Double] -> Double -> Double -> [Double] -- apply like slines
marbleHopefully [] _ _ = [] -- still makes lines, not "random" enough to make marble
marbleHopefully (d:ds) x y 
	| x==(fst exDisplay) = sin(x+ (0.5*abs(noise2D (0.5*x) (0.5*y))) + (0.25*abs(noise2D (0.25*x) (0.25*y))) + (0.125* abs(noise2D (0.125*x)  (0.125*y)))) : marbleHopefully ds 1 (y+1)
	| otherwise =sin(x+ (0.5*abs(noise2D (0.5*x) (0.5*y))) + (0.25*abs(noise2D (0.25*x) (0.25*y))) + (0.125* abs(noise2D (0.125*x)  (0.125*y)))) : marbleHopefully ds (x+1) y

-- Note some of the above functions can also be combined to give some different effects ( ie: makeColours = map (fillIn col) (map diluted (fractal2 [1 .. fst exDisplay] [1 .. snd exDisplay])) ) -- I haven't had time to test all the combinations


