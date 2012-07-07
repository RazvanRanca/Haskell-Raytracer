module MakePPM 

where

import Declarations

-- creates ppm file and writes the initial necessary values
initialise :: Display -> FileName -> IO ()
initialise d f = writeFile f ("P3\n" ++ show(fst d) ++ " " ++ show(snd d) ++ "\n255\n")

-- takes Current state of view, number of pixels on a line, the current number of the pixel on it's line (initially 0)
showColours :: [ScaledColour] -> Double -> Double -> String
showColours [] _ _= []
showColours (c:cs) n q = show x ++ " " ++ show y ++ " " ++ show z ++ alfa ++ showColours cs n beta
	where 
	(x,y,z) = c
	alfa 
		| q < n-1 = "   "
		| otherwise = "\n"
	beta
		| q< n-1 = (q+1)
		| otherwise = 0

aver :: [Colour] -> Double -- aver provides the average luminiscence of the image, it may be used as avg in some cases
aver cs= (sum [ x+y+z | (x,y,z) <- cs])/(fromIntegral((length cs)*3))

scaleColours :: [Colour] -> [ScaledColour]
scaleColours cs = [(round(x*255),round(y*255),round(z*255)) | (x,y,z)<-cs]

printView :: [Colour] -> Display -> FileName -> IO ()
printView cs d f = do
			initialise d f
			appendFile f (showColours (scaleColours cs) (fst d) 0)



printTexture :: [ScaledColour] -> Display -> FileName -> IO ()
printTexture cs d f = do
				initialise d f
				appendFile f (showColours cs (fst d) 0)
	 

theClamp :: [Colour] -> [Colour] -- if the luminiscence variance isn't very big in the picture, it is better to simply clamp the values instead of tone mapping them
theClamp = map (\(x,y,z) -> ( minmax x, minmax y, minmax z))
	where
	minmax :: Double -> Double
	minmax x
		| x>1 = 1
		| x<0 = 0
		| otherwise = x 
