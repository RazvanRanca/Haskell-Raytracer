module Main where

import qualified Data.Map
import MakePPM
import Declarations
import RayTracer
import RTShade
import NoiseGen
import Values



getView :: Scene -> [Point] -> CubeMap -> [Colour]
getView s ps cube= map (rayTrace s cube) ps

-- generates coordinates of all pixels
getPoints :: Display -> [Point]
getPoints (a,b) = [((a-k),(b-q),0)| q<-[1 .. a], k<-[1 .. b]]

main :: IO ()
main  = do r1 <- readFile "mandelbrotSC.ppm" --"checkerboard.ppm"
           r2 <- readFile "mandelbrotSC.ppm" --"alpforward.ppm"
           r3 <- readFile "mandelbrotSC.ppm" --"alpup.ppm"           
           r4 <- readFile "mandelbrotSC.ppm" --"checkerboard.ppm"
           r5 <- readFile "mandelbrotSC.ppm" --"alprightC.ppm"
           r6 <- readFile "mandelbrotSC.ppm" --"checkerboard.ppm"     
           let
               r11 = read ("[" ++ replace r1 "\n" "," ++ "0]") :: [Int]
               r22 = read ("[" ++ replace r2 "\n" "," ++ "0]") :: [Int]
               r33 = read ("[" ++ replace r3 "\n" "," ++ "0]") :: [Int]
               r44 = read ("[" ++ replace r4 "\n" "," ++ "0]") :: [Int]
               r55 = read ("[" ++ replace r5 "\n" "," ++ "0]") :: [Int]
               r66 = read ("[" ++ replace r6 "\n" "," ++ "0]") :: [Int]
           printView (getView exScene (getPoints exDisplay) (map (Data.Map.fromAscList.(zip [0..])) [r11,r22,r33,r44,r55,r66])) exDisplay exFile
                    
        
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)           



