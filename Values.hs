module Values where

import Declarations
import NoiseGen

exScene :: Scene
exScene = ([((m*80.0,m*30.0,m*13.0),m*10.0,(solidColour mi yellow),1.0,0.0), 
            ((m*50.0,m*30.0,m*13.0),m*10.0,(solidColour mi white),1.0,0.0),
            ((m*20.0,m*30.0,m*13.0),m*10.0,(solidColour mi blue),1.0,0.0),
            ((m*80.0,m*30.0,m*28.0),m*10.0,(solidColour mi purple),1.0,0.0),
            ((m*50.0,m*30.0,m*28.0),m*10.0,(solidColour mi red),1.0,0.0),
            ((m*20.0,m*30.0,m*28.0),m*10.0,(solidColour mi black),1.0,0.0),
            ((m*80.0,m*30.0,m*43.0),m*10.0,(solidColour mi gblue),1.0,0.0),
            ((m*50.0,m*30.0,m*43.0),m*10.0,(solidColour mi yellow),1.0,0.0),
            ((m*20.0,m*30.0,m*43.0),m*10.0,(solidColour mi blue),1.0,0.0),
            ((m*80.0,m*52.0,m*13.0),m*10.0,(solidColour mi green),1.0,0.0),
            ((m*50.0,m*52.0,m*13.0),m*10.0,(solidColour mi red),1.0,0.0),
            ((m*20.0,m*52.0,m*13.0),m*10.0,(solidColour mi white),1.0,0.0),
            ((m*80.0,m*52.0,m*28.0),m*10.0,(solidColour mi gblue),1.0,0.0),
            ((m*50.0,m*52.0,m*28.0),m*10.0,(solidColour mi red),1.0,0.0),
            ((m*20.0,m*52.0,m*28.0),m*10.0,(solidColour mi white),1.0,0.0),
            ((m*80.0,m*52.0,m*43.0),m*10.0,(solidColour mi yellow),1.0,0.0),
            ((m*50.0,m*52.0,m*43.0),m*10.0,(solidColour mi red),1.0,0.0),
            ((m*20.0,m*52.0,m*43.0),m*10.0,(solidColour mi black),1.0,0.0),
            ((m*80.0,m*74.0,m*13.0),m*10.0,(solidColour mi yellow),1.0,0.0),
            ((m*50.0,m*74.0,m*13.0),m*10.0,(solidColour mi purple),1.0,0.0),
            ((m*20.0,m*74.0,m*13.0),m*10.0,(solidColour mi gblue),1.0,0.0),
            ((m*80.0,m*74.0,m*28.0),m*10.0,(solidColour mi black),1.0,0.0),
            ((m*50.0,m*74.0,m*28.0),m*10.0,(solidColour mi white),1.0,0.0),
            ((m*20.0,m*74.0,m*28.0),m*10.0,(solidColour mi yellow),1.0,0.0),
            ((m*80.0,m*74.0,m*43.0),m*10.0,(solidColour mi gblue),1.0,0.0),
            ((m*50.0,m*74.0,m*43.0),m*10.0,(solidColour mi red),1.0,0.0),
            ((m*20.0,m*74.0,m*43.0),m*10.0,(solidColour mi blue),1.0,0.0)],(m*(-10.0), m*80.0, m*(-5.0))) -- to make the sphere a even colour insert "replicate ((mi*100)^2) yourcolourhere" instead of makeColours

solidColour :: Int -> Colour -> [Colour]
solidColour mi c = replicate ((mi*100)^2) c

anObj :: Object
anObj =  ((m*50.0, m*30.0, m*110),m*80.0,(replicate ((mi*100)^2) yellow),0.9,0.0)
