Haskell Ray Tracer
===

**First of all:** have a look at the *samples* folder. You can view the images directly on github. It contains 3 sample and 2 sample procedurally generated textures.

**What this is:** A basic Haskell raytracer made in a week at the end of my first university semester for a programming contest. I ended up getting 2nd place: http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/competition/2009/competition.html

**Running:** runghc Main.hs

**Customizing:** The rendered scene is set in "Values.hs", various other parameters can be set in "Declarations.hs"

Features:
--
* conic perspective (pin-hole camera)
* sphere and plane primitives
* diffuse and specular reflections (blinn-phong)
* shadows (lambert)
* texture mapping on spheres and planes
* cubic environment mapping (i.e. place world inside a big cube and put textures on the inside of the 6 faces, thus reflections behind the camera still look ok)
* procedurally generated textures similar to Perlin noise


Files:
---
* CubicEnvironment -- takes a direction vector and return the needed pixel from the needed face
* Declarations -- type classes, some simple functions and different objects that i thought might be needed in a lot of modules
* makePPM -- output functions, scaling functions, clamping functions - as the name says, creates the PPM
* Main -- just reading of the texture files and initiating of the ray trace with coordinates 
* NoiseGen -- Creates the textures from a random number generator, has quite a few functions at the bottom which can be applied on the noise for different effects, based on perlin noise
* RayTracer -- does a first intersection test, rays that intersect objects are sent to RTSecRays (ray tracing of secondary rays) and the others are sent to the cubic environmental mapping ; Also the exponential toning function is applied here, because at any later place it would have acted on the background textures as well, with unpleasent results
* RTIntersections -- Determines if a ray hits a ball and if yes, which ball and where it is hit
* RTSecRays -- Handles the recursive reflection rays by using RTIntersect. Also adds the specular reflection. When it has found the pixel it needs to make a reflection of, it calls RTShade for computation of lighting and TextureMap for determining the colour of said pixel
* RTShade -- calculates shading for a certain point, based on position of other objects and lambert function
* TextureMap -- it takes a ray and the object it hits and returns the colour of the textured pixel(texel) where the ray hit
* Values -- just my current Scene, i had to do it like this because if i put it in Declarations there was a a error due to mutual importation of modules. 