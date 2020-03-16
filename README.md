# solarsystem
This is an example of how the velocity Verlet algorithm can be applied to a real situation. The simulation includes the main bodies in the solar system, whose most important descriptors (mass, aphelion distance, eccentricity) have been checked in the available bibliography. This repository includes:
 - A Fortran file, solarsist.f. The mathematical details can be easily found online. The program reads from the file containing the escalated data (see next point). Then, the necessary computations are carried out iteratively. Some parameters, such as the number of iterations and the maximum time to simulate (in escalated time, with 1 escalated time unit ~ 6 days) can be modified easily. The program has been written using gfortran as a compilator. Take this into consideration when compiling it yourself, as I have not tested it with ifort or any other compilator.
 - A comma separated value (.csv) file, which can be read by an R script.
 - An R script, ./R/normalize.R, which escalates the data in order to make the numbers more manageable.
 - A folder with R scripts, which I have written to visualize the results of the program. Each scripts generates individual .png files than can be used to make a .gif animation. The scripts generate:
   1. An animation of the heliocentric solar system (up to the fifth body, Mars, for clarity).
   2. An animation of the geocentric solar system (with the Earth at the origin and the rest of the planets moving along the ptolemaic epicycles). Up to the fifth body, for clarity.
   3. An animation of the heliocentric solar system that zooms out. Bodies appearing smaller and some other effects have been accounted for. 
 - A folder with the generated animations. 

This simulation was written for my Computational Physics course (University of Granada, 2020).
