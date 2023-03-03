# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

# To change the memory limit of R virtual memory
library(usethis)
usethis::edit_r_environ()

las <- readLAS("C:/Users/Jorda/OneDrive/Documents/University/Comp 3710/AuroraUAV/points_0000000.las",
               select = "xyzrn",
               filter = "-drop_z_below 0")

las_check(las)

print(las)

plot(las)

las <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))

plot(las, color = "Classification", size = 3, bg = "white")
