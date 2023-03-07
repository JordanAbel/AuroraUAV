# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(raster)
library(viridis)
library(crayon)
library(BiocManager)
library(future)
library(rgdal)

# To change the memory limit of R virtual memory
# library(usethis)
# usethis::edit_r_environ()

message(green("\n[INFO] ", Sys.time(), " Reading .LAS files"))

# TODO: Update path to folder location on your local machine
lasC <- readLAScatalog("/Users/Shea/Desktop/COMP 4910/RGB Data/LiDAR split files/-split1")
las <- readLAS(lasC, select = "xyzr", filter = "-drop_z_below 0")

message(green("\n[INFO] ", Sys.time(), " Checking .LAS file"))
las_check(las)
message(green("\n[INFO] ", Sys.time(), " .LAS file check complete"))

# print(las)

# plot(las, bg = "white")

# removing noise from las file (sor or ivf algo)
# TODO: compare results and run time of ivf and sor
las <- classify_noise(las, sor(18,4))
las <- filter_poi(las, Classification != LASNOISE)
plot(las, bg = "white")

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()

  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")

  return(p)
}

mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, time_step = 0.65)
las <- classify_ground(las, mycsf)
plot_crossection(las, colour_by = factor(Classification))

gnd <- filter_ground(las)
# plot(gnd, size = 3, bg = "white", color = "Classification")

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")

dtm <- rasterize_terrain(las, 1, knnidw(k = 10L, p = 2))
# plot(dtm, col = gray(1:50/50))

nlas <- normalize_height(las, knnidw())
plot(nlas, bg = "white")

hist(filter_ground(nlas)$Z, breaks = seq(-50, 50, 0.01), main = "", xlab = "Elevation")

col <- height.colors(25)
chm <- rasterize_canopy(nlas, res = 0.5, pitfree(thresholds = c(0, 60), max_edge = c(0, 1.5)))
# plot(chm, col = col)

# TODO: Need to continue working from here!!!!!!!!!!!!
# Z >= n increase n (height) to decrease amount of segmented objects
las <- filter_poi(nlas, Z >= 5)

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5 ))

algo <- dalponte2016(chm_p2r_1_smoothed, ttops_chm_p2r_1_smoothed)
las <- segment_trees(las, algo) # segment point cloud
# plot(las, bg = "white", color = "treeID") # visualize trees

crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")
plot(crowns["convhull_area"], main = "Crown area (concave hull)")

hulls <- delineate_crowns(las)
writeOGR(obj = hulls, dsn = "/Users/Shea/Desktop", layer="crowns", driver="ESRI Shapefile")
shape <- read_sf(dsn = "/Users/Shea/Desktop/crowns.shp", layer = "SHAPEFILE")

metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
head(metrics)

# plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

plot(metrics["z_max"], pal = hcl.colors)

tree110 <- filter_poi(las, treeID == 282)
plot(tree110, bg = "white")
