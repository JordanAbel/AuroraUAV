# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(terra) # instead of raster
library(viridis)
library(crayon)
library(BiocManager)
library(future)
library(rgdal)
library(rayshader)
library(rgl)

# To change the memory limit of R virtual memory
Sys.setenv(R_MAX_VSIZE = "100Gb")
Sys.getenv("R_MAX_VSIZE") # verify that changes has taken effect

message(green("\n[INFO] ", Sys.time(), " Reading .LAS files"))

# =========================TESTING TILING===================

lasC <- readLAScatalog("/Users/Shea/Desktop/COMP 4910/RGB Data/points.las")
ortho <- terra::rast("/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif")
t_dir <- "/Users/Shea/Desktop/COMP 4910/RGB Data/Tiles"

opt_output_files(lasC) <- paste0(t_dir, "/{XLEFT}_{YBOTTOM}_{ID}") # label outputs based on coordinates
opt_chunk_size(lasC) <- 250 # square tile size in meters
opt_chunk_buffer(lasC) <- 0
tiles <- catalog_retile(lasC)
tiles <- readLAScatalog(t_dir)

plot(tiles, chunk = TRUE)
# plotRGB(ortho)

# Read only the specified files of the LAScatalog
t_files <- list.files(t_dir, pattern = ".las$", full.names = TRUE)
subset <- readLAS(t_files[1:5])

plot(subset, bg = "white")

norm_color <- function(l, o) # create user-defined function
{
  cn <- classify_noise(l, ivf(18, 2))
  cn <- filter_poi(cn, Classification != LASNOISE) # denoise
  gnd <- classify_ground(cn, csf(sloop_smooth = TRUE, class_threshold = 1, time_step = 0.65))
  nl <- normalize_height(gnd, knnidw()) # normalize
  # colorized <- merge_spatial(nl, o) # colorize
  return(nl) # output
}

# las <- clip_circle(tiles, 680500, 5605500, 100)

nlasrgb <- norm_color(subset, ortho) # apply user defined function

plot(nlasrgb, color = "RGB", bg = "white", size = 2)

# ====================================================

# TODO: Update path to folder location on your local machine
# lasC <- readLAScatalog("/Users/Shea/Desktop/COMP 4910/RGB Data/LiDAR split files/-split1")
# las <- readLAS(lasC, filter = "-drop_z_below 0")

message(green("\n[INFO] ", Sys.time(), " Checking .LAS file"))
las_check(las)
message(green("\n[INFO] ", Sys.time(), " .LAS file check complete"))

# plot(las, bg = "white")

# removing noise from las file (sor or ivf algo)
# note: ivf seems to be much faster, unsure if signifigant difference in results is present
las <- classify_noise(las, sor(18, 3))
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

las <- classify_ground(las, csf(sloop_smooth = TRUE, class_threshold = 1, time_step = 0.65))
# plot_crossection(las, colour_by = factor(Classification))

gnd <- filter_ground(las)
# plot(gnd, size = 3, bg = "white", color = "Classification")

dtm <- rasterize_terrain(las, res = 1, knnidw())
# plot_dtm3d(dtm, bg = "white")

nlas <- normalize_height(las, knnidw())
plot(nlas, bg = "white")

# hist(filter_ground(nlas)$Z, breaks = seq(-50, 50, 0.01), main = "", xlab = "Elevation")

col <- height.colors(25)
# TODO: Find maximum res (lower number) by 6/ point cloud density
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


# =======attempting to transfer tree segments to rgb orthomosaic=======
trees_sp <- as_Spatial(crowns)
ortho_rgb <- brick("/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif") # TODO: change path
trees_sp <- spTransform(trees_sp, crs(ortho_rgb))
trees_sf <- st_as_sf(trees_sp)
ortho_rgb_cropped <- crop(ortho_rgb, extent(trees_sp))
trees_raster <- rasterize(trees_sf, ortho_rgb_cropped)

plotRGB(ortho_rgb_cropped, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
plot(trees_raster, col = "red", add = TRUE)

# ===================================================================
metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
head(metrics)

# plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

plot(metrics["z_max"], pal = hcl.colors)

tree110 <- filter_poi(las, treeID == 282)
plot(tree110, bg = "white")
