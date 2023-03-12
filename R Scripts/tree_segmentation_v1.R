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

# Increase virtual memory limit of R environment
Sys.setenv(R_MAX_VSIZE = "100Gb")
Sys.getenv("R_MAX_VSIZE") # verify that change has taken effect

message(green("\n[INFO] ", Sys.time(), " Reading .LAS files"))

# TODO: Update paths for local machine
lasC <- readLAScatalog("/Users/Shea/Desktop/COMP 4910/RGB Data/points.las") # Full ortho .las point cloud
ortho <- terra::rast("/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif") # Full RGB ortho map
t_dir <- "/Users/Shea/Desktop/COMP 4910/RGB Data/Tiles" # directory to store tiles after tiling step

opt_output_files(lasC) <- paste0(t_dir, "/{XLEFT}_{YBOTTOM}_{ID}") # label outputs based on coordinates
opt_chunk_size(lasC) <- 250 # square tile size in meters
opt_chunk_buffer(lasC) <- 0
# tiles <- catalog_retile(lasC) # Tile full ortho point cloud. **COMMENT OUT AFTER 1 SUCCESSFUL RUN**
tiles <- readLAScatalog(t_dir) # Read tile folder

# plot(tiles, chunk = TRUE)
# plotRGB(ortho)

# Read only the specified subset of tiles - for processing speeds & testing purposes
t_files <- list.files(t_dir, pattern = ".las$", full.names = TRUE)
subset <- readLAS(t_files[1:5])

# Check subset tiles. Will indicate if there are any invalidities.
message(green("\n[INFO] ", Sys.time(), " Checking point cloud"))
las_check(subset)

plot(subset, bg = "white")

# las <- clip_circle(tiles, 680500, 5605500, 100)

# Function to plot crossection of point cloud. Illustrates ground/ non-ground points
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

# Function to denoise and normalize point cloud
noise_norm <- function(l, p, subplots = FALSE)
{
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Classifying & filtering noise points"))
  cn <- classify_noise(l, sor(19, 0.9))
  cn <- filter_poi(cn, Classification != LASNOISE) # drop noise points

  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Classifying ground points"))
  gnd <- classify_ground(cn, csf(sloop_smooth = TRUE, class_threshold = 1, time_step = 0.65))

  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Normalizing height"))
  nl <- normalize_height(gnd, knnidw()) # normalize
  nl <- filter_poi(nl, Z >= 0) # drop points below zero

  # Generate subplots (de-noised, ground, terrain model, normalized, histogram)
  if (subplots == TRUE)
  {
    message(green("\n[INFO] ", Sys.time(), " noise_norm() - Generating subplots"))

    plot(cn, bg = "white", main = "De-noised Point Cloud")
    plot_dtm3d((rasterize_terrain(gnd, res = 1, tin())), bg = "white", main = "Terrain Model")
    plot(nl, bg = "white", main = "Normalized Point Cloud")
    plot_crossection(nl, colour_by = factor(Classification))
    hist(filter_ground(nl)$Z, breaks = seq(-50, 50, 0.01), xlab = "Elevation", main = "Elevation Histogram")
  }

  # plot processed subset point cloud in rgb colour
  if (p == TRUE)
    message(green("\n[INFO] ", Sys.time(), " noise_norm() - Generating processed plot in RGB colour"))
    plot(nl, color = "RGB", bg = "white", size = 3)

  return(nl) # output
}

nnsub <- noise_norm(subset, TRUE, FALSE)

las_check(nnsub) # check again to ensure noise_norm went well. This may be unnecessary.

# ========================================

col <- height.colors(25)
# TODO: Calculate & tune res. Start at 6/ point cloud density. Will change depending on subset
chm <- rasterize_canopy(nnsub, res = 0.08, pitfree(thresholds = c(0, 60), max_edge = c(0, 1.5)))
plot(chm)

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
