# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(terra) # instead of raster
library(crayon)
library(sp)
library(viridis)

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
# subset <- readLAS(t_files[8:10]) # t_files[n:m] = file position in order. Delete “[n:m]” to read entire catalog.
subset <- readLAS(t_files)

# Check subset tiles. Will indicate if there are any invalidities.
message(green("\n[INFO] ", Sys.time(), " Checking point cloud"))
las_check(subset)

# plot(subset, bg = "white")

# ===========================================================================================
# POINT CLOUD PRE-PROCESSING
# ===========================================================================================

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

# Pre-processing function to denoise and normalize point cloud. Boolean option params to generate plots
noise_norm <- function(l, p, subplots)
{
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Processing", npoints(l), "points"))
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Classifying & filtering noise points"))
  cn <- classify_noise(l, sor(19, 0.9))
  cn <- filter_poi(cn, Classification != LASNOISE) # drop noise points
  diff <- npoints(l) - npoints(cn)
  per <- round((diff / npoints(l)) * 100, 2)
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Removed", diff, "noise points (", per, "% )"))

  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Classifying ground points"))
  gnd <- classify_ground(cn, csf(sloop_smooth = TRUE, class_threshold = 1, time_step = 0.65))

  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Normalizing height"))
  nl <- normalize_height(gnd, knnidw()) # normalize
  nl <- filter_poi(nl, Z >= 0) # drop points below zero

  # Generate subplots (de-noised, ground, terrain model, normalized, histogram)
  if (subplots == TRUE)
  {
    message(green("\n[INFO] ", Sys.time(), " noise_norm() - Generating subplots"))

    plot_crossection(nl, colour_by = factor(Classification))
    plot(cn, bg = "white", main = "De-noised Point Cloud")
    plot_dtm3d((rasterize_terrain(gnd, res = 1, tin())), bg = "white", main = "Terrain Model")
    plot(nl, bg = "white", main = "Normalized Point Cloud")
    hist(filter_ground(nl)$Z, breaks = seq(-50, 50, 0.01), xlab = "Elevation", main = "Elevation Histogram")
  }

  # plot processed subset point cloud in rgb colour
  if (p == TRUE)
    message(green("\n[INFO] ", Sys.time(), " noise_norm() - Generating processed plot in RGB colour"))
    plot(nl, color = "RGB", bg = "white", size = 3)

  return(nl) # output
}

nnsub <- noise_norm(subset, FALSE, FALSE)

# ===========================================================================================
# POINT CLOUD POST-PROCESSING
# ===========================================================================================

las <- nnsub
c <- height.colors(25)
# Tune base_res. May change depending on subset
base_res <- (6 / density(nnsub))
base_res <- (6 / density(nnsub)) + 0.1

# This function will create the window size based off of the tree height
variable_ws <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

# Pitfree with and subcircle tweak
chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(
  subcircle = 0.15,
  thresholds = c(0,5,10,15,20,25,30,35,40),
  max_edge = c(0, 2)
), pkg = "terra")

ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(variable_ws))

plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = c)
plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)

algo <- dalponte2016(chm_pitfree_05_2, ttops_chm_pitfree_05_2)
las <- segment_trees(las, algo) # segment point cloud
# plot(las, bg = "white", color = "treeID") # visualize trees

# Testing displaying ID's
# trees_clean <- subset(las, !is.na(las@data$treeID))
# plot(trees_clean, bg = "white", color = "treeID") # visualize trees
# text(trees_clean@data$X, trees_clean@data$Y, labels = trees_clean@data$treeID, cex = 0.8, pos = 1)


crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")
# TODO: Modify to drop segments of either too little points or small area
q_val <- quantile(crowns$convhull_area, 0.12)
crowns <- crowns[crowns$convhull_area >= q_val,]
# plot(crowns["convhull_area"], main = "Crown area (concave hull)", col = viridis(10))

# ======Testing adding column to crowns data table======
ct <- crowns

# Adding new column
ct$species <- 1 # add int value representing species to every row

# deleteing column
ct$species <- NULL

# modifying rows
ct$species <- 2 # update all rows of species column
ct$species[ct$treeID == 2100] <- 1 # updating species value of treeID 4
ct$species[ct$treeID %in% c(3, 5, 7)] <- 3 # updating all rows with treeID 3, 5, 7 to 3

head(ct)

# ===exporting data===

st_write(ct, "labelled_polygons_test.geojson", driver = "GeoJSON")

# below code to create trees_sp has to be run first
# st_write(trees_sp, "spatial_polygons_test.geojson", driver = "GeoJSON") # doesnt work due to sp format.

# =========================================

# Overlay tree segments to corresponding rgb orthomosaic section
trees_sp <- spTransform(as_Spatial(crowns), crs(ortho))
ortho_cropped <- crop(ortho, extent(subset))

png(filename="plot1.png", width = 6000, height = 6000, units = "px") # initialize png output for plot

plotRGB(ortho_cropped, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
plot(trees_sp, add = TRUE, border = "red", lwd = 1.5)
text(trees_sp, labels = trees_sp@data$treeID, cex = 1.1, col = "red")
dev.off() # save png after plotting data


# plot(trees_sp, border = "red", lwd = 1.5)


# =====Testing merging polygons with rgb for ML algo data=====
# labeled <- merge(ortho_rgb_cropped, trees_sp)
trees_rast <- rasterize(trees_sp, ortho_cropped, field = 1)

st_crs(trees_sp) <- st_crs(ortho_cropped)

sv_trees <- as(trees_sp, "SpatVector")
vals <- terra::extract(ortho_cropped, sv_trees)


terra::writeRaster(labeled, "labelled_rgb_test_1.png")


# ===================================================================
metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
head(metrics)

# plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

plot(metrics["z_max"], pal = hcl.colors)

tree110 <- filter_poi(las, treeID == 282)
plot(tree110, bg = "white")