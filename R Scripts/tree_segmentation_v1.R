# Automatically install and load required packages into R session hello
pkg_list <- readLines("./R Scripts/R_requirements.txt")

# Check for the existence of each package and install if necessary
cran_mirror <- "https://cran.r-project.org" # Set the CRAN mirror to use for package installations
for (pkg_name in pkg_list) {
  if (!require(pkg_name, character.only = TRUE)) {
    install.packages(pkg_name, repos = cran_mirror)
    if (!require(pkg_name, character.only = TRUE)) {
      stop("Failed to install package: ", pkg_name)
    }
  }
  library(pkg_name, character.only = TRUE)
}

# Increase virtual memory limit of R environment
Sys.setenv(R_MAX_VSIZE = "100Gb")
Sys.getenv("R_MAX_VSIZE") # verify that change has taken effect

message(green("\n[INFO] ", Sys.time(), " Reading .LAS files"))

# TODO: Update paths for local machine
lasC <- readLAScatalog("/AuroraUAV/RGB Data/points.las") # Full ortho .las point cloud
ortho <- terra::rast("/AuroraUAV/RGB Data/rgb_map.tif") # Full RGB ortho map
t_dir <- "/AuroraUAV/RGB Data/Tiles" # directory to store tiles after tiling step

opt_output_files(lasC) <- paste0(t_dir, "/{XLEFT}_{YBOTTOM}_{ID}") # label outputs based on coordinates
opt_chunk_size(lasC) <- 250 # square tile size in meters
opt_chunk_buffer(lasC) <- 0
tiles <- catalog_retile(lasC) # Tile full ortho point cloud. **COMMENT OUT AFTER 1 SUCCESSFUL RUN**
tiles <- readLAScatalog(t_dir) # Read tile folder

# plot(tiles, chunk = TRUE)
# plotRGB(ortho)

# Read only the specified subset of tiles - for processing speeds & testing purposes
t_files <- list.files(t_dir, pattern = ".las$", full.names = TRUE)
# subset <- readLAS(t_files[13:15]) # t_files[n:m] = file position in order. Delete “[n:m]” to read entire catalog.
subset <- readLAS(t_files)

# Check subset tiles. Will indicate if there are any invalidities.
message(green("\n[INFO] ", Sys.time(), " Checking point cloud"))
las_check(subset)

# plot(subset, bg = "white")

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
noise_norm <- function(l, p = TRUE, subplots = FALSE)
{
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Processing", npoints(l), "points"))
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Classifying & filtering noise points"))
  cn <- classify_noise(l, sor(19, 0.9))
  cn <- filter_poi(cn, Classification != LASNOISE) # drop noise points
  diff <- npoints(l) - npoints(cn)
  message(green("\n[INFO] ", Sys.time(), " noise_norm() - Removed", diff, "noise points"))

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

# ========================================

c <- height.colors(25)
# TODO: Tune base_res. May change depending on subset
base_res <- (6 / density(nnsub))
print(base_res)
chm <- rasterize_canopy(nnsub, res = base_res,
                        pitfree(subcircle = 0.15,
                                thresholds = c(0,5,10,15,20,25,30,35,40),
                                max_edge = c(0, 2)
                        )
)
plot(chm, col = c)

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

algo <- dalponte2016(chm_pitfree_05_2, ttops_chm_pitfree_05_2)
las <- segment_trees(las, algo) # segment point cloud
# plot(las, bg = "white", color = "treeID") # visualize trees


crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")
# TODO: Modify to drop segments of either too little points or small area
q_val <- quantile(crowns$convhull_area, 0.12)
crowns <- crowns[crowns$convhull_area >= q_val,]
plot(crowns["convhull_area"], main = "Crown area (concave hull)", col = viridis(10))

j <- (length(crowns$treeID))

cli_progress_bar("Adding RGB data to crown metrics", total = length(crowns$treeID))
for (x in 1:length(crowns$treeID)) {
  tree <- filter_poi(las, treeID==crowns$treeID[x])

  r <- tree$R
  g <- tree$G
  b <- tree$B

  crowns$R[x] <- toString(r)
  crowns$G[x] <- toString(g)
  crowns$B[x] <- toString(b)

  cli_progress_update()
}
cli_progress_done()

write.csv(crowns, "crown_metrics.csv")

# =======attempting to transfer tree segments to rgb orthomosaic=======
trees_sp <- as_Spatial(crowns)
trees_sp <- spTransform(trees_sp, crs(ortho))
# trees_sf <- st_as_sf(trees_sp)
ortho_rgb_cropped <- crop(ortho, extent(trees_sp), res = NULL)
# trees_raster <- rasterize(trees_sf, ortho_rgb_cropped)
x11(width = 50000, height = 40000)
layout(matrix(c(1), 1, 1, byrow = TRUE))

plotRGB(ortho_rgb_cropped, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
# plotRGB(ortho, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
plot(trees_sp, add = TRUE, border = "red", lwd = 1)
