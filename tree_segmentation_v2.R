# Load args from GUI
args <- commandArgs(trailingOnly = TRUE)

# Automatically install and load required packages into R session
cwd <- args[4] # Obtain execution directory from GUI
pkg_file <- paste0(cwd, "/R_requirements.txt")
pkg_list <- readLines(pkg_file)

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

# Automatically increase virtual memory limit of R environment
Sys.setenv(R_MAX_VSIZE = "100Gb")
# Sys.getenv("R_MAX_VSIZE") # verify that change has taken effect

# Paths
ortho_path <- args[1]
las_path <- args[2]
sd_path <- args[3]

# Tile params
t_size <- as.numeric(args[5])
t_buffer <- as.numeric(args[6])

# plots booleans
cross_plt <- args[7]
dtm_plt <- args[8]
non_norm <- args[9]
norm_plt <- args[10]
rgb_plt <- args[11]
canopy_plt <- args[12]
seg_plt <- args[13]
overlay__plt <- args[14]

plt_list <- args[7:14]

# print(plt_list)
# print(ortho_path)
# print(las_path)

message(green("\n[INFO] ", Sys.time(), " Reading point cloud"))
lasC <- readLAScatalog(las_path) # Full ortho point cloud
ortho <- terra::rast(ortho_path) # Full RGB ortho map

# Function to read point cloud file and perform tiling.
pc_read_tile <- function(cat, t_size, t_buff) {
  # Create directory to store tiles if it does not exist yet
  loc <- paste0(sd_path, "/Tiles")
  if(!file.exists(loc)){
    dir.create(loc)
    t_dir <- loc
  } else {
    t_dir <- loc
  }

  # Check if directory is empty to determine if tiling has been done. Perform tiling if it is.
  if(length(list.files(t_dir)) == 0) {
    message(green("\n[INFO] ", Sys.time(), " Tiling point cloud. This may take a while."))
    opt_output_files(cat) <- paste0(t_dir, "/{XLEFT}_{YBOTTOM}_{ID}") # label outputs based on coordinates
    opt_chunk_size(cat) <- t_size # square tile size in meters
    opt_chunk_buffer(cat) <- t_buff # buffer around each tile
    catalog_retile(cat) # Tile full ortho point cloud.
  }

  # Read only the specified subset of tiles - for processing speeds & testing purposes
  t_files <- list.files(t_dir, pattern = ".las$", full.names = TRUE)
  subset <- readLAS(t_files[1]) # t_files[n:m] = file position in order. Delete “[n:m]” to read entire catalog.
  # subset <- readLAS(t_files) # TODO: delete small subset line above to always do full file

  return(subset)
}


subset <- pc_read_tile(lasC, t_size = t_size, t_buff = t_buffer)

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

# Pre-processing function to denoise and normalize point cloud
noise_norm <- function(l)
{
  # Create directory to store tiles if it does not exist yet
  loc <- paste0(sd_path, "/Plots")
  if(!file.exists(loc)){
    dir.create(loc)
    nn_dir <- loc

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

    out <- list(nl, gnd, cn)

    message(green("\n[INFO] ", Sys.time(), " noise_norm() - Saving Plots"))
    writeLAS(nl, paste0(nn_dir, "/Normalized.las"), index = FALSE)
    writeLAS(gnd, paste0(nn_dir, "/Ground_Classification.las"), index = FALSE)
    writeLAS(cn, paste0(nn_dir, "/De-Noised.las"), index = FALSE)

    return(out) # output
  } else {
    nn_dir <- loc
    out <- list(readLAS(paste0(nn_dir, "/Normalized.las")),
                readLAS(paste0(nn_dir, "/Ground_Classification.las")),
                readLAS(paste0(nn_dir, "/De-Noised.las")))

    return(out) # output
  }
}

gen_plots <- function (pc, plt_list)
{
  nl <- pc[1]
  gnd <- pc[2]
  cn <- pc[3]

  # Generate specified subplots
  # Generate specified subplots
  cross_plt <- as.logical(plt_list[1])
  dtm_plt <- as.logical(plt_list[2])
  n_norm_plt <- as.logical(plt_list[3])
  norm_plt <- as.logical(plt_list[4])
  rgb_plt <- as.logical(plt_list[5])

  if (cross_plt == TRUE) {
    message(green("\n[INFO] ", Sys.time(), "Plotting cross section"))
    plot_crossection(nl, colour_by = factor(Classification))
  }
  if (dtm_plt == TRUE) {
    message(green("\n[INFO] ", Sys.time(), "Plotting digital terrian model"))
    dtm_plt <- (rasterize_terrain(gnd, res = 1, tin()))

    ggsave(path = sd_path, "dtm_plot.png", dtm_plt)
    # plot_dtm3d(dtm_plt, bg = "white", main = "Terrain Model")
  }
  if (n_norm_plt == TRUE) {
    message(green("\n[INFO] ", Sys.time(), "Plotting non-normalized point cloud (Heat map colouring)"))
    plot(cn, bg = "white", main = "Non-normalized Point Cloud")
  }
  if (norm_plt == TRUE) {
    message(green("\n[INFO] ", Sys.time(), "Plotting normalized point cloud (Heat map colouring)"))
    plot(nl, bg = "white", main = "Normalized Point Cloud")
  }
  if (rgb_plt == TRUE) {
    message(green("\n[INFO] ", Sys.time(), "Plotting point cloud (RGB colouring)"))
    plot(nl, color = "RGB", bg = "white", size = 3, main = "RGB Point Cloud")
  }
}

nn_list <- noise_norm(subset)
gen_plots(nn_list, plt_list)

#
# # ===========================================================================================
# # POINT CLOUD POST-PROCESSING
# # ===========================================================================================

# las <- nn_list[1]
# c <- height.colors(25)
# # Tune base_res. May change depending on subset
# base_res <- (6 / density(nnsub))
# base_res <- (6 / density(nnsub)) + 0.1
#
# # This function will create the window size based off of the tree height
# variable_ws <- function(x) {
#   y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
#   y[x < 2] <- 3
#   y[x > 20] <- 5
#   return(y)
# }
#
# # Pitfree with and subcircle tweak
# chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(
#   subcircle = 0.15,
#   thresholds = c(0,5,10,15,20,25,30,35,40),
#   max_edge = c(0, 2)
# ), pkg = "terra")
#
# ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(variable_ws))
#
# plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = c)
# plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)
#
# algo <- dalponte2016(chm_pitfree_05_2, ttops_chm_pitfree_05_2)
# las <- segment_trees(las, algo) # segment point cloud
# # plot(las, bg = "white", color = "treeID") # visualize trees
#
# # Testing displaying ID's
# # trees_clean <- subset(las, !is.na(las@data$treeID))
# # plot(trees_clean, bg = "white", color = "treeID") # visualize trees
# # text(trees_clean@data$X, trees_clean@data$Y, labels = trees_clean@data$treeID, cex = 0.8, pos = 1)
#
#
# crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")
# q_val <- quantile(crowns$convhull_area, 0.12)
# crowns <- crowns[crowns$convhull_area >= q_val,]
# # plot(crowns["convhull_area"], main = "Crown area (concave hull)", col = viridis(10))
#
# # ======Crowns data frame manipulation and export for manual labelling======
# #TODO: Remove all ct lines. work with oroginal crowns DF
# ct <- crowns
#
# ct$species <- 0 # Add empty column to make manual work easier
#
# ct_export <- ct[, c("treeID", "species")] # Select only treeID and species columns
# ct_export$geometry <- NULL # Delete geometry column (this was somehow staying included in previous line)
#
# write.csv(ct_export, file = "segment_list.csv", row.names = FALSE) # Write to .csv for manual identification
#
# # Import csv and add column to data frame after manual labelling is completed.
# labelled <- read.csv("segment_list.csv")
#
# ct$species <- labelled$species[match(ct$treeID, labelled$treeID)]
#
# # Below are examples of how to manipulate data frame data in R
# # ct$species[ct$treeID == 2100] <- 1 # updating species value of treeID 4
# # ct$species[ct$treeID %in% c(3, 5, 7)] <- 3 # updating all rows with treeID 3, 5, 7 to 3
#
# # ========================================================
#
# # Overlay tree segments to corresponding rgb orthomosaic section
# trees_sp <- spTransform(as_Spatial(crowns), crs(ortho))
# ortho_cropped <- crop(ortho, extent(subset))
#
# png(filename= "cropped_overlay.png", width = 29812, height = 33605, res = 300, units = "px") # initialize png output for plot
#
# plotRGB(ortho_cropped, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
# plot(trees_sp, border = "red", lwd = 1.5, bg = "transparent")
# text(trees_sp, labels = trees_sp@data$treeID, cex = 1.1, col = "red")
# dev.off() # save png after plotting data
# # *** Above lines work for overlaying, but output rgb image at low resolution ***
#
#
#
# library(stars)
#
# trees_sp <- spTransform(as_Spatial(crowns), crs(ortho))
# cropped <- crop(ortho, trees_sp)
#
# m <- as.data.frame(cropped, xy = TRUE)
# merged <- terra::merge(trees_sp, m)
#
#
# png(filename="res_test_2.png", width = 6000, height = 6000, units = "px") # initialize png output for plot
# # plot(o_stars, rgb = 1:3)
# plotRGB(r)
# dev.off()
#
# cropped_stars <- st_as_stars(cropped)
#
# plot(cropped_stars)
#
# trees_stars <- st_as_stars(trees_sp, crs = st_crs(cropped_stars))
#
#
#
# png(filename="cropped_ortho1.png", width = 29812, height = 33605, res = 300, units = "px") # initialize png output for plot
#
# plot(cropped_stars)
#
# dev.off()
#
# plot(cropped_stars, rgb = 1:3, alpha(0.5))
# plot(trees_sp, border = "red", lwd = 1.5, add = TRUE)
# # text(trees_sp, labels = trees_sp@data$treeID, cex = 1.1, col = "red")
#
#
# dev.off()
#
# plotRGB(cr, r = 1, g = 2, b = 3)
#
# plot(cropped_stars, rgb = 1:3, add = FALSE)
# # plotRGB(cropped, r = 1, g = 2, b = 3, stretch = "lin", add = FALSE)
# # plot(trees_sp, add = TRUE, border = "red", lwd = 1.5)
# # text(trees_sp, labels = trees_sp@data$treeID, cex = 1.1, col = "red")
#
# # plotRGB(ortho_r)
#
# dev.off()
#
#
#
#
# # =====Testing merging polygons with rgb for ML algo data=====
# # labeled <- merge(ortho_rgb_cropped, trees_sp)
# trees_rast <- rasterize(trees_sp, ortho_cropped, field = 1)
#
# st_crs(trees_sp) <- st_crs(ortho_cropped)
#
# sv_trees <- as(trees_sp, "SpatVector")
# vals <- terra::extract(ortho_cropped, sv_trees)
#
#
# terra::writeRaster(labeled, "labelled_rgb_test_1.png")
#
#
# # ===================================================================
# metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
# head(metrics)
#
# # plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max
#
# plot(metrics["z_max"], pal = hcl.colors)
#
# tree110 <- filter_poi(las, treeID == 282)
# plot(tree110, bg = "white")