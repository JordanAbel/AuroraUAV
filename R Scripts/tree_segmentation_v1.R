# Automatically install and load required packages into R session
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


















#-----------------------Labelling part that creates a Yolo dataset(optional, NOT BEING USED IN SEGMENTATION(only for future team reference))

 trees_sp <- as_Spatial(crowns)

 # ortho - full ortho photo, trees_sp - spatial object containign polygons(crowns)
 process_data <- function(ortho, trees_sp, install_dependencies = FALSE) {

   if (install_dependencies) {
     install.packages("grid")
     install.packages("EBImage")
     install.packages("magick")
     install.packages("abind")
     install.packages("ggimage")
   }

   library(grid)
   library(EBImage)
   library(magick)
   library(abind)
   library(ggimage)
   library(sf)
   library(raster)
   library(terra)
   #in this case we are extending the orthoptho with the tree to be multiple of 32 to match yolo image requirenments
   extend_bbox_by_buffer <- function(bbox, buffer, xmin, ymin, xmax, ymax) {
     bbox_extended <- bbox
     bbox_extended["xmin"] <- max(floor((bbox["xmin"] - buffer) / 32) * 32, xmin)
     bbox_extended["ymin"] <- max(floor((bbox["ymin"] - buffer) / 32) * 32, ymin)
     bbox_extended["xmax"] <- min(ceiling((bbox["xmax"] + buffer) / 32) * 32, xmax)
     bbox_extended["ymax"] <- min(ceiling((bbox["ymax"] + buffer) / 32) * 32, ymax)
     return(bbox_extended)
   }

   get_bbox_from_polygon <- function(polygon) {
     min_x <- min(polygon[, 1])
     max_x <- max(polygon[, 1])
     min_y <- min(polygon[, 2])
     max_y <- max(polygon[, 2])

     return(c(min_x, min_y, max_x, max_y))
   }

   # function that creates yolo label for individual photo
   create_yolo_label2 <- function(tree_polygons, ortho, tree_id, output_dir, cropped_bbox, buffer_percentage = 1.6) {
     tree_sp <- tree_polygons[tree_polygons$treeID == tree_id, ]

     tree_bbox <- st_bbox(tree_sp)

     if (!any(is.na(tree_bbox)) && is_bbox_inside_extent(tree_bbox, terra::ext(ortho))) {
       # calculating buffer based on a percentage of the square root of the convhull_area and multiplier that we have to specify since default plygons are way too small
       buffer_distance <- sqrt(tree_sp$convhull_area) * buffer_percentage

       # creating the buffer
       buffered_tree_sp <- st_buffer(tree_sp, dist = buffer_distance)

       tree_coords <- st_coordinates(st_transform(buffered_tree_sp, st_crs(ortho)))

       tree_bbox_corrected <- get_bbox_from_polygon(tree_coords)

       # normalize the coordinates
       tree_coords_normalized <- tree_bbox_corrected
       tree_coords_normalized[1] <- (tree_bbox_corrected[1] - cropped_bbox["xmin"]) / (cropped_bbox["xmax"] - cropped_bbox["xmin"])
       tree_coords_normalized[2] <- (tree_bbox_corrected[2] - cropped_bbox["ymin"]) / (cropped_bbox["ymax"] - cropped_bbox["ymin"])
       tree_coords_normalized[3] <- (tree_bbox_corrected[3] - cropped_bbox["xmin"]) / (cropped_bbox["xmax"] - cropped_bbox["xmin"])
       tree_coords_normalized[4] <- (tree_bbox_corrected[4] - cropped_bbox["ymin"]) / (cropped_bbox["ymax"] - cropped_bbox["ymin"])

       # write to a label file
       label_file <- file.path(output_dir, paste0("tree_", tree_id, ".txt"))
       x_center <- (tree_coords_normalized[1] + tree_coords_normalized[3]) / 2
       y_center <- (tree_coords_normalized[2] + tree_coords_normalized[4]) / 2
       width <- tree_coords_normalized[3] - tree_coords_normalized[1]
       height <- tree_coords_normalized[4] - tree_coords_normalized[2]
       label_line <- paste("0", x_center, y_center, width, height)
       writeLines(label_line, label_file)
     } else {
       cat("Tree with", tree_id, " id, bbox is not valid or outside the ortho extent.\n")
     }
   }


   # labelling the whole thing:
   # splitting the dataset folders
   tree_ids <- unique(trees_sp$treeID)
   split_indices <- sample.int(length(tree_ids), size = length(tree_ids))
   train_ids <- tree_ids[split_indices <= 0.7 * length(tree_ids)]
   valid_ids <- tree_ids[split_indices > 0.7 * length(tree_ids) & split_indices <= 0.85 * length(tree_ids)]
   test_ids <- tree_ids[split_indices > 0.85 * length(tree_ids)]

   # creating folders for train, validation, and test sets
   dir.create("data")
   dir.create("data/train")
   dir.create("data/train/images")
   dir.create("data/train/labels")
   dir.create("data/valid")
   dir.create("data/valid/images")
   dir.create("data/valid/labels")
   dir.create("data/test")
   dir.create("data/test/images")
   dir.create("data/test/labels")

   process_tree <- function(tree_id, ortho, tree_polygons, output_dir) {
     # transforming the tree polygon and computing the bounding box
     transformed_tree_polygon <- st_transform(tree_polygons[tree_polygons$treeID == tree_id, ], st_crs(ortho))
     tree_bbox <- st_bbox(transformed_tree_polygon)

     # adding buffer for cropped image and make dimensions of image multiples of 32
     buffer <- 10 # Adjust this value as needed
     ortho_xmin <- terra::xmin(ortho)
     ortho_ymin <- terra::ymin(ortho)
     ortho_xmax <- terra::xmax(ortho)
     ortho_ymax <- terra::ymax(ortho)
     cropped_bbox <- extend_bbox_by_buffer(tree_bbox, buffer, ortho_xmin, ortho_ymin, ortho_xmax, ortho_ymax)

     # write label info to txt
     create_yolo_label2(tree_polygons, ortho, tree_id, file.path(output_dir, "labels"), cropped_bbox)

     # crop the image
     cropped_image <- crop(ortho, cropped_bbox)

     # save the image
     writeRaster(cropped_image, file.path(output_dir, "images", paste0("tree_", tree_id, ".png")))
   }

   trees_sp_st <- st_as_sf(trees_sp)

   is_bbox_inside_extent <- function(bbox, extent) {
     if (bbox["xmin"] >= extent[1] & bbox["xmax"] <= extent[2] &
         bbox["ymin"] >= extent[3] & bbox["ymax"] <= extent[4]) {
       return(TRUE)
     } else {
       return(FALSE)
     }
   }

   for (tree_id in train_ids) {
     process_tree(tree_id, ortho, trees_sp_st, "data/train")
   }

   for (tree_id in valid_ids) {
     process_tree(tree_id, ortho, trees_sp_st, "data/valid")
   }

   for (tree_id in test_ids) {
     process_tree(tree_id, ortho, trees_sp_st, "data/test")
   }



   generate_yaml <- function(train_dir, valid_dir, test_dir, num_classes, class_names, output_file) {
     yaml_lines <- c(
       paste0("train: ", train_dir),
       paste0("val: ", valid_dir),
       paste0("test: ", test_dir),
       "",
       paste0("nc: ", num_classes),
       paste0("names: ", class_names)
     )

     writeLines(yaml_lines, output_file)
   }

   # generating yaml file
   train_dir <- "../train/images"
   valid_dir <- "../valid/images"
   test_dir <- "../test/images"

   # please specify the num of classes and names of classes here
   num_classes <- 1
   class_names <- "['tree']"

   output_file <- "data.yaml"

   generate_yaml(train_dir, valid_dir, test_dir, num_classes, class_names, output_file)



   # code to check accuracy of bounding box by displaying the labelled image with bounding box
   install.packages("grid")
   library(grid)
   install.packages("EBImage")
   install.packages("magick")
   install.packages("abind")
   install.packages("ggimage")
   library(EBImage)
   library(magick)
   library(abind)

   library(ggimage)
   #paths for image, change to your own
   image_file <- "/Users/andreimarkov/PycharmProjects/yolotest/train/images/tree_1888.png"
   label_file <- "/Users/andreimarkov/PycharmProjects/yolotest/train/labels/tree_1888.txt"
   img <- magick::image_read(image_file)

   label_line <- readLines(label_file)

   # extracting the bounding box coordinates from the label
   label_data <- strsplit(label_line, " ")[[1]]
   class_id <- as.integer(label_data[1])
   x_center <- as.numeric(label_data[2])
   y_center <- as.numeric(label_data[3])
   width <- as.numeric(label_data[4])
   height <- as.numeric(label_data[5])

   # Compute the bounding box
   img_width <- img_info$width
   img_height <- img_info$height
   xmin <- x_center - width / 2
   ymin <- y_center - height / 2
   xmax <- x_center + width / 2
   ymax <- y_center + height / 2

   df <- data.frame(xmin = xmin * img_width, xmax = xmax * img_width, ymin = ymin * img_height, ymax = ymax * img_height)

   #plot the bounding box
   img_ggplot <- ggplot(data = df) +
     annotation_custom(rasterGrob(as.raster(img)), xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
     geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "red", fill = NA, inherit.aes = FALSE) +
     coord_fixed(xlim = c(1, img_width), ylim = c(1, img_height), expand = FALSE) +
     labs(x = NULL, y = NULL) +
     theme_minimal() +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank())

   print(img_ggplot)

 }
