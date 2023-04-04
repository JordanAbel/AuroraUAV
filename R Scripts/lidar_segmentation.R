# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

# To change the memory limit of R virtual memory
library(usethis)
usethis::edit_r_environ()

las <- readLAS("../RGB Data/points.las",
               select = "xyzr",
               filter = "-drop_z_below 0")
las_check(las)

chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))
plot(las, bg = "white", size = 4)

ttops <- locate_trees(las, lmf(ws = 5))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

x <- plot(las, bg = "white", size = 4)
add_treetops3d(x, ttops)

f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 1090] <- 3
  y[x > 1110] <- 5
  return(y)
}

heights <- seq(-5,30,0.5)
ws <- f(heights)
plot(heights, ws, type = "l",  ylim = c(0,5))

# Point-to-raster 2 resolutions
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
chm_pitfree_05_1 <- rasterize_canopy(las, 0.5, pitfree(), pkg = "terra")
chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
ttops_chm_pitfree_05_1 <- locate_trees(chm_pitfree_05_1, lmf(5))
ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

par(mfrow=c(3,2))
col <- height.colors(50)
plot(chm_p2r_05, main = "CHM P2R 0.5", col = col); plot(sf::st_geometry(ttops_chm_p2r_05), add = T, pch =3)
plot(chm_p2r_1, main = "CHM P2R 1", col = col); plot(sf::st_geometry(ttops_chm_p2r_1), add = T, pch = 3)
plot(chm_p2r_05_smoothed, main = "CHM P2R 0.5 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_05_smoothed), add = T, pch =3)
plot(chm_p2r_1_smoothed, main = "CHM P2R 1 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_1_smoothed), add = T, pch =3)
plot(chm_pitfree_05_1, main = "CHM PITFREE 1", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_1), add = T, pch =3)
plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
head(metrics)

plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

custom_crown_metrics <- function(z, i) { # user-defined function
  metrics <- list(
     z_max = max(z),   # max height
     z_sd = sd(z),     # vertical variability of points
     i_mean = mean(i), # mean intensity
     i_max  = max(i)   # max intensity
   )
   return(metrics) # output
}

ccm <- ~custom_crown_metrics(z = Z, i = Intensity)

metrics <- crown_metrics(las, func = ccm, geom = "convex")
plot(metrics["z_max"], pal = hcl.colors)

#-----------------------Labelling part

trees_sp <- as_Spatial(crowns)

# process_data(ortho, trees_sp)

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



  # code to check accuracy of bounding box
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

