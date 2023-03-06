# Required Packages
library(lidR)
library(sf)
library(ggplot2)
library(raster)
library(viridis)
library(BiocManager)
library(lidR.li2012enhancement)

# Install custom package
# install.packages("BiocManager")
# BiocManager::install("EBImage")
# devtools::install_github("mcoghill/lidR.li2012enhancement")

# las <- readLAS("C:/Users/Jorda/OneDrive/Documents/University/Comp 3710/AuroraUAV/points.las",
#                select = "xyzr",
#                filter = "-drop_z_below 0")
#
# ttops <- locate_trees(las, lmfxauto())
#
# x <- plot(las)
# add_treetops3d(x, ttops)

# To change the memory limit of R virtual memory
# library(usethis)
# usethis::edit_r_environ()

las <- readLAS("C:/Users/Jorda/OneDrive/Documents/University/Comp 3710/AuroraUAV/points.las",
               select = "xyzrn",
               filter = "-drop_z_below 0")

# las_check(las)

# print(las)

# plot(las)

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

mycsf <- csf(sloop_smooth = TRUE, class_threshold = 2, cloth_resolution = 1.5, time_step = 1)
las <- classify_ground(las, mycsf)
plot_crossection(las, colour_by = factor(Classification))

gnd <- filter_ground(las)
# plot(gnd, size = 3, bg = "white")

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
# plot_dtm3d(dtm_tin, bg = "white")

dtm <- rasterize_terrain(las, 1, knnidw())
# plot(dtm, col = gray(1:50/50))

nlas <- normalize_height(las, knnidw())
# plot(nlas, size = 4, bg = "white")

hist(filter_ground(nlas)$Z, breaks = seq(-50, 50, 0.01), main = "", xlab = "Elevation")

col <- height.colors(25)
chm <- rasterize_canopy(nlas, res = 0.5, pitfree(thresholds = c(0, 60), max_edge = c(0, 1.5)))
# plot(chm, col = col)

las <- filter_poi(nlas, Z >= 0)

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

algo <- dalponte2016(chm_p2r_1_smoothed, ttops_chm_p2r_1_smoothed)
las <- segment_trees(las, algo) # segment point cloud
# plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

# metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
# head(metrics)

# plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

tree110 <- filter_poi(las, treeID == 1438)
plot(tree110, size = 8, bg = "white")
