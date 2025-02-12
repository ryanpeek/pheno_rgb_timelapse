## Create Region of Interest (ROI) for image extraction
## R. Peek, 2024

## Create region of interest (ROI) for phenocam image extraction.
## this helps delineate and create an ROI mask of an existing photo
## to be used in extracting phenometrics from each photo. ROI should
## correspond to a specific vegetation type within a given image that translates
## across all images in the set.

# Libraries ---------------------------------------------------------------

library(tidyverse) # all wrangling
library(glue) # making paths/filenames
library(fs) # file systems
library(terra) # for working with raster images

# Get Photo Directory --------------------------------------------

site_id <- "COWO1" # location

# Full path to folder where photos are located
# this function helps select the folder and ensures there are images in the folder to use
select_dir <- function(){
  print("Select any image file WITHIN the folder you want to use:")
  dirname(file.choose(new = FALSE))
}

# select an image from inside the folder of interest
photo_directory <- select_dir()
photo_directory

# Read Photo Metadata -----------------------------------------------------

# create path to drive location:
photo_date_dir <- basename(photo_directory)
#photo_date_dir <- "20240604" # can specify manually if necessary
exif_path <- fs::path_dir(photo_directory)

# read in the exif metadata (run via 02_extract_metadata)
photo_exif <- read_csv(glue("{exif_path}/pheno_exif_{site_id}_{photo_date_dir}.csv.gz"))

# Select Photo for Drawing ROI -------------------------------------------------------

# get a test image, change number for different image
img <- terra::rast(glue("{photo_directory}/{photo_exif$pheno_name[11]}"))

# flip?
img <- terra::flip(img)

# plot
terra::plotRGB(img) # ignore projection warning

# set bands
RGB(img) <- 1:3

# if on osx, need to start quartz window:
#quartz()
#terra::plotRGB(img) # ignore projection warning

# Draw ROI Polygon -----------------------------------------------------

# USE ONE OF THESE ABBREVATIONS
## FROM Richardson et al
# https://www.nature.com/articles/sdata201828
# AG agriculture
# DB deciduous broadleaf
# DN deciduous needleleaf
# EB evergreen broadleaf
# EN evergreen needleleaf
# GR grassland
# MX mixed vegetation (generally EN/DN, DB/EN, or DB/EB)
# SH shrubs
# TN tundra (includes sedges, lichens, mosses, etc.)
# WT wetland
# WA water

# Create NEW mask type and number
## First number is for photo set (specific to download batch)
## second number is iterative to type (if multiple ROI of type: SH created)
mask_type <-"WA_01_01"

# draw a polygon function
drawPolygon <- function (col = "#80303080", lty = 1, ...)
{
  xy <- locator(2)
  lines(xy$x, xy$y, lty = lty)

  while(is.list(c1 <- locator(1))) {
    xy$x <- c(xy$x, c1$x)
    xy$y <- c(xy$y, c1$y)
    lines(xy$x, xy$y, lty = lty)
  }
  xy <- data.frame(xy)
  xy <- rbind(xy, xy[1, ])
  polygon(xy$x, xy$y, lty = lty, col = col, ...)

  invisible(xy)
}

## one click creates a net that starts the drawing, HIT ESCAPE WHEN DONE TO SAVE!
x <- drawPolygon()
x_poly <- terra::vect(cbind(x$x, x$y), "polygons")
plot(x_poly, col=alpha("yellow", 0.6), add=TRUE)
# ext(x_poly)

# Rasterize ROI --------------------------------------------------

# matches extent of photo so rasterize and set everything to 1 or 0
r <- terra::rasterize(x_poly, img)
summary(r)
r[is.na(r)] <- 0
plot(r) # preview mask plot

# Write out ROI Mask -----------------------------------------------------------

fs::dir_create(path = glue("{exif_path}/ROI/"))
terra::writeRaster(r, filename = glue("{exif_path}/ROI/{site_id}_{mask_type}.tif"), overwrite=TRUE)
terra::writeRaster(r, filename = glue("ROI/{site_id}_{mask_type}.tif"), overwrite=TRUE)

# Make a Plot -------------------------------------------------------------

# make sure to save and write out (only once)
png(filename = glue("{exif_path}/ROI/{site_id}_{mask_type}_roi_masked.png"), bg = "transparent")
plotRGB(img)
plot(r, col=c(alpha("white", 0), alpha("yellow",0.8)), legend=FALSE, axes=FALSE, add=TRUE)
dev.off()

png(filename = glue("ROI/{site_id}_{mask_type}_roi_masked.png"), bg = "transparent")
plotRGB(img)
plot(r, col=c(alpha("white",0), alpha("yellow",0.8)), legend=FALSE, axes=FALSE, add=TRUE)
dev.off()

# Quick Check -------------------------------------------------------------

# read in and plot
r_in <- rast(glue("{exif_path}/ROI/{site_id}_{mask_type}.tif"))
r_poly <- as.polygons(r_in) # make polygons

# filter to 1 (only the masked region)
r_poly[2]
terra::plotRGB(img) # ignore projection warning
plot(r_poly[2], add=TRUE, col="yellow2")
#dev.off()
#graphics.off()
