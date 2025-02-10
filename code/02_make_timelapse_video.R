# Load photo metadata and make timelapse video

# Libraries ---------------------------------------------------------------

# these are loaded by the functions (see below)
library(tidyverse) # wrangling data
library(av) # video codecs
library(fs) # file systems
library(glue) # pasting paths
library(magick) # for resizing images
library(purrr) # for looping
library(beepr) # to know when things are done


# Set Parameters ----------------------------------------------------------

site_id <- "TWA3" # location

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
#photo_date_dir <- "20240314" # can specify manually if necessary
exif_path <- fs::path_dir(photo_directory)

# read in the exif metadata (run via 02_extract_metadata)
photo_exif <- read_csv(glue("{exif_path}/pheno_exif_{site_id}_{photo_date_dir}.csv.gz"))

# Get Photo Stack ---------------------------------------------------------

# make img composite for video
get_photo_stack <- function(photo_paths, scale_w_h){
  purrr::map(photo_paths, ~image_read(.x) %>%
               image_scale(scale_w_h)) %>%
    image_join()
}

# Filter -----------------------------------------------------------------

## filter to photos that fall at 12:00
filt_time <- "12:00:00"

photo_exif_noon <- photo_exif |>
  # this filters to only photos at noon
  filter(hms(glue("{filt_time}"))==hms::as_hms(datetime))

# Get photo composite ----------------------------------------------------

# this takes all photos, resizes and stacks
photo_stack <- get_photo_stack(glue("{photo_directory}/{photo_exif$pheno_name_uniq}"), scale_w_h = "800x560")
beepr::beep()

# this takes all photos from the filtered dataset and resizes and stacks
photo_stack <- get_photo_stack(glue("{photo_directory}/{photo_exif_noon$pheno_name}"), scale_w_h = "800x560")
beepr::beep()

# check image
#photo_stack[nrow(photo_exif_noon)]

# 6. Save to video ----------------------------------------------------------

fs::dir_create(glue("{exif_path}/videos"))

# makes a high quality video
image_write_video(image = photo_stack, path = glue("{exif_path}/videos/{site_id}_{photo_date_dir}_video.mp4"), framerate =  12)

## Save to GIF --------------------------------------------------------------

# this should only be for small datasets, otherwise too large
#image_write_gif(image = photo_stack, delay = 1/12, glue("figs/{cam_id}_{cam_name}_{cam_deploy}.gif"))

