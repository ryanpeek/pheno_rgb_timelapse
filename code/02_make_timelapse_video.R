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

site_id <- "MABE4" # location

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

# read in the exif metadata (run via 01_rename_extract_metadata)
# read in the "complete" option if it exists for exif metadata
if(file_exists(glue("{exif_path}/pheno_exif_{site_id}_complete.csv.gz"))){
  print("Using 'complete' list of photos")
  photo_exif <- read_csv(glue("{exif_path}/pheno_exif_{site_id}_complete.csv.gz"))
} else({
  print("Using local photo directory")
  photo_exif <- read_csv(glue("{exif_path}/pheno_exif_{site_id}_{photo_date_dir}.csv.gz"))
})

# Get Photo Stack ---------------------------------------------------------

# make img composite for video
get_photo_stack <- function(photo_paths, scale_w_h){
  purrr::map(photo_paths, ~image_read(.x) %>%
               image_scale(scale_w_h)) %>%
    image_join()
}

# Full Period (can be big) ----------------------

photo_exif_period <- photo_exif
range(photo_exif_period$datetime)

# Filter to Period or Time --------------------------------------------------

## filter to photos that fall at 12:00
filt_time <- "12:00:00"

photo_exif_period <- photo_exif |>
  # this filters to only photos at noon
  filter(hms(glue("{filt_time}"))==hms::as_hms(datetime))
range(photo_exif_period$datetime)

# or filter to specific time period:
range(photo_exif$datetime)

# give specific date or month
date_start <- ymd("20250220")
date_end <- ymd("20250420")
# time range
time_start <- "11:00:00"
time_end <- "13:00:00"

photo_exif_period <- photo_exif |>
  filter(as_date(datetime) >= date_start & as_date(datetime) <= date_end) |>
  filter(hms::as_hms(datetime) >= hms(time_start) & hms::as_hms(datetime) <= hms(time_end))

range(photo_exif_period$datetime)

# Get photo composite ----------------------------------------------------

# FOR ONE SINGLE FOLDER/PHOTO SET
#photo_stack <- get_photo_stack(glue("{photo_directory}/{photo_exif_period$pheno_name}"), scale_w_h = "800x560")
#beepr::beep()

# FOR FULL DATASET (ALL FOLDERS)
photo_stack_complete <- get_photo_stack(glue("{exif_path}/{photo_exif_period$file_folder}/{photo_exif_period$pheno_name}"), scale_w_h = "800x560")
beepr::beep()


# check image
#photo_stack[nrow(photo_exif_noon)]

# 6. Save to video ----------------------------------------------------------

fs::dir_create(glue("{exif_path}/videos"))

# video name
#vid_name <- glue("{gsub('-','', date_start)}_{gsub('-','', date_end)}_15min") # or "complete" or "photo_date_dir
vid_name <- "complete_f20"
framerate_sel <- 20 # default 12, try 20 for smoother video but need more photos

# makes a high quality video of complete period
image_write_video(image = photo_stack_complete, path = glue("{exif_path}/videos/{site_id}_{vid_name}_video.mp4"), framerate =  framerate_sel)

# makes a high quality video (try framerate 12, 20, or 30)
image_write_video(image = photo_stack, path = glue("{exif_path}/videos/{site_id}_{photo_date_dir}_video_20.mp4"), framerate =  20)

## Save to GIF --------------------------------------------------------------

# this should only be for small datasets, otherwise too large
#image_write_gif(image = photo_stack, delay = 1/12, glue("figs/{cam_id}_{cam_name}_{cam_deploy}.gif"))

