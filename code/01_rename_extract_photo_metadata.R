# extract photo metadata from exif, rename photos uniquely
# Ryan Peek, 2024

## Rename photos sequentially by date with a Site ID (SITE_ID_YYYY_MM_DD_HHMMSS)
## Additionally, code uses exiftools to extract photo attributes for future use
# write these data to csv.gz (info like exposure, photo size, etc)

# Libraries ---------------------------------------------------------------

library(tidyverse) # various naming/tidying functions
library(glue) # pasting names together
library(janitor) # cleaning filenames
library(fs) # file/directory metadata
library(exiftoolr) # getting metadata from photos (REQUIRES A LOCAL ExifTool install)
library(digest) # for unique hash ID

# Getting Exiftools Installed ---------------------------------------------

## this process only needs to be done ONCE per R installation (version)
## Go through steps 1-3 and run lines below the double ##

## 1. Download the ExifTool Windows Executable (stand-alone) version (.zip) from here:

## https://exiftool.org/, look for Windows Executable: exiftool-xx.xx.zip
## Download to default downloads folder (i.e., Downloads/exiftool-12.87.zip)

## 2. create path to the downloaded tool:

## make sure the version number matches what you have
## (different versions should still work, just need the correct one below for the path to work)

## These don't need to be changed, they are set automagically and work on windows
username <- Sys.getenv("USERNAME")
drive <- r'(C:\Users\)'

##  if you put in Downloads, this should work as long as you update version
path_to_exif_zip <- glue("{drive}/{username}/Downloads/exiftool-12.99_64.zip")

## 3. Check path works!

if(fs::file_exists(path_to_exif_zip)=="TRUE") {
  print("Path is legit!")
} else( "Path is borked...double check")

## 4. Now we can test and install

## this only needs to be done once!
# install_exiftool(local_exiftool = path_to_exif_zip)

## Check EXIF works: should get "Using ExifTool version XX.XX" and the version

exif_version()

# Set Paths ---------------------------------------------------------------

# CHANGE/CHECK THESE!
site_id <- "COLE1" # Site ID (avoid spaces and special characters)

# Full path to folder where photos are located
# this function helps select the folder
# and ensures there are images in the folder to process
select_dir <- function(){
  print("Select any image file WITHIN the folder you want to use:")
  dirname(file.choose(new = FALSE))
}

# select ANY IMAGE from the folder you want to process photos
photo_directory <- select_dir()
photo_directory # double check this is correct!

# TYPICALLY THE CODE IS SET UP TO RECOGNIZE FOLDER STRUCTURES AS FOLLOWS
# DIRECTORIES ARE IN ALL CAPS, filenames are lowercase

## TIMELAPSE PROJECT
    ## ----> SITE_ID
        ## ----> YYYYMMDD
            ## ----> photo_filename_0001.jpg
            ## ----> photo_filename_0002.jpg

# Get Photo File List -----------------------------------------------------

# get a complete list of all the photos on the card or in the directory
photo_list <- fs::dir_info(photo_directory, type = "file", recurse = TRUE)

# filter out any videos (AVI):
photo_list <- photo_list |> filter(!fs::path_ext(path)=="AVI")

# add a few vars
photo_list <- photo_list |>
  mutate(
    file_name = fs::path_file(path),
    full_path = path,
    datetime_fs = ymd_hms(modification_time)) |>
  relocate(c(file_name, full_path, datetime_fs), .before="path") |>
  select(-path)

# make sure all unique! (should be zero)
photo_list |> group_by(datetime_fs) |> tally() |> filter(n>1) |> nrow()

# Get Photo METADATA  -------------------------------------------------

## REQUIRES exiftoolr
## with large photo lists, this can take some time (>5 minutes).
## gets metadata used in calculating metrics, takes awhile with lots of photos

system.time(
  if(nchar(exiftoolr::exif_version())>0){ # fails here if exif not installed
    photo_attribs <- exiftoolr::exif_read(photo_list$full_path)  |>
      # reformat and select fields of interest
      janitor::clean_names() |>
      select(any_of(c("directory", "file_name", "file_number",
                      "create_date", "exposure_time", "moon_phase",
                      "ambient_temperature", "ambient_infrared", "ambient_light",
                      "serial_number", "image_size", "image_width", "image_height",
                      "battery_voltage_avg"))) |>
      rename(file_path=directory, datetime = create_date, exposure=exposure_time,
             ambient_temp_C=ambient_temperature) |>
      mutate(
        full_path = glue("{file_path}/{file_name}"),
        datetime = ymd_hms(datetime),
        # add unique pheno_name for image naming
        pheno_name = glue("{site_id}_{format(as_date(datetime), '%Y_%m_%d')}_{gsub(':', '', hms::as_hms(datetime))}.{path_ext(file_name)}"),
        # this also adds unique hash ID for imagery where there may be duplicates
        hashid = map_vec(full_path, ~digest::digest(.x, algo="crc32", serialize=FALSE)),
        pheno_name_uniq = glue("{site_id}_{format(as_date(datetime), '%Y_%m_%d')}_{gsub(':', '', hms::as_hms(datetime))}_{hashid}.{path_ext(file_name)}"))
  } else("No exiftools installed...")
)

# make sure all photos have a unique name! (Below should return zero)
photo_attribs |> group_by(pheno_name_uniq) |> tally() |> filter(n>1) |> nrow()
photo_attribs |> group_by(pheno_name) |> tally() |> filter(n>1) |> nrow()

# PHENO: Write IMG Metadata ----------------------------------------------

# create path to drive location:
metadata_path <- fs::path_dir(photo_directory)
photo_attribs <- photo_attribs |> arrange(datetime)
last_date <- last(format(as_date(photo_attribs$datetime), '%Y%m%d'))

# write out metadata
write_csv(photo_attribs, glue("{metadata_path}/pheno_exif_{site_id}_{last_date}.csv.gz"))

# Rename Photos in Place -----------------------------------------

# For 15 min or greater photo intervals we can rename in place
# as long as photo names are unique!
# this code checks to see all photo names are unique using only datetime stamp
# if not, it appends a unique random hash code to the site_date_time

# CAMERA DEFAULT NAME
# Change if your default model name is different (i.e., "IMG" or "MOULTRIE")
cam_default_img_name <- "RCNX"

if(photo_list |> filter(grepl(glue("^{cam_default_img_name}"), file_name)) |> tally() |> nrow()==0){
  print("Already renamed, no renaming required. Done!")
} else if(photo_attribs |> group_by(pheno_name) |> tally() |> filter(n>1) |> nrow()==0){
  print("No duplicates, using simple phenoname: site_date_time")
  fs::file_move(path = photo_attribs$full_path, new_path = glue("{photo_attribs$file_path}/{photo_attribs$pheno_name}"))
} else {
  print("Duplicates present, appending unique random hash code site_date_time")
  fs::file_move(path = photo_attribs$full_path, new_path = glue("{photo_attribs$file_path}/{photo_attribs$pheno_name_uniq}"))
}


