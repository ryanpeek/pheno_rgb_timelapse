# merge files into one

# Libraries ---------------------------------------------------------------

library(tidyverse) # various naming/tidying functions
library(glue) # pasting names together
library(janitor) # cleaning filenames
library(fs) # file/directory metadata
library(purrr)

# Set Paths ---------------------------------------------------------------

# CHANGE/CHECK THESE!
site_id <- "MABE4" # Site ID (avoid spaces and special characters)

# Full path to folder where photos are located
# this function helps select the folder and ensures there are images in the folder to use
select_dir <- function(){
  print("Select any image file WITHIN the folder you want to use:")
  dirname(file.choose(new = FALSE))
}

gz_directory <- select_dir() # here select a gz file
gz_directory # double check this is correct!
site_dir <- fs::path_dir(gz_directory) # should be to just the site gz

# List GZ Exif Files ------------------------------------------------------

(gz_files <- fs::dir_ls(gz_directory, regexp = "[0-9]{1}.csv.gz$"))
#(gz_files <- fs::dir_ls(site_dir, regexp = "[0-9]{1}.csv.gz$"))
#(gz_files <- fs::dir_ls(gz_directory, regexp = "[0-9]{3}RECNX.csv.gz$"))

# read one:
gz_dat <- read_csv(gz_files)

# read all into one dataframe
gz_dat <- read_csv(gz_files, id = "gz_file")

# make sure to arrange by DATE
gz_dat <- gz_dat |> arrange(datetime)


# read all in a list if issues with read_csv
gz_dat <- map(gz_files, ~read_csv(.x, id = "gz_file"))
names(gz_dat)
gz_dat <- bind_rows(gz_dat[[1]], gz_dat[[2]])

# check how many per gz file? should match number of photo folders
gz_dat |> group_by(gz_file) |> tally()

# Test paths (we want to get TRUE so we know path is valid)
fs::file_exists(glue("{gz_directory}/{gz_dat$file_folder[1]}/{gz_dat$pheno_name[1]}"))

# test with exif for specific photo
# exiftoolr::exif_read(glue("{site_dir}/{gz_dat$file_folder[1]}/{gz_dat$pheno_name[1]}")) |> clean_names() |> glimpse()


# Write Out ---------------------------------------------------------------

# write out gz file for use
write_csv(gz_dat, glue("{gz_directory}/pheno_exif_{site_id}_complete.csv.gz"))
