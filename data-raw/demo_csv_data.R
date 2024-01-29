## code to prepare `demo_csv_data` dataset goes here
library(tidyverse)
library(readxl)
library(openxlsx)
library(digest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function to get directory path for package.
get_package_path <- function(pkg_name) {
  # Get the current working directory
  wd <- normalizePath(getwd(), winslash = "/")

  # Split the path into components
  path_parts <- strsplit(wd, "/")[[1]]

  # Find the index of the package name in the path
  pkg_index <- match(pkg_name, path_parts)

  # If the package name is found in the path
  if (!is.na(pkg_index)) {
    # Reconstruct the path up to the package directory
    pkg_path <- paste(path_parts[1:pkg_index], collapse = "/")
  } else {
    # Package not found in path, return NA or an appropriate error/message
    pkg_path <- NA
    warning("Package directory not found in the current path.")
  }

  return(pkg_path)
}

cwd <- get_package_path('cdrs')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Where is the data?
zip_ <- file.path(cwd, "data-raw", "DRS public data_2023_12_01.zip")

message(str_glue("Loading data with cdrs_read: {zip_}"))
# Load the data
load(file.path(cwd, "R", "sysdata.rda"))
dat <- cdrs::cdrs_read(path_ = zip_)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We'll need the data dictionary, so let's load it.
message("Creating temporary directory with unzipped public data.")
# create temporary directory for output
temp_ <- tempdir()
# unzip it to temp dir
unzip(zip_, exdir = temp_)
# get name of dir in tempdir we're interested in.
new_dir <- basename(zip_) %>%
  str_remove("\\.zip")
# get name of data dictionary file (xlsx)
dd <- list.files(file.path(temp_, new_dir), pattern = "\\.xlsx")
# get path to dd
dd <- file.path(temp_, new_dir, dd)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now, we know that variables with > 8 unique values are either:
# some sort of bespoke attribute (either an exclusive ID, or a weight)
# OR a write-in text.
# We want to create a dummy dataset out of this. Each variable will be randomly
# winnowed to a uniform sample of each possible variable type.
# We want the output to be exactly 8 value in length.
message("Creating demo data set.")
nmax <- 8

# begin extracting data.
dat2 <- map2_dfc(dat, names(dat), function(col_, nm_){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get unique values
  uniq_ <- unique(col_)

  # get missingness values first
  missing_ <- uniq_[str_which(uniq_, "\\<.+\\>")]

  # get all other values
  normal_ <- uniq_[str_which(uniq_, "\\<.+\\>", negate = T)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(length(uniq_) > nmax){
    # LARGE NUMBER

    if(nm_ == "DRS_ID"){
      # If its DRS ID, I have a specific output
      output <- paste0("DRS",
                       2295:2302)

    } else if(length(missing_) > 0) {
      # If it contains missing values...

      # sample `normal_` values.
      normal_ <- sample(x = normal_,
                        size = {nmax - length(missing_)},
                        replace = F)

      # randomize
      output <- sample(x = c(missing_, normal_),
             size = nmax)

    } else {
      # If it is neither DRS_ID, nor contains <Missingness> values,
      # then just sample it.
      output <- sample(col_, size = nmax, replace = F)
    }

  } else if(length(uniq_) == nmax){
    # EXACTLY 8 VALUES
    output <- sample(uniq_, size = nmax, replace = F)

  } else if(length(uniq_) < nmax){
    # LESS THAN 8 VALUES

    # lets get a random sample + at least 1 copy of each unique value.
    nmin <- nmax - length(uniq_)
    additional <- sample(uniq_, size = nmin, replace = T)
    output <- sample(c(uniq_, additional), size = nmax)

  }

  # return
  output
})

# Where do we want to write files? ----
message("Beginning section to write files.")
# Set the path to the inst/extdata directory relative to this script
# Assuming this script is in the data-raw directory at the package root
extdata_path <- file.path(cwd, "inst", "extdata", "demo")

# Create the extdata/demo directory if it doesn't exist
if (!dir.exists(extdata_path)) {
  message("Creating extdata dir structure.")
  dir.create(extdata_path, recursive = TRUE)
}

# Write Csv ----
# Let's give it same date as our zip file.
date_ <- basename(zip_) %>%
  str_extract("\\d{4}\\_\\d{2}\\_\\d{2}")

write_csv(
  x = dat2,
  file = file.path(extdata_path, str_glue("DRS_demo_{date_}.csv"))
)

# Write Hash ----
write(
  x = digest(
    object = file.path(extdata_path, str_glue("DRS_demo_{date_}.csv")),
    algo = "sha256",
    file = T
  ),
  file = file.path(extdata_path, str_glue("DRS_demo_{date_}.hash.txt"))
)

# Write Data Dictionary ----
file.copy(
  from = dd,
  to = file.path(extdata_path,
                 str_glue("DRS_data_dictionary_demo_{date_}.xlsx"))
)

unlink(temp_)
