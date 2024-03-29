## code to prepare `demo_csv_data` dataset goes here
library(tidyverse)
# library(readxl)
# library(openxlsx)
library(digest)
library(withr)

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

cwd <- get_package_path("cdrs")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Where is the data?
zip_ <- file.path(cwd, "data-raw", "DRS public data_2023_12_01.zip")

message(str_glue("Loading data with cdrs_read: {zip_}"))
# Load the (internal?) data
# load(file.path(cwd, "R", "sysdata.rda"))
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
# this value identifies the greatest number of categories possible per
# categorical variables.
nmax <- 8
# This value indicates how many rows of demo data we want.
nrows <- 200

withr::with_seed(
  seed = 062016,
  code = {
    # begin extracting data.
    dat2 <- map2_dfc(dat, names(dat), function(col_, nm_) {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # get unique values
      uniq_ <- unique(col_)

      # get missingness values first
      missing_ <- uniq_[str_which(uniq_, "\\<.+\\>")]

      # get all other values
      normal_ <- uniq_[str_which(uniq_, "\\<.+\\>", negate = T)]

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(nm_ == "DRS_ID") {
        output <- paste0(
          "DRS",
          2295:(2294 + nrows)
        )
      } else if (length(uniq_) > nmax) {
        # LARGE NUMBER (either Q1a or WTFINAL)

        if (length(missing_) > 0) {
          # If it contains missing values...

          # sample `normal_` values.
          # In this case, we primarily want to ensure that
          # at least one of each
          # <missingness> values are recorded.
          # So we'll leave space for each value of from `missing_`.
          # Chances are, we'll get several, depending on the size of nrows.
          normal_ <- sample(
            x = uniq_,
            size = {
              nrows - length(missing_)
            },
            replace = T
          )

          # randomize
          output <- sample(
            x = c(missing_, normal_),
            size = nrows,
            replace = F
          )
        } else {
          # If it is neither DRS_ID, nor contains <Missingness> values,
          # then just sample it.
          output <- sample(col_, size = nrows, replace = T)
        }
      } else if (length(uniq_) == nmax) {
        # EXACTLY 8 VALUES (ie. levels/categories)
        output <- sample(uniq_, size = nrows, replace = T)
      } else if (length(uniq_) < nmax) {
        # LESS THAN 8 VALUES

        # lets get a random sample + at least 1 copy of each unique value.
        additional <- sample(uniq_, size = {nrows - length(uniq_)}, replace = T)
        output <- sample(c(uniq_, additional), size = nrows)
      }

      # return
      output
    })
  }
)

# Where do we want to write files? ----
message("Beginning section to write files.")
# Set the path to the inst/extdata directory relative to this script
# Assuming this script is in the data-raw directory at the package root
demo_path <- file.path(cwd, "inst", "extdata", "demo")

# Create the extdata/demo directory if it doesn't exist
if (!dir.exists(demo_path)) {
  message("Creating extdata dir structure.")
  dir.create(demo_path, recursive = TRUE)
}

# Write to File ----
# Let's give it same date as our zip file.
date_ <- basename(zip_) %>%
  str_extract("\\d{4}\\_\\d{2}\\_\\d{2}")

output_paths <- list(
  csv = str_glue("DRS_demo_{date_}.csv"),
  hash = str_glue("DRS_demo_{date_}.hash.txt"),
  xlsx = str_glue("DRS_data_dictionary_demo_{date_}.xlsx")
)

## Write to File ----
write_csv(
  x = dat2,
  file = file.path(demo_path, output_paths$csv)
)

## Write Hash ----
write(
  x = digest(
    object = file.path(demo_path, output_paths$csv),
    algo = "sha256",
    file = T
  ),
  file = file.path(demo_path, output_paths$hash)
)

## Write Data Dictionary ----
file.copy(
  from = dd,
  to = file.path(demo_path, output_paths$xlsx)
)

## Write to Zip ----
with_dir(demo_path, {
  zip(zipfile = str_glue("DRS_demo_{date_}.zip"),
      files = output_paths %>% unlist())
})

# move zip file up one dir.
file.copy(
  from = file.path(demo_path,
                   str_glue("DRS_demo_{date_}.zip")),
  to = file.path(cwd,
                 "inst",
                 "extdata",
                 str_glue("DRS_demo_{date_}.zip"))
)

file.remove(file.path(demo_path,
                      str_glue("DRS_demo_{date_}.zip")))

unlink(temp_)
