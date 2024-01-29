
# Used for drs_read
necessary_files <- data.frame(
  name = c("dd", "data", "hash"),
  style = c(
    "DRS_data_dictionary_*_.xlsx",
    "DRS_*.csv",
    "DRS_*.hash.txt"
  ),
  regex = c(
    "^DRS(\\_|\\s)data(\\_|\\s)dictionary(\\_|\\s).+\\.xlsx$",
    "^DRS(\\_|\\s).+\\.csv$",
    "^DRS(\\_|\\s).+\\.hash\\.txt$")
)

# usethis::use_data(necessary_files, internal = T, overwrite = T)
