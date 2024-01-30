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
    "^DRS(\\_|\\s).+\\.hash\\.txt$"
  )
)

# Used (potentially) for fpc
zone_N <- data.frame(
  Zone = factor(c(1, 2, 3)),
  N = c(11727, 540340, 166085)
)

usethis::use_data(necessary_files, zone_N, internal = T, overwrite = T)
