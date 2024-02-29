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
# These values provided in MSG report on Weights.
zone_N <- data.frame(
  Zone = factor(c(1, 2, 3)),
  N = c(11727, 540340, 166085)
)

# Composite Index Frame
# used in load_data > cdrs_composite_index
composite_frame <- data.frame(
  title = c(
    "Sense of Place",
    "Place Attachment",
    "Place Identity & Meaning",
    "Place dependence",
    "Environmental Impacts Experienced",
    "Overall Level of Climate Concern",
    "Adaptive Capacity",
    "Social Capital",
    "Civic Engagement"
  ),
  index_name = c(
    "idx_sop",
    "idx_pa",
    "idx_pim",
    "idx_pd",
    "idx_eie",
    "idx_olcc",
    "idx_ac",
    "idx_sc",
    "idx_ce"
  ),
  regex = c(
    "^Q(3_[2345678])|(1_[12345])$",
    "^Q3_(5|2|1)$",
    "^Q(3_(7|8|6))|(1_3)$",
    "^Q(3_(3|4))|(1_(2|4|5))$",
    "Q12_[1234567]",
    "Q13",
    "Q24_(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)",
    "Q40_(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17)$",
    "Q42(a|b|c|d|e|f|g|h)$"
  ),
  # ordered levels, comma separated values
  # helps both re-order variables based on perceived scale (eg. Very Unlikely
  # is now 0); and removes unwanted levels (in this case Uncertainty.)
  ordered_levels_csv = c(
    # "Sense of Place",
    NA_character_,
    # "Place Attachment",
    NA_character_,
    # "Place Identity & Meaning",
    NA_character_,
    # "Place dependence",
    NA_character_,
    # "Environmental Impacts Experienced",
    NA_character_,
    # "Overall Level of Climate Concern",
    paste0(c("Not at all concerned",
             "Somewhat concerned",
             "Moderately concerned",
             "Very concerned"),
           collapse = ","),
    # "Adaptive Capacity",
    NA_character_,
    # "Social Capital",
    NA_character_,
    # "Civic Engagement"
    paste0(c("Very unlikely",
             "Somewhat unlikely",
             "Somewhat likely",
             "Very likely"),
           collapse = ",")
  ),
  ncol = c(
    # "Sense of Place",
    12,
    # "Place Attachment",
    3,
    # "Place Identity & Meaning",
    4,
    # "Place dependence",
    5,
    # "Environmental Impacts Experienced",
    7,
    # "Overall Level of Climate Concern",
    7,
    # "Adaptive Capacity",
    15,
    # "Social Capital",
    17,
    # "Civic Engagement"
    8
  )
)

# errata ----
# This is potential errata for collaborator data.
# TODO conditionally weave this into cdrs_read().
# However, these variables are not present in "public" data set.
# So it is not a priority.
dict_errata <- tibble::tibble(
  Variable = c("Q5", "Q11", "Q25"),
  name = c("Label", "Label", "Label"),
  value = c(
    "5. In one or two short sentences, please describe the Delta Region as you would to someone who is not familiar with it.",
    "11. When you imagine life in the Delta one generation from now (approximately 25 years), what do you hope it looks like? Please provide 1-2 short sentences.",
    "25. If you have any additional thoughts on community and environmental wellbeing in the Delta, please write them here."

  ),
  encoding = NA,
  frequency = NA,
  percent = NA
)

# generate internal data ----
usethis::use_data(necessary_files, zone_N, composite_frame, internal = T, overwrite = T)
