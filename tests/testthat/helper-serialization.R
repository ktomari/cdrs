serialize_survey_design <- function(svy_design) {
  # Begin with a random sample of the data.
  rows_ <- nrow(svy_design$variables)

  rows_ <- withr::with_seed(
    seed = 1,
    code = sample(1:rows_, 5)
    )

  dat1 <- svy_design$variables[rows_,]

  # Get sample of probs
  dat2 <- svy_design$allprob[rows_,]

  # Return the data.frame
  cbind(dat1, probs = dat2)
}
