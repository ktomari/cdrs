#' @title Conditional warning
#'
#' @description
#' Mitigate repetitive warning messages by only delivering warnings on the first and every 10th repetition.
#'
#' @details
#'
#' @param nm character. The name of the warning message.
#' @param msg character. Warning message.
#' @return NULL This function does not return any value.
warning_c <- function(
    nm,
    msg
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if the environment exists and create it if it doesn't
  if (!exists(".warning_env",
              envir = .GlobalEnv)) {
    .warning_env <- new.env()
    assign(x = ".warning_env",
           value = .warning_env,
           envir = .GlobalEnv)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if `counter` variable exists in warning env.
  if (!exists(nm,
              envir = .warning_env)) {

    current_value <- 1L

    # assign a counter (integer)
    assign(x = nm,
           value = 1L,
           envir = .warning_env)

  } else {
    current_value <- get(nm,
                         envir = .warning_env)
    assign(x = nm,
           value = current_value + 1L,
           envir = .warning_env)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Conditionally (based on frequency) display warning.
  if(current_value == 1 | current_value %% 10 == 0){
    warning(paste0(nm, ". ", msg))
  }

  # Return invisible NULL to indicate no output
  invisible(NULL)
}
