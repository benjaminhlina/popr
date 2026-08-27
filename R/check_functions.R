#' Check functions
#'
#' These functions produce errors, warnings, and information
#' to the user.

check_start <- function(x, t0 = round(Sys.time())) {
  cli::cli_alert_info("Starting {x} likelihood calculation at {.val {t0}}")
  invisible(t0)
}
