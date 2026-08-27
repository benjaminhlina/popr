#' Class transformers
#'
#' @param x a `vector` description
#'
#' @name class_transformers
#' @keywords internal

ac <- function(x) {
  return(as.character(x))
}
#' @name class_transformers
#' @keywords internal
an <- function(x) {
  return(as.numeric(ac(x)))
}
#' @name class_transformers
#' @keywords internal
an. <- function(x) {
  return(as.numeric(x))
}


#' Integrate dnorm
#'
#' @param mean the mean value
#' @param sd the sd
#' @param lower lower bounds
#' @param upper upper bounds
#'
#' @name intergration_helper
#' @keywords internal

.integrate_dnorm_rows <- function(mean, sd, lower, upper) {
  mapply(
    function(m, s, lo, hi) {
      stats::integrate(stats::dnorm, lo, hi, mean = m, sd = s)$value
    },
    mean,
    sd,
    lower,
    upper
  )
}
