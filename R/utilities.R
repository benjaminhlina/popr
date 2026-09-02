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

#' Safe base functions
#'
#' These functions check if x is `NA` then
#' return a stat (e.g, range, min, max)
#'
#' @param x a `numerical` vector
#'
#' @name safe_functions
#' @keywords internal
.safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

#' @param type a `character` determining whether `min()` or `max()` is used
#'
#' @name safe_functions
#' @keywords internal
.safe_range_val <- function(x, type = c("min", "max")) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (type == "min") min(x) else max(x)
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
  # mapply(
  #   function(m, s, lo, hi) {
  #     stats::integrate(stats::dnorm, lo, hi, mean = m, sd = s)$value
  #   },
  #   mean,
  #   sd,
  #   lower,
  #   upper
  # )
  integrated <- stats::pnorm(q = upper, mean = mean, sd = sd) -
    stats::pnorm(q = lower, mean = mean, sd = sd)
  return(integrated)
}

#' Index functions
#'
#' Provides an index number.
#'
#' @param x an object to get a index
#' @name index_functions
#' @keywords internal

.valid_idx <- function(x) {
  vals <- if (inherits(x, "SpatRaster")) terra::values(x)[, 1] else as.vector(x)
  idx_na <- which(is.na(vals))
  idx <- seq_along(vals)
  idx[!idx %in% idx_na]
  return(idx)
}

#' @name index_functions
#' @keywords internal

.valid_idx_na <- function(x) {
  vals <- if (inherits(x, "SpatRaster")) terra::values(x)[, 1] else as.vector(x)
  idx_na <- which(is.na(vals))
  return(idx_na)
}
