#' Likelihood integration functions
#'
#' @param w a `vector` description
#' @param wsd a `vector` containging the standard deviation
#' @param minT a `vector` containing the minmum temperature
#' @param maxT a `vector` containing the maximum temperature
#'
#' @name  likelihood_integration
#' @keywords internal

likint3 <- function(w, wsd, minT, maxT) {
  widx <- !is.na(w)
  wdf <- data.frame(w = as.vector(w[widx]), wsd = as.vector(wsd[widx]))
  wint <- .integrate_dnorm_rows(w[widx], wsd[widx], minT, maxT)
  w <- w * 0
  w[widx] <- wint
  w
}

#. sub and .corr are the same function

#' @param w a `matrix`/`data.frame` with columns: mean, sd, lower bound, upper bound
#' @param widx a logical vector indicating which rows of `w` to integrate
#'
#' @details `likint3_matrix` use to be called `linkint3.sub`
#' @name likelihood_integration
#' @keywords internal
#'
likint3_matrix <- function(w, widx) {
  wdf <- w[widx, ]
  wint <- .integrate_dnorm_rows(wdf[, 1], wdf[, 2], wdf[, 3], wdf[, 4])
  w[, 1] <- 0
  w[widx, 1] <- wint
  w[, 1]
}
