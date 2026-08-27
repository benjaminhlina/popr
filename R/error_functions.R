#' Error functions
#'
#' These functions produce error message displayed to the user.
#'
#'
#' @param df a `data.frame`
#' @param arg_name The name of the argument
#'
#' @name error_functions
#' @keywords internal

error_df <- function(df, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }
  if (!(inherits(df, "data.frame"))) {
    cli::cli_abort(
      c(
        "{arg_name} is not {.class data.frame}",
        "x" = "Please provide {arg_name} with a {.class data.frame}"
      )
    )
  }
}

#'
#' @param path The file path to a file.
#'
#' @name error_functions
#' @keywords internal

error_empty_file <- function(path) {
  cli::cli_abort(
    c(
      "The file did not contain any objects.",
      "x" = "File path: {.file {path}}"
    ),
  )
}

#' @name error_functions
#' @keywords internal
error_load <- function(path) {
  cli::cli_abort(
    c(
      "Failed to read {.arg path} as a raster.",
      "x" = "{.file {path}} could not be loaded by {.fn terra::rast}.",
      "i" = "{.arg path} was type {.cls character}.",
      "i" = "Original error: {conditionMessage(e)}"
    ),
    class = "bathy_rast_read_failed"
  )
}

#' @param vec a `vector` to check if numeric
#' @name error_functions
#' @keywords internal

error_numeric <- function(num, arg_name = NULL) {
  if (!(inherits(num, "numeric"))) {
    cli::cli_abort(
      c(
        "{arg_name} needs to be a numeric.",
        "x" = "Please supply {arg_name} is a {.class {numeric}}"
      )
    )
  }
}

#' @name error_functions
#' @keywords internal

error_path <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(
      c(
        "File not found.",
        "x" = "No such file: {.file {path}}",
        "i" = "Please provide the correct path to file you are wanting to use "
      ),
    )
  }
}

#' @param raster a raster object to assess
#' @name error_functions
#' @keywords internal

error_raster <- function(raster) {
  cli::cli_abort(
    c(
      "{.arg raster} must be a {.cls SpatRaster}, {.cls RasterLayer}, matrix, or a file path to one of those.",
      "x" = "You supplied an object of class {.cls {class(raster)}}.",
      "i" = "{.arg raster} had {.field typeof}: {.val {typeof(raster)}}."
    ),
    class = "bathy_invalid_input"
  )
}

#' @param spatrast a raster object to assess
#' @name error_functions
#' @keywords internal

error_spatrast <- function(spatrast, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }

  if (!(inherits(spatrast, "SpatRaster"))) {
    cli::cli_abort(
      c(
        "{arg_name} is not {.class SpatRaster}",
        "x" = "Please provide {arg_name} with a {.class SpatRaster}"
      )
    )
  }
}
