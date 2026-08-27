#' Error functions
#'
#' These functions produce error message displayed to the user.
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
