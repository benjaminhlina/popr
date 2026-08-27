#' Load functions
#'
#' These functions load different data into R
#'
#' @param bathy A bathymetric feature to be brought in as a `SpatRaster` object
#'
#' @details returns a `SpatRaster` object to be used with an attribute of index. To access
#' index use `attr(obj, "idx")`.
#' @seealso [terra::rast()]
#'
#' @name load_functions
#' @export

load_bathy_raster <- function(bathy) {
  # general message
  msg_bathy(bathy = bathy)

  if (inherits(bathy, "SpatRaster")) {
    attr(bathy, "idx") <- .valid_idx(bathy)
    return(bathy)
  }

  # use terra to return a spat rast if bahty is raster or matrix
  if (inherits(bathy, "RasterLayer") || is.matrix(bathy)) {
    bathy <- terra::rast(bathy)
    attr(bathy, "idx") <- .valid_idx(bathy)
    return(bathy)
  }

  if (is.character(bathy)) {
    if (grepl("\\.RData$", bathy, ignore.case = TRUE)) {
      error_bathy(bathy)

      e <- new.env()
      load(bathy, envir = e)
      obj_name <- ls(e)

      if (length(obj_names) == 0) {
        error_empty_file(bathy)
      }

      obj <- get(obj_name[1], envir = e)

      msg_load_rdata(obj, obj_name)

      return(load_bathy_raster(obj))
    }

    msg_load(pathy = bathy)

    out <- tryCatch(
      terra::rast(bathy),
      error = function(e) {
        error_load(path = bathy)
      }
    )
    attr(out, "idx") <- .valid_idx(out)
    return(out)
  }
  error_raster(raster = bathy)
}
