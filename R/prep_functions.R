#' Prepare functions
#'
#' These functions prepare data to then be used in Hidden Markov models
#'
#' @param bathy a `spatrater` generated using `load_bathy_raster(())`
#' @param weighted a `logical` value that determines whther or not sd is weighted. Default is `FALSE`
#' @param focal_dim a `numerical` value that is odd. See details below.
#' @param ctd.dir only required when `weighted = TRUE`. The path to ctd.
#'
#' @details  window used (i.e., `focal_dim`) must have odd dimensions. If you need even sides,
#' you can use a matrix and add a column or row of `NA`'s to mask out values.
#' Window values are typically `1` or `NA` to indicate whether a value is used or ignored in computations, respectively.
#' `NA`` values in w can be useful for creating non-rectangular (e.g. circular) windows.
#' A weights matrix of numeric values can also be supplied to `focal_dim`.
#' In the case of a weights matrix, cells with `NA`` weights will be ignored, and the rest of the values in the
#' focal window will be multiplied by the corresponding weight prior to `sd` being applied.
#'  Note, `na.rm` does not need to be TRUE if `focal_dim` contains `NA`` values as these cells are ignored in computations.
#'
#' @seealso [terra::focal()] [load_bathy_raster()]
#'
#' @export

prep_bathy_likdepth <- function(
  bathy, # SpatRaster, RasterLayer, matrix, or path to .RData/.tif
  weighted = FALSE,
  focal_dim = 9,
  ctd.dir = NULL # only required when weighted = TRUE
) {
  error_spatrast(bathy)
  error_logical(weighted)
  error_focal_dim(focal_dim)

  idx <- attr(bathy, 'idx')
  idx_na <- attr(bathy, 'idx_na')

  # ---- local bathymetric roughness (sd within a moving window) ----
  if (weighted) {
    if (is.null(ctd.dir)) {
      stop("ctd.dir must be supplied when weighted = TRUE")
    }
    if (!focal_dim %in% c(3, 5, 9)) {
      stop("pre-computed sd matrices only exist for focal_dim = 3, 5, or 9")
    }
    sd_path <- file.path(
      ctd.dir,
      paste0("sdcell.n", focal_dim, ".weighted.RData")
    )
    sdi <- local({
      e <- new.env()
      load(sd_path, envir = e)
      e$sdcell
    })
  } else {
    sd_r <- terra::focal(
      bathy_r,
      w = focal_dim,
      fun = "sd",
      na.rm = TRUE
    )
    sdi <- terra::as.matrix(sd_r, wide = TRUE)

    ssd <- as.vector(sdi)
    fill_sd0 <- stats::quantile(ssd[ssd != 0], probs = .01, na.rm = TRUE)
    if (is.na(fill_sd0)) {
      fill_sd0 <- 1e-3
    }
    sdi[sdi == 0] <- fill_sd0
    sdi[is.na(sdi)] <- fill_sd0
  }

  list(
    bathy = bathy,
    sdi = sdi, # local bathymetric sd matrix
    idx = idx,
    idx_na = idx_na,
    focal_dim = focal_dim,
    weighted = weighted
  )
}
