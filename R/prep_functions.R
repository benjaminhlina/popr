bathy <- load_bathy_raster(
  "/Users/benhlina/Library/CloudStorage/Dropbox/Dal-Post Doc/data/Geolocation for Jena/bathy_grid/bathy.mat.gsl.ssgb.lon72.41.lat41.52.RData"
)

prep_bathy_likdepth <- function(
  bathy, # SpatRaster, RasterLayer, matrix, or path to .RData/.tif
  weighted = FALSE,
  focal_dim = 9,
  ctd.dir = NULL # only required when weighted = TRUE
) {
  error_spatrast(bathy)

  # ---- local bathymetric roughness (sd within a moving window) ----
  if (weighted) {
    if (is.null(ctd.dir)) {
      cli::cli_abort("ctd.dir must be supplied when weighted = TRUE")
    }
    if (!focal_dim %in% c(3, 5, 9)) {
      cli::cli_abort(
        "pre-computed sd matrices only exist for focal_dim = 3, 5, or 9"
      )
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
      bathy,
      w = focal_dim,
      fun = "sd",
      na.rm = TRUE
    )
    sdi <- terra::as.matrix(sd_r, wide = TRUE)

    ssd <- as.vector(sdi)
    fill_sd0 <- stats::quantile(ssd[ssd != 0], probs = 0.01, na.rm = TRUE)
    if (is.na(fill_sd0)) {
      fill_sd0 <- 1e-3
    }
    sdi[sdi == 0] <- fill_sd0
    sdi[is.na(sdi)] <- fill_sd0
  }

  list(
    bathy_r = bathy_r, # SpatRaster, in case downstream needs extent/crs/res

    sdi = sdi, # local bathymetric sd matrix (same dims as zbi)
    idx = idx, #
    idx_na = idx,
    focal_dim = focal_dim,
    weighted = weighted
  )
}

# ---- helper: accept SpatRaster / RasterLayer / matrix / path, always return SpatRaster ----

load_bathy_raster(
  "/Users/benhlina/Library/CloudStorage/Dropbox/Dal-Post Doc/data/Geolocation for Jena/bathy_grid/bathy.mat.gsl.ssgb.lon72.41.lat41.52.RData"
)

# ---- per-fish/per-day likelihood: now just consumes prepped bathy ----
calc_likdepth_transmit <- function(
  pdt,
  bathy_prep, # output of prep_bathy_likdepth()
  ncores = 4,
  daily_sd_depth = 0.57
) {
  zbi <- bathy_prep$zbi
  sdi <- bathy_prep$sdi
  inds <- bathy_prep$inds

  # ... likelihood computation against pdt using zbi / sdi / daily_sd_depth ...
}
