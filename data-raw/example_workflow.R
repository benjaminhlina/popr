# ----- load packages ----

{
  library(popr)
}

# ----- load bathymetry for area ----

bathy <- load_bathy_raster(bathy_matrix)


# ----- prep bathy for likelihood calcualtion -----
preped_bathy <- prep_bathy_likdepth(bathy = bathy, focal_dim = 3)
preped_bathy
# ------ prep log for summary prior to likelihood calculation ------
preped_log <- prep_summary_likdepth(raw_log = pdt, iniloc = iniloc, bin = 3600)

preped_log

# ---- calculaite likeilhoood depth -----
likedepth_transmit <- calc_likdepth_transmit(
  preped_log = preped_log,
  preped_bathy = preped_bathy,
  daily_sd_depth = 0.57
)

ldt_dims <- dim(likedepth_transmit)

days <- seq(1, 2, by = 1)


days |>
  purrr::walk(
    ~ fields::image.plot(likedepth_transmit[,, .x]),
    .progress = TRUE
  )


t <- bathy |>
  .convert_spr_to_vec()

.valid_idx(bathy, na = TRUE)
