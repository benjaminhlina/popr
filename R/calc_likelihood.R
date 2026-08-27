#' Calculate Likelihood Functions
#'
#' These are functions that will calculated the likelihood
#' of movement based on additonal parameters such as depth
#'
#' @param pdt a `data.frame` containing psat depth data for a single transmitter
#' @param bathy a the file path to a bathymetric grid  "/project/bathy_grid/bathy.mat.gsl.ssgb.lon72.49.lat41.52.RData"
#' @param ncores a `numeric` that dictates the number of cores used.
#'  description
#' @weighted a `logical` value that dictates whether the model weights predictions
#' @focal_dim a `numeric` do not know what this does
#' @daily_sd_depth a numeric value
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#'
calc_likdepth_transmit <- function(
  pdt,
  bathy = NULL,
  ncores = 4,
  weighted = FALSE, # nb cores in the parallel cluster 4 default value #
  focal_dim = 9,
  daily_sd_depth = 0.57
) {
  # ctd.dir="C:/Users/pgatti/Documents/halibut/R/Data_PGalbraith/toBeLoaded/";ncores=4;weighted=FALSE; focalDim = 3;daily.sd.depth=.57; pdt=sm$sm;depth.list=depth.mix

  options(warn = 0)
  # t0 <- round(Sys.time())
  check_start(x = "Depth")

  #--------------------------
  # data recorded by the tag
  # pre process the summary table
  pdt$Date <- lubridate::parse_date_time(ac(pdt$day), "%Y-%m-%d")
  # in case of lubridate failure # pdt$Date=as.POSIXct(ac(pdt$day), format="%Y-%m-%d")
  dateVec <- pdt$Date
  T <- length(dateVec)

  print(paste0(
    "Generating profile likelihood for ",
    ac(dateVec)[1],
    " through ",
    ac(dateVec)[length(dateVec)]
  ))

  #--------------------------
  # load env data CTD
  # load(paste0(ctd.dir,"bathy.mat.13_18.RData"))
  load(bathy)
  zbi <- db
  rm(db)
  indNA <- which(is.na(zbi))
  inds <- 1:length(zbi)
  inds <- inds[!inds %in% indNA]

  #---------------------
  # compute arrays of standard deviation of bathymetry
  if (weighted) {
    print('!! only pre-computed matrix is for focaldim=3, 5 or 9')
    load(paste0(ctd.dir, 'sdcell.n', focalDim, '.weighted.RData'))
    sdi <- sdcell
    rm(sdcell)
  } else {
    sd <- raster::focal(
      raster::raster(zbi),
      w = matrix(1, nrow = focalDim, ncol = focalDim),
      fun = function(x) sd(x, na.rm = TRUE)
    )
    sdi <- raster::as.array(sd)
    # fill sd at 0
    ssd <- as.vector(sdi)
    fill.sd0 <- quantile(ssd[ssd != 0], na.rm = TRUE, probs = .01)
    if (is.na(fill.sd0)) {
      fill.sd0 <- 1e-3
    } # cas de force majeure
    sdi[sdi == 0] <- fill.sd0
    sdi[is.na(sdi)] <- fill.sd0
    sdi <- sdi[,, 1]
  }

  #---------------------------------------------------------------------
  # start of heavy daily loop
  # here is the place to implement parallelisation
  #--------------------------
  #  t1.=Sys.time()
  print(paste("Starting iterations through deployment period ", "..."))

  # declare cluster parrallel
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl, cores = ncores)
  ans <- foreach::foreach(i = 1:T) %dopar%
    {
      # i = 2
      # i=187
      # i=341
      time <- as.Date(ac(dateVec[i])) #as.Date(udates[i])
      pdt.i <- pdt[ac(pdt$day) == ac(time), ]
      # range fish depth considered to be the bottom
      # error tag +-1m +-1% depth
      if (is.na(pdt.i$Depth_max) | is.infinite(pdt.i$Depth_max)) {
        lik <- zbi
        lik[inds] <- 1
      } else {
        zz <- sort(an(pdt.i[, c('Depth_min', 'Depth_max')]))
        df <- cbind.data.frame(
          low = zz[1] - (1 + .01 * abs(zz[1])),
          high = zz[2] + (1 + .01 * abs(zz[2]))
        )

        # indexes

        # lik gauss
        lik <- likint3.sub(
          w = cbind(
            as.vector(zbi),
            as.vector(sdi) + daily.sd.depth,
            an(df[1]),
            an(df[2])
          ),
          widx = inds
        )
        lik <- matrix(data = lik, ncol = ncol(zbi), nrow = nrow(zbi))

        # lik
        lik[is.na(lik)] <- 0
        lik[indNA] <- NA
        lik <- lik / max(lik, na.rm = TRUE)
      }
      lik
    }
  # end of heavy daily loop
  parallel::stopCluster(cl)
  #---------------------------------------------------------------------
  # array of likelihood (empty)
  L. <- array(0, dim = c(dim(zbi)[1], dim(zbi)[2], length(dateVec)))
  # switch from list to array
  for (i in 1:T) {
    L.[,, i] <- ans[[i]]
  }

  L. <- aperm(L., c(2, 1, 3))
  t1 <- Sys.time()
  print(paste(
    "CTD profile calculations took ",
    round(as.numeric(difftime(t1, t0, units = "mins")), 2),
    "minutes..."
  ))
  #options(warn = 2)
  return(L.)
}
