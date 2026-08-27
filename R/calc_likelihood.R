#' Calculate Likelihood Functions
#'
#' These are functions that will calculated the likelihood
#' of movement based on additonal parameters such as depth
#'
#' @param pdt a `data.frame` containing psat depth data for a single transmitter
#' @param bathy a the file path to a bathymetric grid  "/project/bathy_grid/bathy.mat.gsl.ssgb.lon72.49.lat41.52.RData"
#' @param ncores a `numeric` that dictates the number of cores used.
#'  description
#' @param weighted a `logical` value that dictates whether the model weights predictions
#' @param focal_dim a `numeric` do not know what this does
#' @param daily_sd_depth a numeric value
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
  daily_sd_depth = 0.57
) {
  # ctd.dir="C:/Users/pgatti/Documents/halibut/R/Data_PGalbraith/toBeLoaded/";ncores=4;weighted=FALSE; focalDim = 3;daily.sd.depth=.57; pdt=sm$sm;depth.list=depth.mix

  error_df(pdt)
  error_spatrast(bathy)
  error_numeric(ncores)

  options(warn = 0)
  # t0 <- round(Sys.time())
  msg_start(x = "Depth")

  msg_dates(df = pdt)

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
        likint3_matrix()
        lik <- likint3_matrix(
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
