#' Mask k
#'
#' This fucntion takes coordates and calcualtes eludian distance between
#' locations
#'
#' @param k is `matrix` continaing coordinates
#'
#' @return k as a `matrix` that are elucdian distances between locations with maks
#'
#' @export
mask_k <- function(k) {
  #K=matrix(data=NA)
  coord_kx <- matrix(
    data = rep(1:ncol(k), each = nrow(k)),
    ncol = ncol(k),
    nrow = nrow(k)
  ) -
    (trunc(ncol(k) / 2) + 1)

  coord_ky <- matrix(
    data = rep(1:ncol(k), nrow(k)),
    ncol = ncol(k),
    nrow = nrow(k)
  ) -
    (trunc(ncol(k) / 2) + 1)

  coord <- cbind(as.vector(coord_kx), as.vector(coord_ky))
  coord <- apply(coord, 1, function(x) {
    ed <- sqrt(x[1]**2 + x[2]**2)
    return(ed)
  }) # distance euclidienne avec le point de coordonnes 0 0

  coord <- matrix(data = coord, ncol = ncol(k), nrow = nrow(k))
  coord[round(coord) > (trunc(ncol(k) / 2))] <- NA
  mask <- which(is.na(coord))
  k[mask] <- 0
  k <- k / sum(k, na.rm = T)
  return(k)
}
