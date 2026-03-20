#' Compute PAD metrics
#'
#' @description This function computes PAD metrics from a lidar point cloud preprocessed output.
#' The expected LAS attributes are: `gpstime`, `X`, `Y`, `Z`, `Zref`, `ReturnNumber`, `Easting`,
#' `Northing`, `Elevation`.
#' @param z0 numeric. Default = 0. Minimum height of the first layer in meters.
#' @param dz numeric. Default = 1. Height of a layer in meters.
#' @param nlayers numeric. Default = 60. Number of layers.
#' @param G numeric. Default = 0.5. Leaf projection ratio.
#' @param omega numeric. clumping factor.
#' Default is 1.
#' Value 1 means "no clumping" and therefore assumes a homogeneous distribution of vegetation element in the strata.
#' Value < 1 means clumping.
#' @param scanning_angle logical. Default = TRUE. Use the scanning angle computed from the trajectories to estimate cos(theta). If false: cos(theta) = 1
#' @param cover_type character. Default = "all". Should all returns or only first returns be considered for cover estimation.
#' Accepted values are be "all" or "first".
#' @param height_cover numeric. Default = 2. The height from which the canopy cover should be estimated.
#' @param use_cover logical. Default = FALSE. Use cover for PAD estimates.
#' @param limit_N_points numeric. Default = 400. minimum number of point in the pixel/plot for computing profiles & metrics.
#' @param limit_flight_agl numeric. Default = 800. Limit flight height above ground in m. If the distance between the flight height and the ground
#' and (Elevation - Zref) is lower than `limit_flight_agl`, NULL is returned.
#' This limit serves as a safeguard to eliminate cases where the trajectory reconstruction would be outlier.
#' @param keep_N logical. Default = FALSE. Keep the number of entering rays (N) and the number of hits (Ni) in each layer.
#'
#' @return
#' \itemize{
#'   \item `pad_metrics`: a formula for lidR::xxx_metrics functions
#'   \item `.pad_metrics`: a list of PAD metrics, see below.
#' }
#'
#' The list of PAD metrics includes:
#' \itemize{
#'   \item date as the mean gpstime
#'   \item Cover layers at `height_cover`, 4m and 6m
#'   \item PAD layers from `z0` to `z0 + nlayers * dz`,
#' }
#'
#' If keep_N = TRUE, the list also contains Ni and N layers at same heights than PAD layers.
#'
#' @examples
#' \dontrun{
#' las_file <- system.file("extdata", "M30_FontBlanche.laz", package = "lidarforfuel")
#' las <- lidR::readLAS(las_file)
#' # In real life, traj should be computed separately with buffer to avoid border effects
#' traj <- get_traj(las)
#' nlas <- fPCpretreatment(las, traj = traj)
#' pad <- lidR::cloud_metrics(nlas, pad_metrics(z0 = 0, dz = 0.5, nlayers = 120))
#' # or
#' pad_rast <- lidR::pixel_metrics(nlas, pad_metrics(), res = 10)
#' }
#' @name pad_metrics
#' @export
pad_metrics <- function(
  z0 = 0, dz = 1, nlayers = 60,
  G = 0.5, omega = 0.77,
  scanning_angle = TRUE,
  cover_type = "all", height_cover = 2, use_cover = TRUE,
  limit_N_points = 400, limit_flight_agl = 800, keep_N = FALSE
) {
  fun <- substitute(
    ~ .pad_metrics(
      gpstime, X, Y, Z, Zref, ReturnNumber,
      Easting, Northing, Elevation,
      z0 = z0, dz = dz, nlayers = nlayers, G = G, omega = omega,
      scanning_angle = scanning_angle,
      cover_type = cover_type, height_cover = height_cover, use_cover = use_cover,
      limit_N_points = limit_N_points, limit_flight_agl = limit_flight_agl,
      keep_N = keep_N
    ), list(
      z0 = z0, dz = dz, nlayers = nlayers, G = G, omega = omega,
      scanning_angle = scanning_angle,
      cover_type = cover_type, height_cover = height_cover, use_cover = use_cover,
      limit_N_points = limit_N_points, limit_flight_agl = limit_flight_agl,
      keep_N = keep_N
    )
  ) |> stats::as.formula()

  return(fun)
}

#' @rdname pad_metrics
#'
#' @export
.pad_metrics <- function(
  gpstime, X, Y, Z, Zref, ReturnNumber,
  Easting, Northing, Elevation,
  z0 = 0, dz = 1, nlayers = 60,
  G = 0.5, omega = 0.77,
  scanning_angle = TRUE,
  cover_type = "all", height_cover = 2, use_cover = TRUE,
  limit_N_points = 400, limit_flight_agl = 800,
  keep_N = FALSE
) {
  if (length(Z) < limit_N_points) {
    warning("NULL return: the number of points < limit_N_points. Check the point cloud.")
    return(NULL)
  }

  if (scanning_angle) {
    ## calculates component of  vector U (plane -> point). To take into account scanning angle in PAD estimation ----
    # no abs as we don't want to have flight_agl < 0
    flight_agl <- Elevation - Zref
    norm_U <- sqrt((X - Easting)^2 + (Y - Northing)^2 + flight_agl^2)
    ### Exception if the mean of norm_U < limit_flight_agl For LiDAr HD 1000m mean that plane flew lower than 1000m over the plot => unlikely for LiDAR HD => probably error in trajectory reconstruction
    if (mean(flight_agl, na.rm = TRUE) < limit_flight_agl) {
      warning("NULL return: limit_flight_agl below the threshold. Check your trajectory and avoid using scanning_angle mode if the trajectory is uncertain")
      return(NULL)
    }
    Nz_U <- flight_agl / norm_U
  } else {
    Nz_U <- 1
  }

  ## Create a sequence to make strata  ----
  if (is.null(nlayers)) {
    z_max_pad <- plyr::round_any(max(Z), dz, ceiling)
  } else {
    z_max_pad <- z0 + dz * nlayers
  }

  breaks <- c(-Inf, seq(z0, z_max_pad, dz))
  ## get number of return in strata
  Ni <- cut(Z, breaks = breaks) |>
    table() |>
    c()
  N <- cumsum(Ni)

  # min z of each layer
  min_layer <- breaks[-length(breaks)]

  # remove first layer useless for the rest
  Ni <- Ni[-1]
  N <- N[-1]
  min_layer <- min_layer[-1]

  NRD <- Ni / N
  # case of Ni=0 and N=0
  NRD[is.nan(NRD)] <- 0
  ## NRD estimation  ----
  # Ni +1 et N +2 pour les cas où Ni=0 ou NRD=1 => NRDc de l'equations 23 et 24 de Pimont et al 2018
  i_NRDc <- NRD %in% c(0, 1)
  NRD[i_NRDc] <- (Ni[i_NRDc] + 1) / (N[i_NRDc] + 2)

  ## Gap fraction estimation ----
  Gf <- 1 - NRD


  ### cos theta take into account scanning angle
  cos_theta <- mean(abs(Nz_U))

  ## Plant area density calculation (actually FAD --> fuel area density: leaves + twigs) ----
  if (cover_type == "first") {
    # first returns covers
    first_returns <- ReturnNumber == 1
    N_f <- sum(first_returns)
    cover_h <- sum(first_returns[Z > height_cover]) / N_f
    cover_4 <- sum(first_returns[Z > 4]) / N_f
    cover_6 <- sum(first_returns[Z > 6]) / N_f
  } else if (cover_type == "all") {
    # compute "NRD" cover, i.e. all returns above height_cover
    cover_h <- sum(Z > height_cover) / length(Z)
    cover_4 <- sum(Z > 4) / length(Z)
    cover_6 <- sum(Z > 6) / length(Z)
  } else {
    stop("cover_type must be 'all' or 'first'")
  }

  if (use_cover) {
    PAD <- -(log(Gf) * cos_theta / (G * omega) / dz)
  } else {
    if (height_cover >= max(Z)) {
      warning(paste0("height_cover > maximum vegetation height"))
    }
    if (cover_h == 0) {
      PAD <- -(log(Gf) * cos_theta / (G * omega) / dz)
      warning(paste0("Cover method was not use as Cover = 0"))
    } else {
      PAD <- (-log(1 - Ni / (N * cover_h)) / (G * omega * (dz / cos_theta))) * cover_h
    }
  }

  # set PAD to 0 for upper strata with no points
  min_empty <- plyr::round_any(max(Z), dz, ceiling)
  PAD[min_layer >= min_empty] <- 0

  intervals <- names(PAD) |>
    gsub("(\\(|\\[|\\]|\\))", "", x=_) |>
    gsub(",", "_", x=_)

  names(PAD) <- paste0("PAD_", intervals)
  output <- as.list(PAD)

  if (keep_N) {
    names(Ni) <- paste0("Ni_", intervals)
    names(N) <- paste0("N_", intervals)

    output <- c(output, as.list(Ni))
    output <- c(output, as.list(N))
  }

  date <- mean(gpstime)

  output <- c(
    list(date = date),
    list(Cover = cover_h, Cover_4 = cover_4, Cover_6 = cover_6),
    output
  )
  return(output)
}
