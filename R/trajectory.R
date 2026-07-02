lasrenumber <- function(las, multi_pulse = FALSE) {
  .N <- Z <- gpstime <- ReturnNumber <- NULL
  by <- "gpstime"
  if (multi_pulse) {
    by <- c("gpstime", "UserData")
  }
  data.table::setorder(las@data, gpstime, ReturnNumber, -Z)
  new_num <- las@data[, list(ReturnNumber = seq_len(.N), NumberOfReturns = .N), by = by]
  las@data[, names(new_num)] <- new_num
  las
}

lasrmdup <- function(las, multi_pulse = FALSE) {
  by <- "gpstime"
  if (multi_pulse) {
    by <- c("gpstime", "UserData")
  }
  ReturnNumber <- dup <- NULL
  dup <- las@data[, list(any(duplicated(ReturnNumber))), by = by]
  dup <- dup[dup$V1 == FALSE, ]
  las@data <- las@data[dup, on = by]
  las
}

#' Get trajectory from lidar data
#'
#' @param las LAS object
#' @param thin LAS data thinning in seconds, see lidR::track_sensor
#' @param interval LAS data thinning in seconds, see lidR::track_sensor
#' @param rmdup remove duplicated points in ReturnNumber
#' @param renum renumber points in ReturnNumber
#' @param multi_pulse If TRUE the pulse index is defined by (gpstime, UserData).
#' If FALSE the pulse index is defined by gpstime.
#'
#' @return sf object with gpstime, PointSourceID, SCORE and Point Z geometry
#'
#' @export
get_traj <- function(
  las,
  thin = 0.0001,
  interval = .2,
  rmdup = TRUE,
  renum = TRUE,
  multi_pulse = FALSE
) {
  X <- Y <- Z <- gpstime <- PointSourceID <- NULL

  if (thin > 0) {
    # thinning pulses before rmdup
    # reduces considerably the computation time
    data <- las@data
    ftime <- plyr::round_any(data$gpstime - min(data$gpstime), thin)
    times <- data[, first(gpstime), by = ftime]$V1
    data <- data[gpstime %in% times]
    las@data <- data
  }

  if (rmdup) {
    # remove points with duplicated ReturnNumber
    # warning: can be very long for big las
    las <- lasrmdup(las, multi_pulse = multi_pulse)
  }

  if (renum) {
    las <- lasrenumber(las, multi_pulse = multi_pulse)
  }

  traj <- try(
    {
      traj <- lidR::track_sensor(
        las,
        algorithm = lidR::Roussel2020(interval = interval),
        thin_pulse_with_time = 0,
        multi_pulse = multi_pulse
      )
    },
    silent = TRUE
  )

  try_default_traj <- FALSE
  if (inherits(traj, "try-error")) {
    warning("Failed to compute trajectory with lidR::track_sensor.")
    try_default_traj <- TRUE
  }
  if (nrow(traj) == 0) {
    warning("Trajectory computed with lidR::track_sensor is empty.")
    try_default_traj <- TRUE
  }

  if (try_default_traj) {
    warning("Setting default trajectory to 1400m above of the ground points.")
    traj <- lidR::filter_ground(las)@data[, list(gpstime, X, Y, Z, PointSourceID)]
    if (nrow(traj) == 0) {
      warning("No ground point found, cannot set default trajectory.")
      return(NULL)
    }
    traj <- traj[, list(
      X = mean(X), Y = mean(Y), Z = mean(Z) + 1400,
      PointSourceID = PointSourceID[1],
      SCORE = 0
    ), by = "gpstime"]
    traj <- traj |> sf::st_as_sf(coords = c("X", "Y", "Z"), crs = sf::st_crs(las))
  }

  return(traj)
}

#' Add trajectory to lidar data
#'
#' Interpolate trajectory to las points and add coordinates as extrabytes
#' @param las LAS object
#' @param traj sf object with column gpstime and Point Z geometries
#'
#' @return LAS object with additional attributes Easting, Northing, Elevation, Time
#' @export
add_traj_to_las <- function(las, traj) {
  X <- Y <- Z <- gpstime <- NULL

  if (inherits(traj, "sf")) {
    traj <- cbind(traj[, "gpstime"], sf::st_coordinates(traj)) |>
      sf::st_drop_geometry()
  }

  traj <- dplyr::rename(
    traj,
    Easting = X,
    Northing = Y,
    Elevation = Z,
    Time = gpstime
  )

  traj <- data.table::as.data.table(traj)

  # Find closest gpstime between traj and las
  nn2_gpstimes <- RANN::nn2(traj$Time, las$gpstime, k = 1)
  las@data <- cbind(las@data, traj[nn2_gpstimes$nn.idx, ])

  las <- lidR::add_lasattribute(las, name = "Easting", desc = "trajectory coordinate")
  las <- lidR::add_lasattribute(las, name = "Northing", desc = "trajectory coordinate")
  las <- lidR::add_lasattribute(las, name = "Elevation", desc = "trajectory coordinate")
  las <- lidR::add_lasattribute(las, name = "Time", desc = "trajectory timestamp")

  las
}
