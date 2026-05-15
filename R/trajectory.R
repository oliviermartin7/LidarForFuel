#' Renumber returns by height
#'
#' @param las LAS object
#' @param multi_pulse If TRUE the pulse index is defined by (gpstime, UserData).
#' If FALSE the pulse index is defined by gpstime.
#'
#' @return LAS
#' @export
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

#' Remove duplicated pulses or returns
#'
#' Removes in this order:
#' 1. duplicated pulses that have the different number of returns
#' 2. pulses with more returns than NumberOfReturns
#' 3. pulses with duplicated returns
#'
#' If using both lasrdup and lasrenumber, lasrmdup should used before lasrenumber.
#' @param las LAS object
#' @param multi_pulse If TRUE the pulse index is defined by (gpstime, UserData).
#' If FALSE the pulse index is defined by gpstime.
#'
#' @return LAS
#' @export
lasrmdup <- function(las, multi_pulse = FALSE) {
  by <- "gpstime"
  if (multi_pulse) {
    by <- c("gpstime", "UserData")
  }

  ReturnNumber <- NumberOfReturns <- valid <- NULL
  # remove duplicated pulses that have the different number of returns
  # we cannot detect
  dup <- las@data[, list(valid = length(unique(NumberOfReturns)) == 1 && .N <= NumberOfReturns[1]), by = by]
  las@data <- las@data[dup[valid == TRUE], on = by]

  # remove duplicated points
  dup <- las@data[, list(valid = !any(duplicated(ReturnNumber))), by = by]
  las@data <- las@data[dup[valid == TRUE], on = by][, valid := NULL][]
  las
}

#' Get trajectory from lidar data
#'
#' @param las LAS object
#' @param thin LAS data thinning in seconds, see lidR::track_sensor
#' @param interval LAS data thinning in seconds, see lidR::track_sensor
#' @param rmdup If TRUE, removes duplicated points in ReturnNumber
#' @param renum If TRUE, renumbers points in ReturnNumber
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
  renum = FALSE,
  multi_pulse = TRUE
) {
  gpstime <- NULL
  if (thin > 0) {
    # thinning pulses before rmdup
    # reduces considerably the computation time
    data <- las@data
    ftime <- plyr::round_any(data$gpstime - min(data$gpstime), thin)
    times <- data[, first(gpstime), by = ftime]$V1
    data <- data[gpstime %in% times]
    las@data <- data
  }

  if (!"UserData" %in% names(las@data)) {
    multi_pulse <- FALSE
  }

  if (rmdup) {
    # remove points with duplicated ReturnNumber
    # warning: can be very long for big las
    las <- lasrmdup(las, multi_pulse = multi_pulse)
  }

  if (renum) {
    las <- lasrenumber(las, multi_pulse = multi_pulse)
  }

  traj <- lidR::track_sensor(
    las,
    algorithm = lidR::Roussel2020(interval = interval),
    thin_pulse_with_time = 0,
    multi_pulse = multi_pulse
  )

  if (nrow(traj) == 0) {
    stop("Computed trajectory is empty: maybe not enough points to compute (expects 50 points by interval).")
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
