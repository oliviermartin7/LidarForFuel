#' convert gps time to POSIXct datetime
#'
#' @param gpstime numeric. vector of gps time
#' @param gpstime_ref character. reference datetime
#' @return POSIXct datetime
#' @export
gpsptime_to_datetime <- function(gpstime, gpstime_ref = "2011-09-14 01:46:40") {
  datetime <- as.POSIXct(gpstime_ref, tz = "UTC") + gpstime
  return(datetime)
}

#' convert datetime to gps time
#'
#' @param datetime POSIXct vector of datetime
#' @param gpstime_ref character. reference datetime
#' @return numeric. vector of gps time
#' @export
datetime_to_gpstime <- function(datetime, gpstime_ref = "2011-09-14 01:46:40") {
  gpstime <- as.numeric(datetime - as.POSIXct(gpstime_ref, tz = "UTC"))
  return(gpstime)
}

#' test if points are in the months
#'
#' @param datetime POSIXct vector of datetime
#' @param months numeric vector of months defining the expected season
#' @return logical vector with TRUE for points within the season
#' @examples \dontrun{
#' library(lidR)
#' filter_poi(las, is_in_season(gpstime_to_datetime(gpstime), months = 5:9))
#' # same as
#' filter_poi(las, filter_gpstime(months = 5:9))
#' }
#' @export
is_in_season <- function(datetime, months = 1:12) {
  if (identical(sort(months), 1:12)) {
    return(rep(TRUE, length(datetime)))
  }
  # test la saison
  date_months <- lubridate::month(datetime)
  valid_points <- date_months %in% months
  return(valid_points)
}

.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' test if points are in the date mode +- deviation_days
#'
#' @param datetime POSIXct vector of datetime
#' @param deviation_days numeric. the number of days defining the valid range around the date mode
#' @return logical vector with TRUE for points within the date mode +- deviation_days
#' @examples \dontrun{
#' library(lidR)
#' filter_poi(las, is_near_date_mode(gpstime_to_datetime(gpstime), deviation_days = 14))
#' # same as
#' filter_poi(las, filter_gpstime(deviation_days = 14))
#' }
#' @export
is_near_date_mode <- function(datetime, deviation_days = Inf) {
  if (is.infinite(deviation_days)) {
    return(rep(TRUE, length(datetime)))
  }
  date <- as.Date(datetime)
  d <- .mode(date)
  valid_points <- abs(date - d) <= deviation_days
  return(valid_points)
}


#' test if points are in the season and around a date mode
#'
#' This function returns TRUE when points are in the season and around the date mode,
#' the date mode being computed on points within the season.
#' @param gpstime numeric. vector of gps time in seconds
#' @param months numeric vector of months defining the expected season
#' @param deviation_days numeric. the number of days defining the valid range around the date mode
#' @param gpstime_ref character. Default = "2011-09-14 01:46:40". The datetime corresponding to gpstime=0, in order to retrieve the real datetime of points.
#' It is expected to be in timezone UTC. Default is "2011-09-14 01:46:40" which is the standard GPS Time (1980-01-06 00:00:00)
#' plus 1e9 seconds, as defined in LAS 1.4 specifications.
#'
#' @return logical vector with TRUE for points within the season and date mode +- deviation_days.
#' @examples \dontrun{
#' library(lidR)
#' filter_poi(las, filter_gpstime(gpstime, months = 5:9, deviation_days = 14))
#' # same as
#' filter_poi(las, filter_gpstime(months = 5:9)) |>
#'   filter_poi(filter_gpstime(deviation_days = 14))
#' # but different from
#' filter_poi(las, filter_gpstime(deviation_days = 14)) |>
#'   filter_poi(filter_gpstime(months = 5:9))
#' }
#' @export
filter_gpstime <- function(gpstime, months = 1:12, deviation_days = Inf, gpstime_ref = "2011-09-14 01:46:40") {
  datetime <- gpsptime_to_datetime(gpstime)
  # filter seasons
  season_points <- is_in_season(datetime, months = months)
  # filter deviation days on season points
  valid_points <- rep(FALSE, length(datetime))
  valid_points[season_points] <- is_near_date_mode(datetime[season_points], deviation_days)
  return(valid_points)
}


#' Filter by pixel
#'
#' @param las A \code{\link[lidR]{LAS}} object.
#' @param res numeric. The resolution of the output. Can optionally be raster of type stars or SpatRaster.
#' In that case the raster is used as the template.
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) meaning that the grid aligns on (0,0). Not considered if res is a raster.
#' @param filter Logical predicate. Multiple conditions are combined with '&' or '|'.
#'
#' @return A \code{\link[lidR]{LAS}} object with the filtered data.
#'
#' @examples \dontrun{
#' library(lidR)
#' library(lidarforfuel)
#' grid <- expand.grid(x = 1:100, y = 1:100)
#' las <- lidR::LAS(
#'   data = data.table(
#'     X = as.numeric(grid$x),
#'     Y = as.numeric(grid$y),
#'     Z = 0,
#'     # distribute points on a year
#'     gpstime = as.numeric(seq_len(nrow(grid))) * 3600 * 24 * 365 / nrow(grid)
#'   )
#' )
#' las1 <- pixel_filter(las = las, res = 10, filter = filter_gpstime(gpstime, months = 5:8))
#' range(gpsptime_to_datetime(las$gpstime))
#' range(gpsptime_to_datetime(las1$gpstime))
#' }
#' @export
pixel_filter <- function(las, res, start = c(0, 0), filter) {
  if (!inherits(las, "LAS")) {
    stop("Argument is not a LAS object", call. = FALSE)
  }


  # conditions <- lazyeval::f_capture(cond)
  # formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  # if (!formula) func <- lazyeval::f_capture(cond)
  # func   <- lazyeval::f_interp(func)
  # call   <- lazyeval::as_call(func)
  if (is_raster(res)) {
    bbox <- raster_bbox(res)
    start <- c(bbox["xmin"], bbox["ymin"])
    res <- raster_res(res)
  }

  # get cell number same as pixel_metrics
  bbox_las <- sf::st_bbox(las)
  xmin <- adjust_start(start[1], bbox_las["xmin"], res)
  ymin <- adjust_start(start[2], bbox_las["ymin"], res)
  # pixel_metrics is starting from ymax to define row
  col <- .bincode(las$X, seq(xmin, bbox_las["xmax"] + res, res), right = FALSE, include.lowest = TRUE)
  row <- .bincode(las$Y, seq(ymin, bbox_las["ymax"] + res, res), right = TRUE, include.lowest = TRUE)
  col <- col - min(col)
  row <- row - min(row)

  # to check equality with pixel_metrics
  # cell <- col + (max(col) + 1) * row + 1
  # template <- lidR:::raster_layout(las, res, start)
  # cell1 <- lidR:::get_group.raster_template(template, las)
  # col1 <- (cell1 - 1) %% template$ncol
  # row1 <- template$nrow - (cell1 - col1 - 1) / template$ncol - 1
  # cell1 <- col1 + (max(col) + 1) * row1 + 1
  # all(cell == cell1)

  data <- las@data
  data[["cell"]] <- col + (max(col) + 1) * row + 1
  filter <- deparse(substitute(filter))
  text <- paste0("data[,.(valid=", filter, "), by = 'cell']")
  keep <- eval(parse(text = text))
  # free memory
  rm(data)
  gc()
  # keep <- las@data[,.(valid={cond}), by="cell"]

  las1 <- lidR::filter_poi(las, keep$valid)
  return(las1)
}
