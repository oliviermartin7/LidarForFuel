.class_metrics <- function(
  gpstime, Classification, classes = c(1:6, 9, 17, 18, 64, 66, 67),
  season_filter = 1:12, deviation_days = Inf, gpstime_ref = "2011-09-14 01:46:40"
) {
  valid_points <- filter_gpstime(gpstime, months = season_filter, deviation_days = deviation_days, gpstime_ref = gpstime_ref)
  Classification <- Classification[valid_points]
  classes <- sort(classes)
  uclasses <- unique(Classification)
  if (!all(uclasses %in% classes)) {
    warning("Unexpected classes are excluded: ", uclasses[!(uclasses %in% classes)])
    Classification <- Classification[Classification %in% classes]
  }
  Classification <- Classification |> factor(levels = classes)
  class_count <- Classification |>
    table()
  names(class_count) <- paste("class", names(class_count), sep = "_")
  class_count <- c(class_count, total = sum(class_count))
  return(as.list(class_count))
}

#' Class counts
#'
#' @param classes Vector of classes to count
#' Default class specs are from https://data.geopf.fr/annexes/ressources/documentation/DC_LiDAR_HD_1-0.pdf.
#'
#' @return Formula for lidR::xxx_metrics
#' @examples
#' \dontrun{
#' las_file <- system.file("extdata", "M30_FontBlanche.laz", package = "lidarforfuel")
#' las <- lidR::readLAS(las_file)
#' # as cloud metrics
#' class_count <- lidR::cloud_metrics(las, class_metrics())
#' # as pixel metrics, i.e. rasterized
#' class_count <- lidR::pixel_metrics(las, class_metrics(), res = 10)
#' }
#'
#' @export
class_metrics <- function(
  classes = c(1:6, 9, 17, 18, 64, 66, 67),
  season_filter = 1:12, deviation_days = Inf, gpstime_ref = "2011-09-14 01:46:40"
) {
  fun <- substitute(
    ~ .class_metrics(gpstime, Classification, classes, season_filter = season_filter, deviation_days = deviation_days, gpstime_ref = gpstime_ref), list(
      classes = classes,
      season_filter = season_filter, deviation_days = deviation_days, gpstime_ref = gpstime_ref
    )
  ) |> stats::as.formula()
  return(fun)
}
