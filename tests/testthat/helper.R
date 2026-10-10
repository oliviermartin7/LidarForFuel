test_log_layout <- logger::layout_glue_generator(
  "{format(time, format = '%Y-%m-%d %H:%M:%S')} {level} {topenv}.{fn}: {msg}"
)
logger::log_layout(test_log_layout)

small_laz_file <- function() {
  system.file(
    "extdata", "0_42.laz",
    package = "lidarforfuel"
  )
}

raster_template <- function() {
  # the raster is not aligned on (0,0) on purpose,
  # to check that alignment occures as expected in tests
  xoff <- yoff <- pi + 100
  N <- res <- 10
  data <- matrix(0, nrow = N, ncol = N)
  extent <- c(
    xmin = xoff,
    xmax = N * res + xoff,
    ymin = yoff,
    ymax = N * res + yoff
  )
  x <- terra::rast(data, extent = extent, crs = "epsg:32631")
  return(x)
}
