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

raster_file <- function() {
  system.file(
    "extdata", "raster_template.tif",
    package = "lidarforfuel"
  )
}