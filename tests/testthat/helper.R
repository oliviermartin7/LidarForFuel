test_log_layout <- logger::layout_glue_generator(
  "{format(time, format = '%Y-%m-%d %H:%M:%S')} {level} {topenv}.{fn}: {msg}"
)
logger::log_layout(test_log_layout)

small_laz_file <- function() {
  system.file(
    "extdata", "M30_FontBlanche.laz",
    package = "lidarforfuel"
  )
}
