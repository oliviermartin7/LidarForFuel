filter_seasons <- function(las, months = 1:12, gpstime_ref = "2011-09-14 01:46:40", plot_hist_days = FALSE) {
  if ((identical(sort(months), 1:12)) || lidR::is.empty(las)) {
    return(las)
  }

  datetime <- as.POSIXct(gpstime_ref, tz = "UTC") + las@data$gpstime

  # test la saison
  months_acquisition <- lubridate::month(datetime)

  pts_summer <- months_acquisition %in% months
  las <- lidR::filter_poi(las, pts_summer)
  proportions_of_winter_pont <- (1 - nrow(las@data) / length(months_acquisition)) * 100
  if (proportions_of_winter_pont > 0) {
    if (plot_hist_days) {
      graphics::hist(
        datetime,
        breaks = "day",
        main = "Histogram of acquisition date", xlab = "Date of acquisition"
      )
    }
    warning(
      paste0(
        "Careful ", round(proportions_of_winter_pont),
        " % of the returns were excluded because they were sampled outside of",
        " the chosen season (Month: ",
        paste0(lubridate::month(months, label = TRUE), collapse = " "), ")"
      )
    )
  }

  return(las)
}

filter_date_mode <- function(las, deviation_days = Inf, gpstime_ref = "2011-09-14 01:46:40", plot_hist_days = FALSE) {
  # filter the points that are not in the same day as the statistical data mode and +- deviation days
  datetime <- as.POSIXct(gpstime_ref) + las@data$gpstime
  if (is.infinite(deviation_days)) {
    return(las)
  }

  hist_test <- graphics::hist(
    datetime,
    breaks = "day",
    plot = plot_hist_days,
    main = "Histogram of acquisition date",
    xlab = "Date of acquisition"
  )
  count_days <- hist_test$counts
  if (length(count_days) > deviation_days) {
    seq_dates <- seq.Date(
      from = as.Date(min(datetime)),
      to = as.Date(max(datetime)),
      by = "days"
    )
    id_max_count <- which(count_days == max(count_days))
    id_vec_dates <- (id_max_count - deviation_days):(id_max_count + deviation_days)
    id_vec_dates <- id_vec_dates[id_vec_dates > 0]

    good_dates <- lubridate::floor_date(as.POSIXct(seq_dates[id_vec_dates], tz = "CET"), unit = "day")
    date_days <- lubridate::floor_date(datetime, unit = "day")
    las <- lidR::filter_poi(las, date_days %in% good_dates)
    percentage_point_remove <- (1 - nrow(las@data) / length(datetime)) * 100
    warning(
      paste0(
        "Careful ",
        round(percentage_point_remove),
        " % of the returns were removed because they had a deviation of days",
        " around the most abundant date greater than your threshold (",
        deviation_days, " days)."
      )
    )
  }

  return(las)
}



#' Point cloud pre-treatment for using fCBDprofile_fuelmetrics in pixels
#'
#' @description Function for preprocessing las (laz) files for use in fCBDprofile_fuelmetrics. This can be used in the catalog_apply lidR function. The pretreatment consists of normalizing the point cloud and adding various attributes: Plane position for each point (easting, northing, elevation), LMA (leaf mass area) and wood density (WD) by intersecting the point cloud with an LMA and WD map or by providing LMA and WD values.
#' @param chunk LAS object, LAScluster chunk or character. If character, the path to a las (laz) file is expected.
#' Can be apply to a catalog see lidR::catalog_apply # nolint: line_length_linter.
#' @param classify logical (default is FALSE). Make a ground classification. Only if the original point cloud is not classified
#' @param LMA character or numeric. Default = 140. If available path to a LMA map (.tif) of the area if available or a single LMA value in g.m² (e.g 140. cf: Martin-Ducup et al. 2024)
#' @param WD character or numeric. Default = 591. If available, path to a WD map (.tif) of the area if available or a single WD value in kg.m3 (e.g 591 cf: Martin-Ducup et al. 2024).
#' @param LMA_bush  character or numeric.Default = 140. similar to LMA but for the understorey strata (see H_strata_bush)
#' @param WD_bush character or numeric. Default = 591. similar to WD but for the understorey strata (see H_strata_bush)
#' @param H_strata_bush numeric. Default = 2. Height of the strata to consider for separating LMA and WD between canopy and bush.
#' @param Height_filter numeric. Default = 80. Height limit to remove noise point
#' @param deviation_days numeric. Maximum number of days tolerated between the acquisition in a given point cloud (a tile or plot). Deactivated by default
#' @param plot_hist_days logical. Should the histogram of dates of acquisition be displayed. Default =FALSE
#' @param start_date date. The reference datetime to retrieve datetime from relative gpstime of the laz.
#' It is expected to be in timezone UTC. Default is "2011-09-14 01:46:40" which is the standard GPS Time (1980-01-06 00:00:00)
#' plus 1e9 seconds, as defined in LAS 1.4 specifications.
#' @param season_filter numeric. A vector of integer for months to keep (e.g: 5:10 keep retunrs between may and october)
#' @param traj sf object. Trajectory covering the LAS chunk. The sf object is expected to have a column gpstime and Point Z geometries.
#' If NULL, the function will try to compute it.
#' @return LAS object. A normalized point cloud (.laz) with several new attributes needed to run fCBDprofile_fuelmetrics, see details.
#' @details
#' The attributes added to the laz are:
#' - LMA : LMA value of each point
#' - Zref :original Z
#' - Easting, Northing, Elevation, Time that are the X,Y,Z coordinates of the sensor
#'   and the its GPStime for each point (obtained from lidR::track_sensor()).
#' @examples
#' \donttest{
#' path2laz <- system.file("extdata", "M30_FontBlanche.laz", package = "lidarforfuel")
#' # LMA value selected = 120.6 that is the LMA for Pinus halepensis, the dominant species of the plot
#' M30_FontBlanche_pretreated <- fPCpretreatment(path2laz, LMA = 120.6)
#' # displaying the new attributes in the las
#' names(M30_FontBlanche_pretreated)
#' }
#' @import data.table
#' @export
fPCpretreatment <- function(
  chunk,
  classify = FALSE,
  LMA = 140,
  WD = 591,
  WD_bush = 591,
  LMA_bush = 140,
  H_strata_bush = 2,
  Height_filter = 60,
  start_date = "2011-09-14 01:46:40",
  season_filter = 1:12,
  deviation_days = Inf,
  plot_hist_days = FALSE,
  traj = NULL
) {
  X <- Z <- Classification <- NULL

  # read chunk
  if (inherits(chunk, "LAS")) {
    las <- chunk
  } else {
    las <- lidR::readLAS(chunk)
  }
  # TODO: filter virtual points, cf virtual classes in lidar-hd specs
  las <- filter_seasons(las, season_filter, plot_hist_days = plot_hist_days)
  las <- filter_date_mode(las, deviation_days, plot_hist_days = plot_hist_days)
  # TODO: does it really happens in lidar-hd?
  las <- lidR::filter_poi(las, !is.na(X))

  if (lidR::is.empty(las)) {
    return(NULL)
  }

  if (is.null(traj)) {
    warning(paste0(
      "Computing trajectory from LAS file...\n",
      "Trajectory would better be computed outside pretreatment, ",
      "with a buffer (e.g. 500m) to avoid border effects."
    ))
    traj <- get_traj(las, thin = 0.0001, interval = .2, rmdup = TRUE, renum = TRUE)
  }
  las <- add_traj_to_las(las, traj)

  if (classify == TRUE) {
    lidR::classify_ground(las, algorithm = lidR::csf())
  }

  # if (norm_ground == TRUE){
  #   # Filter ground points
  #   las_ground=lidR::filter_ground(las)
  #   dtm = lidR::rasterize_terrain(las_ground, algorithm = tin(),res=3)
  #   dtm_las=LAS(data.table(as.data.frame(dtm,xy=T)))
  #   # calculate normals on dtm and get vector components
  #   dtm_las=geom_features(las=dtm_las,search_radius = 6,features_list = c("Nx","Ny","Nz"))
  #
  #   # Find closest neighboor between normal DTM and las and attribute normal DTM to each point of the las
  #   nn2_las_DTM=nn2(dtm_las@data[,1:3],las@data[,1:3],k=1)
  #   las@data=cbind(las@data,dtm_las@data[nn2_las_DTM$nn.idx,4:6])
  # }

  # LMA
  if (is.numeric(LMA)) {
    las@data[["LMA"]] <- LMA
  } else {
    ## Load LMA map
    LMA_map <- terra::rast(LMA)
    ### Add LMA to point cloud
    las <- lidR::merge_spatial(las, LMA_map$LMA, attribute = "LMA")
  }
  if (is.numeric(WD)) {
    las@data[["WD"]] <- WD
  } else {
    ## Load LMA map
    WD_map <- terra::rast(WD)
    ### Add WD to point cloud
    las <- lidR::merge_spatial(las, WD_map$WD, attribute = "WD")
  }

  # Normalyze height
  las <- lidR::normalize_height(las = las, algorithm = lidR::tin())
  # Remove points too low (<-3) or too high (>35m). Keep vegetation, ground, unclassified and water point only
  las <- lidR::filter_poi(las, (Classification <= 5 | Classification == 9) & Z < Height_filter)
  las <- lidR::classify_noise(las, lidR::sor(5, 10))

  las@data[Z <= H_strata_bush]$LMA <- LMA_bush
  las@data[Z <= H_strata_bush]$WD <- WD_bush
  # add names to laz
  las <- lidR::add_lasattribute(las, name = "LMA", desc = "leaf mass area")
  las <- lidR::add_lasattribute(las, name = "WD", desc = "Wood density")
  las <- lidR::add_lasattribute(las, name = "Zref", desc = "original Z")
  # if (norm_ground == T){
  #   las=lidR::add_lasattribute(las,name="Nx",desc="normal")
  #   las=lidR::add_lasattribute(las,name="Ny",desc="normal")
  #   las=lidR::add_lasattribute(las,name="Nz",desc="normal")
  # }
  # las=remove_lasattribute(las, name="Reflectance")
  # las=remove_lasattribute(las, name="Deviation")
  # las@data=las@data[,c("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")]
  # las@header@VLR = list("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")

  return(las)
}
