


#' Point cloud pre-treatment for using fCBDprofile_fuelmetrics in pixels
#'
#' @description Function for preprocessing las (laz) files for use in fCBDprofile_fuelmetrics. This can be used in the catalog_apply lidR function. The pretreatment consists of normalizing the point cloud and adding various attributes: Plane position for each point (easting, northing, elevation), LMA (leaf mass area) and wood density (WD) by intersecting the point cloud with an LMA and WD map or by providing LMA and WD values.
#' @param chunk LAS object, LAScluster chunk or character. If character, the path to a las (laz) file is expected.
#' Can be apply to a catalog see lidR::catalog_apply # nolint: line_length_linter.
#' @param classify logical (default is FALSE). Make a ground classification. Only if the original point cloud is not classified
# @param season_filter numeric. A vector of integer for months to keep (e.g: filter_season = 5:10 keep retunrs between may and october)
# @param deviation_days numeric. Maximum number of days tolerated between the acquisition in a given point cloud (a tile or plot). Deactivated by default
#' @param exclude_classes numeric. Default = NULL. A vector of integer for classes to filter out of the point cloud. If NULL, all classes are kept.
# @param plot_hist_days logical. Should the histogram of dates of acquisition be displayed. Default =FALSE
# @param gpstime_ref character. Default = "2011-09-14 01:46:40". The datetime corresponding to gpstime=0, in order to retrieve the real datetime of points.
#' It is expected to be in timezone UTC. Default is "2011-09-14 01:46:40" which is the standard GPS Time (1980-01-06 00:00:00)
#' plus 1e9 seconds, as defined in LAS 1.4 specifications.
#' @param traj sf object. Trajectory covering the LAS chunk. The sf object is expected to have a column gpstime and Point Z geometries.
#' If NULL, the function will try to compute it.
#' @param dtm rast or stars object. Digital terrain model (DTM) to use for height normalisation. It should be larger than the area of interest.
#' If provided, the DTM is tinned to interpolate at each point location. If NULL, the normalisation function is using the tin of ground points.
#' @return LAS object. A normalized point cloud (.laz) with several new attributes needed to run fCBDprofile_fuelmetrics, see details.
#' @details
#' The attributes added to the laz are:
#' - Zref : Z value before normalization, i.e. altitude of the point
#' - Easting, Northing, Elevation, Time that are the X,Y,Z coordinates of the sensor
#'   and the its GPStime for each point (obtained from lidR::track_sensor()).
#' @examples
#' \donttest{
#' path2laz <- system.file("extdata", "M30_FontBlanche.laz", package = "lidarforfuel")
#' M30_FontBlanche_pretreated <- fPCpretreatment(path2laz)
#' # displaying the new attributes in the las
#' names(M30_FontBlanche_pretreated)
#' }
#' @import data.table
#' @export
fPCpretreatment <- function(
  chunk,
  classify = FALSE,
  # season_filter = 1:12,
  # deviation_days = Inf,
  exclude_classes = NULL,
  # plot_hist_days = FALSE,
  # gpstime_ref = "2011-09-14 01:46:40",
  traj = NULL,
  dtm = NULL
) {
  Classification <- NULL
  # read chunk
  if (inherits(chunk, "LAS")) {
    las <- chunk
  } else {
    las <- lidR::readLAS(chunk)
  }
  # las <- filter_seasons(las, months = season_filter, gpstime_ref = gpstime_ref, plot_hist_days = plot_hist_days)
  # las <- filter_date_mode(las, deviation_days, gpstime_ref = gpstime_ref, plot_hist_days = plot_hist_days)

  if (lidR::is.empty(las)) {
    return(NULL)
  }

  if (is.null(traj)) {
    warning(paste0(
      "Computing trajectory from LAS file...\n",
      "Trajectory should rather be computed outside pretreatment, ",
      "with a buffer (e.g. 500m) to avoid border effects."
    ))
    traj <- get_traj(las, thin = 0.0001, interval = .2, rmdup = TRUE, renum = TRUE)
  }
  las <- add_traj_to_las(las, traj)

  if (classify == TRUE) {
    lidR::classify_ground(las, algorithm = lidR::csf())
  }
  # filter classes, typically 66, i.e. virtual points in lidar-hd
  if (!is.null(exclude_classes)) {
    las <- lidR::filter_poi(las, !(Classification %in% exclude_classes))
  }

  # if (norm_ground == TRUE){
  #   # Filter ground points
  #   las_ground=lidR::filter_ground(lasdtm = lidR::rasterize_terrain(las_ground, algorithm = tin(),res=3)
  #   dtm_las=LAS(data.table(as.data.frame(dtm,xy=T)))
  #   # calculate normals on dtm and get vector components
  #   dtm_las=geom_features(las=dtm_las,search_radius = 6,features_list = c("Nx","Ny","Nz"))
  #
  #   # Find closest neighboor between normal DTM and las and attribute normal DTM to each point of the las
  #   nn2_las_DTM=nn2(dtm_las@data[,1:3],las@data[,1:3],k=1)
  #   las@data=cbind(las@data,dtm_las@data[nn2_las_DTM$nn.idx,4:6])
  # }


  # LMA = 140,
  # WD = 591,
  # WD_bush = 591,
  # LMA_bush = 140,
  # H_strata_bush = 2,
  # # LMA
  # if (is.numeric(LMA)) {
  #   las@data[["LMA"]] <- LMA
  # } else {
  #   ## Load LMA map
  #   LMA_map <- terra::rast(LMA)
  #   ### Add LMA to point cloud
  #   las <- lidR::merge_spatial(las, LMA_map$LMA, attribute = "LMA")
  # }
  # if (is.numeric(WD)) {
  #   las@data[["WD"]] <- WD
  # } else {
  #   ## Load LMA map
  #   WD_map <- terra::rast(WD)
  #   ### Add WD to point cloud
  #   las <- lidR::merge_spatial(las, WD_map$WD, attribute = "WD")
  # }

  # Normalize height
  las <- lidR::normalize_height(las = las, algorithm = lidR::tin(), dtm = dtm)
  las <- lidR::classify_noise(las, lidR::sor(5, 10))

  # las@data[Z <= H_strata_bush]$LMA <- LMA_bush
  # las@data[Z <= H_strata_bush]$WD <- WD_bush
  # # add names to laz
  # las <- lidR::add_lasattribute(las, name = "LMA", desc = "leaf mass area")
  # las <- lidR::add_lasattribute(las, name = "WD", desc = "Wood density")
  # las <- lidR::add_lasattribute(las, name = "Zref", desc = "original Z")
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
