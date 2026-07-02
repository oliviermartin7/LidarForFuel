# LidarForFuel - AI Agent Guide

This guide helps AI coding agents understand the LidarForFuel package architecture, development patterns, and completion workflows.

**Package Purpose:** Compute fuel metrics from Airborne LiDAR data for wildfire hazard mapping.  
**Reference Paper:** [Martin-Ducup et al 2025](https://www.sciencedirect.com/science/article/pii/S0168192324004544)  
**Version:** 1.0.0.9009 | **License:** CeCILL

---

## Core Pipeline & Architecture

### Two-Stage Processing
LidarForFuel implements a fixed two-stage pipeline:

1. **[fPCpretreatment](R/fPCpretreatment.R)** — Normalize point cloud + inject attributes
   - Ground detection & height normalization
   - Adds: sensor trajectory (Easting, Northing, Elevation), LMA (Leaf Mass Area), WD (Wood Density)
   - Supports both individual LAS files and lidR catalog workflows

2. **[fCBDprofile_fuelmetrics](R/fCBDprofile_fuelmetrics.R)** — Invert CBD profile + extract metrics
   - Converts normalized cloud → vertical Canopy Bulk Density (CBD) profile
   - Outputs 23+ fuel metrics + 150 profile layers
   - Can operate on individual plots or via `lidR::pixel_metrics()` for large-scale mapping

**Important:** fCBDprofile_fuelmetrics requires a pretreated cloud from fPCpretreatment.

### Module Organization

| File | Purpose | Key Functions |
|------|---------|---|
| [fPCpretreatment.R](R/fPCpretreatment.R) | Cloud normalization & attribute enrichment | fPCpretreatment |
| [fCBDprofile_fuelmetrics.R](R/fCBDprofile_fuelmetrics.R) | Core fuel metric engine | fCBDprofile_fuelmetrics |
| [ffuelmetrics.R](R/ffuelmetrics.R) | Metric derivation algorithms | ffuelmetrics |
| [ffuelmetrics2.R](R/ffuelmetrics2.R) | Alternative metric implementations | ffuelmetrics2 |
| [pad_metrics.R](R/pad_metrics.R) | Plant Area Density profile inversion | pad_metrics, .pad_metrics |
| [trajectory.R](R/trajectory.R) | Sensor tracking | get_traj, add_traj_to_las |
| [filters.R](R/filters.R) | GPS time & seasonal filtering | filter_gpstime, .filter_gpstime, is_in_season, is_near_date_mode |
| [class_metrics.R](R/class_metrics.R) | Classification-based metrics | class_metrics, .class_metrics |
| [fuel_mass_area.R](R/fuel_mass_area.R) | FMA calculations | fuel_mass_area |
| [utils.R](R/utils.R) | Raster utilities & helpers | parse_pad_heights, datetime_to_gpstime, lasrenumber, lasrmdup, pixel_filter |

---

## Development Patterns & Completion Workflows

### Adding or Modifying Functions

**1. Function Template (Use Roxygen2)**
All exported functions must have Roxygen documentation:

```r
#' Brief one-line description
#'
#' Detailed description with context.
#'
#' @param arg1 Description of arg1 (type, constraints)
#' @param arg2 Description of arg2 (default behavior if applicable)
#'
#' @return Description of return value (type, structure, bands if raster)
#'
#' @details Additional technical details, algorithm notes, or gotchas
#'
#' @examples
#' \donttest{
#' # Example 1: typical usage
#' result <- my_function(input_data)
#'
#' # Example 2: advanced usage (if applicable)
#' result2 <- my_function(input_data, custom_param = value)
#' }
#'
#' @export
my_function <- function(arg1, arg2 = default_value) {
  # Implementation
}
```

**Roxygen Notes:**
- Markdown is enabled: Use `[link text](url)` for references, `_italic_`, `**bold**`
- Use `\donttest{}` for expensive computations (functions requiring large LAS files or long processing)
- Private functions (dot-prefixed like `.pad_metrics`) can be exported for internal use
- Auto-generates NAMESPACE on `roxygen2::roxygenise()`

**2. After Implementation: Regenerate Documentation**
```r
roxygen2::roxygenise()  # Updates NAMESPACE, .Rd files
```

---

### Testing Pattern

**Framework:** testthat (all tests in `tests/testthat/` prefix with `test-`)

**Running Tests:**
```r
Sys.setenv(NOT_CRAN = "true")  # REQUIRED for tests to run (skipped on CRAN)
devtools::test()                # Runs all tests
```

**Test Data Available:**
- `inst/extdata/M30_FontBlanche.laz` — Raw point cloud (3.2 MB)
- `inst/extdata/M30_FontBlanche_pretreated.laz` — Pre-processed (used to skip fPCpretreatment)
- `inst/extdata/BD_profile.txt` — Bulk density profile reference data

**Test Template:**
```r
test_that("function does expected task", {
  las <- readLAS("inst/extdata/M30_FontBlanche_pretreated.laz")
  result <- my_function(las)
  
  expect_true(is.numeric(result) || is(result, "SpatRaster"))
  expect_gt(nrow(result), 0)
})
```

---

### Data Type Conventions

**LiDAR Data:**
- Input/Output: LAS/LAZ (via [lidR](https://cran.r-project.org/package=lidR)) — class `LAS` or `LAScatalog`
- Point attributes: X, Y, Z, ReturnNumber, gpstime, Classification, Easting, Northing, Elevation (after preprocessing)
- Use `readLAS()` or `read.las()` from lidR

**Rasters:**
- Primary class: `SpatRaster` (from [terra](https://cran.r-project.org/package=terra))
- Legacy: `RasterStack` (from raster package) — still supported, but terra preferred
- Output bands: 23 metrics + 150 CBD profile layers (per `fCBDprofile_fuelmetrics`)
- Convention: Missing values = -1 (must post-process with `terra::subst(raster, -1, NA)`)

**Tables:**
- Primary class: `data.table` (all operations optimized with data.table)
- Use `data.table::` functions, leverage `.[]` syntax for efficiency
- Also: tibble, dplyr (imported)

---

### Common Development Tasks

#### **Extending fCBDprofile_fuelmetrics with new metric**

1. **Compute metric in [ffuelmetrics.R](R/ffuelmetrics.R) or [ffuelmetrics2.R](R/ffuelmetrics2.R)**
   ```r
   # Example: add new metric derivation
   my_new_metric <- function(cbd_profile, threshold) {
     # Logic here
     return(scalar_value)
   }
   ```

2. **Integrate into [fCBDprofile_fuelmetrics.R](R/fCBDprofile_fuelmetrics.R)**
   - Add parameter to function signature
   - Call your metric function
   - Add to output list/raster stack

3. **Document in Roxygen**
   - Add `@param` for new parameter
   - Update `@return` to reflect new band(s)
   - Add example in `@examples`

4. **Test:**
   ```r
   Sys.setenv(NOT_CRAN = "true")
   devtools::load_all()
   result <- fCBDprofile_fuelmetrics(pretreated_las, your_new_param = value)
   expect_true("new_metric" %in% names(result))
   ```

5. **Regenerate docs:**
   ```r
   roxygen2::roxygenise()
   ```

#### **Adding Helper Function for fPCpretreatment**

1. Implement in [utils.R](R/utils.R) (if generic) or directly in [fPCpretreatment.R](R/fPCpretreatment.R)
2. Use data.table for performance (`data.table::` functions)
3. Document with Roxygen if exported (or `@keywords internal` if private)
4. Test with sample LAS file from inst/extdata

#### **Fixing or Optimizing Data Handling**

- LiDAR point cloud operations → Check [fPCpretreatment.R](R/fPCpretreatment.R), [trajectory.R](R/trajectory.R)
- Bulk density inversion → Check [pad_metrics.R](R/pad_metrics.R), [fCBDprofile_fuelmetrics.R](R/fCBDprofile_fuelmetrics.R)
- Metric extraction → Check [ffuelmetrics.R](R/ffuelmetrics.R), [ffuelmetrics2.R](R/ffuelmetrics2.R)
- Raster post-processing → Check [utils.R](R/utils.R)
- Use `data.table::` for vectorized operations on large point clouds

---

## Key Parameters & Defaults

### GPS Time Reference
```r
default_origin <- as.POSIXct("2011-09-14 01:46:40", tz = "UTC")  # LAS 1.4 standard
```

### Flight Height Safeguard
- Default limit: 800m above canopy (prevents sensor artifacts at extreme altitudes)

### Minimum Point Count
- Default: 400 points per pixel for metric computation (ensures statistical validity)

### LMA/WD Stratification
- Above 2m: Use main canopy LMA/WD values
- Below 2m: Can override with `LMA_bush` / `WD_bush` parameters (future: user-defined layers)

---

## End-to-End Pipeline Example

See [Code_examples/Processing_Pipeline_example_V1.0.R](Code_examples/Processing_Pipeline_example_V1.0.R) for full example. Typical flow:

```r
# 1. Load or download LiDAR tiles (IGN LiDAR HD grid in France)
las <- lidR::readLAS("path/to/tiles/*.laz", select = "xyz", filter = "-keep_first")

# 2. Preprocess
las_pretreated <- fPCpretreatment(
  las, 
  classify = FALSE,
  traj = NULL,  # Auto-computed if NULL (slower)
  LMA = 1.2, 
  WD = 0.5
)

# 3a. Single plot: Extract metrics
metrics <- fCBDprofile_fuelmetrics(
  X = las_pretreated@data$X,
  Y = las_pretreated@data$Y,
  Z = las_pretreated@data$Z,
  ...other required columns...,
  datatype = "List"  # Returns named list of metrics
)

# 3b. Large scale: Raster map (10m resolution)
raster_metrics <- lidR::pixel_metrics(
  las_pretreated,
  ~ fCBDprofile_fuelmetrics(
    X, Y, Z, Zref, gpstime, ReturnNumber, 
    Easting, Northing, Elevation, LMA, threshold, WD, 
    datatype = "Pixel"
  ),
  res = 10
)

# 4. Post-process: Replace -1 (missing) with NA
raster_clean <- terra::subst(raster_metrics, -1, NA)

# 5. Inspect output
print(names(raster_clean))  # 173 bands (23 metrics + 150 profile layers)
terra::plot(raster_clean[[2]])  # Plot Profile Type (Band 2)
```

---

## Important Gotchas

1. **GPS Time Format:** Default epoch = "2011-09-14 01:46:40" (LAS 1.4). Use `gpstime_to_datetime()` or `datetime_to_gpstime()` for conversion.

2. **Pretreated Cloud Required:** Always use fPCpretreatment output with fCBDprofile_fuelmetrics. Direct raw LAS will fail or produce invalid metrics.

3. **Missing Values Convention:** Fuel metric rasters use -1 for NA. Post-process with `terra::subst(raster, -1, NA)` immediately after generation.

4. **Test Environment:** Tests only run if `Sys.setenv(NOT_CRAN = "true")` is set (see [tests/testthat.R](tests/testthat.R)). Always set this before running tests.

5. **Flight Height Artifact:** Functions check for sensor heights > 800m above canopy. Extreme altitudes may produce invalid profiles.

6. **Large-scale Processing:** Use `lidR::catalog_apply()` or `lidR::pixel_metrics()` for production workflows — vectorized operations in R can be slow on single large files.

---

## Building & Releasing

```r
# Development workflow
devtools::load_all()       # Reload package (no install needed)
roxygen2::roxygenise()     # Update documentation
Sys.setenv(NOT_CRAN = "true"); devtools::test()  # Run tests

# Installation (local)
devtools::install()        # Install locally

# Installation (public)
devtools::install_github("oliviermartin7/lidarforfuel")  # Installs from GitHub main branch

# Package check (pre-release)
devtools::check()          # Full R CMD check
```

---

## Common Completion Scenarios

### Scenario: Add validation metric
1. Write metric function in [ffuelmetrics.R](R/ffuelmetrics.R)
2. Integrate into [fCBDprofile_fuelmetrics.R](R/fCBDprofile_fuelmetrics.R) output
3. Add @param + @return documentation
4. Write test in [tests/testthat/](tests/testthat/)
5. Run `roxygen2::roxygenise()` + `devtools::test()`

### Scenario: Optimize slow operation
1. Identify bottleneck (likely in data.table operations or spatial joins)
2. Profile with `profvis::profvis(code)` if available
3. Use vectorized data.table operations (avoid loops)
4. Test performance with `microbenchmark::microbenchmark()`
5. Verify output matches original behavior before/after

### Scenario: Fix failing test
1. Check test file in [tests/testthat/](tests/testthat/)
2. Inspect error message — often GPS time, coordinate, or data type mismatch
3. Verify test data is available in [inst/extdata/](inst/extdata/)
4. Use `debugonce()` or `browser()` to step through function
5. Run `devtools::test()` to re-check

---

## Quick Reference: File Locations

| Task | File(s) |
|------|---------|
| Add function | [R/](R/) (new or existing module) → Roxygen @export |
| Fix metric | [ffuelmetrics.R](R/ffuelmetrics.R) or [ffuelmetrics2.R](R/ffuelmetrics2.R) |
| Fix pretreatment | [fPCpretreatment.R](R/fPCpretreatment.R) + [trajectory.R](R/trajectory.R) |
| Write tests | [tests/testthat/](tests/testthat/) (prefix: `test-*.R`) |
| Reference data | [inst/extdata/](inst/extdata/) (*.laz, *.txt) |
| Code examples | [Code_examples/](Code_examples/) (processing pipelines) |
| Documentation | Roxygen in function headers (via @param, @return, @examples) |
| Package config | [DESCRIPTION](DESCRIPTION), [NAMESPACE](NAMESPACE) (auto-generated), [LidarForFuel.Rproj](LidarForFuel.Rproj) |

---

## Resources

- **Paper:** [Martin-Ducup et al 2025 (Ag. For. Meteorology)](https://www.sciencedirect.com/science/article/pii/S0168192324004544)
- **lidR Docs:** [r-lidar.github.io/lidR](https://r-lidar.github.io/lidR/) — catalog, pixel_metrics, LAS operations
- **terra Docs:** [rspatial.org/terra](https://rspatial.org/terra/) — raster manipulation
- **data.table Docs:** [rdatatable.gitlab.io](https://rdatatable.gitlab.io/data.table/) — efficient table operations
- **Roxygen2:** [roxygen2.r-lib.org](https://roxygen2.r-lib.org/) — documentation generation
- **testthat:** [testthat.r-lib.org](https://testthat.r-lib.org/) — unit testing framework

---

**Last Updated:** 2025-06-12 | **Package Version:** 1.0.0.9009
