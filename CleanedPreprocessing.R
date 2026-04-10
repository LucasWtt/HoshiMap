# Info available here : https://github.com/LucasWtt/Hoshimap


# ==============================================================================
# PACKAGES & SETUP
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra, sf, dplyr, stringr, googledrive, ggplot2, tidyterra)

# Create output structure
base_dirs <- c("DEM", "Landuse", "Lightpollution", "Accessibility", 
               "OpticalDepth", "Hillshade", "Slope", "Skyview")
lapply(paste0("output/", base_dirs, "_prefectures"), dir.create, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# DATA IMPORT & INITIAL CLEANING
# ==============================================================================
# Load Country Boundary
JP_Shp <- vect("data/JP_Shp/jp.shp")

# Load and Clean Rasters
JP_OpticalDepth  <- rast("data/Aerosol_Optical_Thickness.tif")
JP_OpticalDepth[JP_OpticalDepth == -9999] <- NA
JP_OpticalDepth  <- JP_OpticalDepth / 10000

JP_Accessibility <- rast("data/JP_cost/accessibility_to_cities_2015_v1.0.tif")
JP_Accessibility[JP_Accessibility == -9999] <- NA

# Combine Landuse Tiles
lu_list <- list.files("data/landuse_dataset", pattern = "\\.tif$", full.names = TRUE)
JP_Landuse <- do.call(merge, lapply(lu_list, rast))

# Load others
JP_DEM            <- rast("data/JP_DEM.tif")
JP_Skyview        <- rast("data/JP_Skyview.tif")
JP_Lightpollution <- rast("data/JP_Lightpollution")

# ==============================================================================
# CROP & MASK TO NATIONAL BOUNDARY
# ==============================================================================
# This standardizes the "Working Area" before splitting into prefectures
layers <- list(
  DEM             = JP_DEM,
  Skyview         = JP_Skyview,
  Landuse         = JP_Landuse,
  Lightpollution  = JP_Lightpollution,
  Accessibility   = JP_Accessibility,
  OpticalDepth    = JP_OpticalDepth
)

# Apply national crop/mask to all layers
layers <- lapply(layers, function(r) {
  r_crop <- crop(r, JP_Shp)
  mask(r_crop, JP_Shp)
})

# ==============================================================================
# PREFECTURE SPLITTING LOOP
# ==============================================================================
for(i in 1:nrow(JP_Shp)) {
  
  pref      <- JP_Shp[i,]
  pref_name <- gsub("[^[:alnum:]_]", "_", pref$name[1])
  
  if(pref_name == "Okinawa") next # Skipping due to coordinate mismatch
  
  message(paste("--- Processing Prefecture:", pref_name, "---"))
  
  # 1. Create Individual Prefecture Layers
  for(lyr_name in names(layers)) {
    
    # Crop and Mask to Prefecture
    p_crop <- crop(layers[[lyr_name]], pref)
    p_mask <- mask(p_crop, pref)
    
    # Save standard layers
    out_path <- paste0("output/", lyr_name, "_prefectures/", lyr_name, "_", pref_name, ".tif")
    writeRaster(p_mask, out_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    
    # 2. Generate Derived Terrain Layers (only if current layer is DEM)
    if(lyr_name == "DEM") {
      # Slope
      slp <- terrain(p_mask, v = "slope", unit = "degrees")
      writeRaster(slp, paste0("output/Slope_prefectures/Slope_", pref_name, ".tif"), overwrite = TRUE)
      
      # Hillshade
      asp <- terrain(p_mask, v = "aspect", unit = "radians")
      slp_rad <- terrain(p_mask, v = "slope", unit = "radians")
      hld <- shade(slp_rad, asp, angle = 45, direction = 315)
      writeRaster(hld, paste0("output/Hillshade_prefectures/Hillshade_", pref_name, ".tif"), overwrite = TRUE)
    }
  }
}

# ==============================================================================
# ALIGNMENT (RESAMPLING)
# ==============================================================================
# Ensure all rasters have the exact same grid as the DEM for Shiny math
for (i in 1:nrow(JP_Shp)) {
  pref_name <- gsub("[^[:alnum:]_]", "_", JP_Shp$name[i])
  if(pref_name == "Okinawa") next
  
  # Reference Raster (DEM)
  ref <- rast(paste0("output/DEM_prefectures/DEM_", pref_name, ".tif"))
  
  # Resample Continuous Data (Bilinear)
  to_resample_cont <- c("Lightpollution", "Accessibility", "OpticalDepth")
  for(lyr in to_resample_cont) {
    path <- paste0("output/", lyr, "_prefectures/", lyr, "_", pref_name, ".tif")
    if(file.exists(path)) {
      r <- rast(path)
      r_aligned <- resample(r, ref, method = "bilinear")
      writeRaster(r_aligned, path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    }
  }
  
  # Resample Categorical Data (Nearest Neighbor)
  lu_path <- paste0("output/Landuse_prefectures/Landuse_", pref_name, ".tif")
  if(file.exists(lu_path)) {
    r_lu <- rast(lu_path)
    lu_aligned <- resample(r_lu, ref, method = "near")
    writeRaster(lu_aligned, lu_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  }
}

# ==============================================================================
# EXPORT INDIVIDUAL SHAPEFILES
# ==============================================================================
shp_sf <- st_as_sf(JP_Shp)
dir.create("output/Shapefile_prefectures", showWarnings = FALSE)

for (i in 1:nrow(shp_sf)) {
  name_clean <- gsub("[^[:alnum:]_]", "_", shp_sf$name[i])
  st_write(shp_sf[i, ], 
           paste0("output/Shapefile_prefectures/Shp_", name_clean, ".shp"), 
           delete_dsn = TRUE, quiet = TRUE)
}

message("All preprocessing completed successfully.")