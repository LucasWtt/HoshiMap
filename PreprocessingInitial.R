# Info available here : https://github.com/LucasWtt/Hoshimap


#==================================================================================
#Install packages
#==================================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra, sf, dplyr, stringr, googledrive,rgdal,tmap,raster, ggplot2, tidyterra, viridis, ggnewscale, leaflet, shiny, shinythemes, bslib)

#================================================================================== 
#Import, clean and combine shp / rasters
#==================================================================================
# Country Shapefile
JP_Shp <- vect("data/JP_Shp/jp.shp")

# Aerosol optical thickness
JP_OpticalDepth <- rast("data/Aerosol_Optical_Thickness.tif")
JP_OpticalDepth[JP_OpticalDepth == -9999] <- NA
JP_OpticalDepth <- JP_OpticalDepth/10000
# Accessibility
JP_Accessibility <- rast("data/JP_cost/accessibility_to_cities_2015_v1.0.tif")
JP_Accessibility[JP_Accessibility == -9999] <- NA


#Combining 131 Landuse rasters (in dataset file)
landuse_dataset <- list.files("data/landuse_dataset", 
                              pattern = "\\.tif$", 
                              full.names = TRUE)
landuse_rasters <- lapply(landuse_dataset, rast)
landuse_merged <- do.call(merge, landuse_rasters)

writeRaster(landuse_merged, "data/JP_Landuse.tif", overwrite=TRUE)

#==================================================================================
#Compress DEM and Skyview rasters
#==================================================================================
JP_DEM <- rast("data/JP_DEM.tif")
writeRaster(
  JP_DEM,
  "JP_DEM.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

#Compress Skyview raster
JP_Skyview <- rast("data/JP_Skyview.tif")
writeRaster(
  JP_Skyview,
  "Skyview.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

#==================================================================================
#Convert Landuse and Light pollution from .adf to .tif
#==================================================================================
# Landuse
JP_Landuse <- rast("data/JP_Landuse")
summary(JP_Landuse)
writeRaster(JP_Landuse, "data/JP_Landuse.tif", overwrite=TRUE,gdal=c("COMPRESS=LZW", "TILED=YES"))

# Light pollution
JP_Lightpollution <- rast("data/JP_Lightpollution")
summary(JP_Lightpollution)
writeRaster(JP_Lightpollution, "data/JP_Lightpollution.tif", overwrite=TRUE,gdal=c("COMPRESS=LZW", "TILED=YES"))

#==================================================================================
#Crop to country shapefile
#==================================================================================
# DEM
JP_DEM <- crop(JP_DEM, JP_Shp)
JP_DEM <- mask(JP_DEM, JP_Shp)

# Skyview
JP_Skyview <- crop(JP_Skyview, JP_Shp)
JP_Skyview <- mask(JP_Skyview, JP_Shp)

# Landuse
JP_Landuse <- crop(JP_Landuse, JP_Shp)
JP_Landuse <- mask(JP_Landuse, JP_Shp)

# Light pollution
JP_Lightpollution <- crop(JP_Lightpollution, JP_Shp)
JP_Lightpollution <- mask(JP_Lightpollution, JP_Shp)

#Accessibility
JP_Accessibility <- crop(JP_Accessibility, JP_Shp)
JP_Accessibility <- mask(JP_Accessibility, JP_Shp)

#Optical Depth
JP_OpticalDepth <- crop(JP_OpticalDepth, JP_Shp)
JP_OpticalDepth <- mask(JP_OpticalDepth, JP_Shp)

#================================================================================== 
#Setup to create 46 prefecture rasters
#==================================================================================
#Projection check
crs(JP_Shp)
crs(JP_DEM)
crs(JP_Skyview)
crs(JP_Lightpollution)
crs(JP_Landuse)
crs(JP_Accessibility)
crs(JP_OpticalDepth)

# Rasters list
rasters <- list(
  DEM = JP_DEM,
  Skyview = JP_Skyview,
  Landuse = JP_Landuse,
  Lightpollution = JP_Lightpollution,
  Accessibility = JP_Accessibility,
  OpticalDepth = JP_OpticalDepth
)

#==================================================================================
#Exception : Okinawa exclusion
#==================================================================================
okinawa_vect <- JP_Shp[JP_Shp$name == "Okinawa", ]

overlap <- lapply(names(rasters), function(raster_name) {
  ok <- rasters[[raster_name]]
  
  r_ext <- ext(ok)
  ok_ext <- ext(okinawa_vect)
  
  intersects <- !(ok_ext$xmax < r_ext$xmin ||
                    ok_ext$xmin > r_ext$xmax ||
                    ok_ext$ymax < r_ext$ymin ||
                    ok_ext$ymin > r_ext$ymax)
  
  data.frame(
    raster = raster_name,
    status = if(intersects) "OK" else "Problem"
  )
}) %>% do.call(rbind, .)

print(overlap)

# Overlap condition is not fulfilled, Okinawa not taken into account.

#==================================================================================
#Creation of individual rasters (46x4 rasters)
#==================================================================================
# Create output file
for(name in names(rasters)) {
  dir.create(paste0("output/", name, "_prefectures"), showWarnings = FALSE)
}

# Loop on every prefecture
for(i in 1:nrow(JP_Shp)) {
  
  pref <- JP_Shp[i,]
  pref_name <- pref$name[1]
  
  # Ignore Okinawa
  if(pref_name == "Okinawa") {
    cat("Skipping Okinawa\n")
    next
  }
  
  # Loop for every raster
  for(raster_name in names(rasters)) {
    raster_layer <- rasters[[raster_name]]
    
    # Try to crop and mask
    result <- try({
      pref_raster_crop <- crop(raster_layer, pref)
      pref_raster_mask <- mask(pref_raster_crop, pref)
      
      out_file <- paste0("output/", raster_name, "_prefectures/", raster_name, "_",       pref_name, ".tif")
      writeRaster(pref_raster_mask, out_file, overwrite=TRUE)
      
      cat("OK:", raster_name, "-", pref_name, "\n")
    }, silent = TRUE)
    
    if(inherits(result, "try-error")) {
      cat("PROBLEM:", raster_name, "-", pref_name, "\n")
    }
  }
}


#==================================================================================
# Create Hillshade and Slope from DEM_prefectures
#==================================================================================

# Input DEM folder
path_dem <- "output/DEM_prefectures/"

# Output folders
dir.create("output/Hillshade_prefectures", showWarnings = FALSE)
dir.create("output/Slope_prefectures", showWarnings = FALSE)


# List DEM files
dem_files <- list.files(path_dem, pattern = "\\.tif$", full.names = TRUE)

# Loop over each prefecture DEM
for (f in dem_files) {
  
  # Extract prefecture name
  pref_name <- gsub("DEM_|\\.tif", "", basename(f))
  message(paste("Processing:", pref_name))
  
  # Load DEM
  dem <- rast(f)
  
  # ---- SLOPE ----
  slope <- terrain(dem, v = "slope", unit = "degrees")
  writeRaster(
    slope,
    paste0("output/Slope_prefectures/Slope_", pref_name, ".tif"),
    overwrite = TRUE
  )
  
  # ---- HILLSHADE ----
  aspect <- terrain(dem, v = "aspect", unit = "degrees")
  
  hillshade <- shade(
    slope = terrain(dem, v = "slope", unit = "radians"),
    aspect = terrain(dem, v = "aspect", unit = "radians"),
    angle = 45,   # sun elevation
    direction = 315 # NW light (classic cartography)
  )
  
  writeRaster(
    hillshade,
    paste0("output/Hillshade_prefectures/Hillshade_", pref_name, ".tif"),
    overwrite = TRUE
  )
  
  cat("OK:", pref_name, "\n")
}

message("Hillshade and Slope generation completed")

#==================================================================================
#Standardize rasters resolutions (landuse and light pollution)
#==================================================================================
# Define output paths
path_dem <- "output/DEM_prefectures/"
path_light <- "output/Lightpollution_prefectures/"
path_landuse <- "output/Landuse_prefectures/"
path_accessibility <- "output/Accessibility_prefectures/"
path_opticaldepth <- "output/OpticalDepth_prefectures/"

# Create new output files without deleting previous ones
dir.create("output/Lightpollution_aligned", showWarnings = FALSE)
dir.create("output/Landuse_aligned", showWarnings = FALSE)
dir.create("output/Accessibility_aligned",showWarnings = FALSE)
dir.create("output/OpticalDepth_aligned",showWarnings = FALSE)


# Define reference raster (DEM)
dem_files <- list.files(path_dem, pattern = "\\.tif$", full.names = TRUE)

# Loop by prefectures
for (f_dem in dem_files) {
  
  pref_name <- str_remove(basename(f_dem), "DEM_|\\.tif")
  message(paste("Running prefecture :", pref_name))
  
  # Load reference raster (DEM)
  ref_raster <- rast(f_dem)
  
  # --- Light Pollution ---
  file_light <- paste0(path_light, "Lightpollution_", pref_name, ".tif")
  if (file.exists(file_light)) {
    r_light <- rast(file_light)
    # Bilinear resampling (continuous data)
    light_aligned <- resample(r_light, ref_raster, method = "bilinear")
    writeRaster(light_aligned,paste0("output/Lightpollution_aligned/Lightpollution_Aligned_", pref_name, ".tif"),overwrite=TRUE)
  }
  
  # --- Land Use ---
  file_landuse <- paste0(path_landuse, "Landuse_", pref_name, ".tif")
  if (file.exists(file_landuse)) {
    r_landuse <- rast(file_landuse)
    # Nearest neighbor resampling (discrete data)
    landuse_aligned <- resample(r_landuse, ref_raster, method = "near")
    writeRaster(landuse_aligned,paste0("output/Landuse_aligned/Landuse_aligned_", pref_name, ".tif"),overwrite=TRUE)
  }
  
  # --- Accessibility ---
  file_accessibility <- paste0(path_accessibility, "Accessibility_", pref_name, ".tif")
  if (file.exists(file_accessibility)) {
    r_accessibility <- rast(file_accessibility)
    # Bilinear resampling (continuous data)
    accessibility_aligned <- resample(r_accessibility, ref_raster, method = "bilinear")
    writeRaster(accessibility_aligned,paste0("output/Accessibility_aligned/Accessibility_aligned_", pref_name, ".tif"),overwrite=TRUE)
  }
  
  # --- Optical Depth ---
  file_accessibility <- paste0(path_opticaldepth, "OpticalDepth_", pref_name, ".tif")
  if (file.exists(file_opticaldepth)) {
    r_opticaldepth <- rast(file_opticaldepth)
    # Bilinear resampling (continuous data)
    opticaldepth_aligned <- resample(r_opticaldepth, ref_raster, method = "bilinear")
    writeRaster(opticaldepth_aligned,paste0("output/OpticalDepth_aligned/OpticalDepth_aligned_", pref_name, ".tif"),overwrite=TRUE)
  }
}
message("Alignment completed for 138 rasters")


#==================================================================================
#Divide shapefile in prefectures
#==================================================================================
shp <- st_read("data/JP_Shp/jp.shp")
dir.create("Shapefile_prefectures", showWarnings = FALSE)

# Loop
for (i in 1:nrow(shp)) {
  
  name <- shp$name[i]
  
  name_clean <- gsub("[^[:alnum:]_]", "_", name)
  
  st_write(
    shp[i, ],
    paste0("Shapefile_prefectures/Shp_", name_clean, ".shp"),
    delete_dsn = TRUE
  )
}