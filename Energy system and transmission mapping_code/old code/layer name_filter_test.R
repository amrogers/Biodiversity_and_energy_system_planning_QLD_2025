# First, let's see if we can read the GDB at all and what GDAL sees
cat("Checking GDB with different methods...\n")

# Method 1: Check if the GDB path exists
if (file.exists(gdb_path)) {
  cat("GDB path exists\n")
} else {
  cat("GDB path does NOT exist\n")
}

# Method 2: Try to list layers using terra instead of sf
cat("\nTrying to read layers with terra...\n")
terra_layers <- tryCatch({
  terra::describe(gdb_path)
}, error = function(e) {
  cat("Terra error:", e$message, "\n")
  return(NULL)
})

if (!is.null(terra_layers)) {
  print(terra_layers)
}

# Try to get all layers with terra and filter for "ext" prefix
cat("Listing all layers with 'ext' prefix...\n")

# First, let's see what terra::describe returns
layer_info <- tryCatch({
  terra::describe(gdb_path)
}, error = function(e) {
  cat("Error with terra::describe:", e$message, "\n")
  NULL
})

if (!is.null(layer_info)) {
  cat("terra::describe() output type:", class(layer_info), "\n")
  
  # Extract layer names depending on the structure
  if (is.data.frame(layer_info)) {
    all_layers <- layer_info$name
  } else if (is.list(layer_info)) {
    all_layers <- names(layer_info)
  } else if (is.character(layer_info)) {
    all_layers <- layer_info
  } else {
    cat("Unexpected output structure from terra::describe\n")
    print(str(layer_info))
    all_layers <- NULL
  }
  
  if (!is.null(all_layers)) {
    # Filter for layers starting with "ext"
    ext_layers <- all_layers[grepl("^ext", all_layers)]
    
    cat("\nFound", length(ext_layers), "layers starting with 'ext':\n")
    for (layer in ext_layers) {
      cat(" -", layer, "\n")
    }
  }
} else {
  cat("Could not retrieve layer information\n")
}


# Extract a single raster layer
single_layer_name <- "ext2_builtUpAreasBin_pv"  # Replace with your specific layer name
output_file <- paste0("Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/", single_layer_name, ".tif")


# Make sure the output directory exists
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Extracting single layer:", single_layer_name, "\n")

# Read raster from GDB using the correct OpenFileGDB format
raster_obj <- tryCatch({
  gdb_layer_path <- paste0('OpenFileGDB:"', gdb_path, '":"', single_layer_name, '"')
  cat("Using path:", gdb_layer_path, "\n")
  terra::rast(gdb_layer_path)
}, error = function(e) {
  cat("Error reading layer:", e$message, "\n")
  return(NULL)
})

if (!is.null(raster_obj)) {
  # Save as GeoTIFF
  tryCatch({
    terra::writeRaster(raster_obj, output_file, overwrite = TRUE)
    cat("Successfully saved to:", output_file, "\n")
    
    # Print some info about the raster
    cat("Raster info:\n")
    print(raster_obj)
  }, error = function(e) {
    cat("Error saving raster:", e$message, "\n")
  })
} else {
  cat("Failed to read raster\n")
}