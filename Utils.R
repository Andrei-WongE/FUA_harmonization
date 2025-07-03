#  Read all layers or selected layers from gpkg file -----

read_gpkg_layers <- function(gpkg_path, selected_layers = NULL, quiet = TRUE) {
  
  # Get layers
  layers <- st_layers(gpkg_path)
  available_layers <- layers$name
  
  # If selected_layers is NULL, use all layers
  # If not NULL, handle both names and indices
  layers_to_read <- if (is.null(selected_layers)) {
    available_layers
  } else {
    # Convert numeric indices to layer names
    if (is.numeric(selected_layers)) {
      # Check if indices are valid
      if (any(selected_layers < 1) || any(selected_layers > length(available_layers))) {
        stop("Invalid layer index. Available indices: 1 to ", length(available_layers))
      }
      available_layers[selected_layers]
    } else {
      # Validate layer names
      if (!all(selected_layers %in% available_layers)) {
        invalid_layers <- selected_layers[!selected_layers %in% available_layers]
        stop("Invalid layer(s): ", paste(invalid_layers, collapse = ", "), 
             "\nAvailable layers: ", paste(available_layers, collapse = ", "))
      }
      selected_layers
    }
  }
  
  # Read layers
  result <- list()
  for(layer in layers_to_read) {
    if (!quiet) message("Reading layer: ", layer)
    result[[layer]] <- st_read(gpkg_path, layer = layer, quiet = quiet)
  }
  
  # If only one layer, return it directly
  if (length(result) == 1) {
    return(result[[1]])
  }
  
  return(result)
}

