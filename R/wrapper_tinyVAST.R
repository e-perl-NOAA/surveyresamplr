wrapper_tinyVAST <- function(x, y, z, dir_spp, spp_info, n_knots = 500) {
  # Similar structure to wrapper_sdmtmb but with tinyVAST-specific settings
  # Create mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude_dd", "latitude_dd"), n_knots = n_knots)
  
  # Fit model
  eval(parse(text = paste0(
    "fit <- sdmTMB::sdmTMB(", 
    spp_info$model_fn,
    ", data = x,",
    " mesh = mesh,", 
    " family = ", spp_info$model_family, "(),",
    ' time = "year",',
    " anisotropy = ", spp_info$model_anisotropy, ",",
    ' spatiotemporal = as.list(c("',
    gsub(pattern = ", ", replacement = '", "', x = spp_info$model_spatiotemporal),
    '")))'
  )))
  
  # Make predictions
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2, bias_correct = TRUE)
  
  # Save results
  out <- list(
    "fit" = fit, 
    "predictions" = predictions,
    "index" = index
  )
  saveRDS(
    out,
    file.path(dir_spp, paste0("modelout_", y, ".rds"))
  )
  
  return(out)
}