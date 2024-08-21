#' Sdm function
#' 
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod south,
#' longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex sole,
#' yellowtail
#' 
#' @param x species_df file
#' @param y species_files
#' 
species_sdm_fn <- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid","iid"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit) 
}

#' Sdm function but uses delta_lognormal family
#' petrale sole, sablefish
#' 
#' @param x species_df file
#' @param y species_files
#' 
species_sdm_lognormal_fn <- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid","iid"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit)
}


canary_sdm_fn<- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = FALSE,
    spatiotemporal = as.list(c("iid","off"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit)
  
}

darkblotched_sdm_fn<- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 250)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off","iid"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit)
  
}

shortspine_sdm_fn<- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass + Depth_m + (Depth_m^2), #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid","iid"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit)
}

widow_sdm_fn<- function(x,y){ 
  #make mesh
  mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)
  
  #fit model
  fit<- sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off","off"))
  )
  
  #save file
  saveRDS(fit, paste0("fit_",y,".rds"))
  
  return(fit)
}