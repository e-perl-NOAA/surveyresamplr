#######################################################################################################################################
#### resample survey data: Multiple species, multiple years
####
#######################################################################################################################################

####clear environment
rm(list=ls())

####set wds
wd <- getwd() 
output <- file.path(wd, "Results")
arrowtooth <- file.path(output, "Arrowtooth_flounder")
bocaccio <- file.path(output, "Bocaccio")
canary <- file.path(output, "Canary_rockfish")
darkblotched <- file.path(output, "Darkblotched_rockfish")
dover <- file.path(output, "Dover_sole")
lingcod_n <- file.path(output, "Lingcod_north")
lingcod_s <- file.path(output, "Lingcod_south")
longnose <- file.path(output, "Longnose_skate")
pop <- file.path(output, "Pacific_ocean_perch")
dogfish <- file.path(output, "Pacific_spiny_dogfish")
petrale <- file.path(output, "Petrale_sole")
rex <- file.path(output, "Rex_sole")
sablefish <- file.path(output, "Sablefish")
shortspine <- file.path(output, "Shortspine_thornyhead")
widow <- file.path(output, "Widow_rockfish")
yellowtail <- file.path(output, "Yellowtail_rockfish")
figures <- file.path(wd, "Figures")

#set the PAT if needed for package installation
#Sys.setenv(GITHUB_PAT = "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")

#remotes::install_github("pfmc-assessments/nwfscSurvey")
#remotes::install_github("pfmc-assessments/indexwc")

#load packages
#library(nwfscSurvey)
library(sampling)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(readr)
library(tibble)
#library(TMB)
#library(Matrix)
library(sdmTMB)
#library(indexwc)
library(doParallel)

#read in data
catch<-read.csv(file.path(wd, "data", "nwfsc_bt_fmp_spp.csv"))

#split by year
catch_split<- split(catch, catch$Year)

#create a vector of tows for including or excluding.
tow_fn<-function(x){
  tows<- as.data.frame(x$Trawl_id)
  tows<-unique(tows)
  tows<-as.data.frame(tows[!is.na(tows)])
  names(tows)<-"Trawl_id"
  return(tows)
}

tows<-lapply(catch_split,tow_fn)

#specify how to downsample; for simple random sampling, a proportion of stations should do
include_or_exclude <- function(df, proportions) {
  
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- replicate(10, sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)), simplify = F)
    
    # Create a new dataframe with the random assignments
    lapply(random_vectors, function (rv){
      cbind(df, RandomAssignment = rv)
    })
    
  })
  
  #flatten into single list
  result_list<- do.call(c, result_list)
  
  # Set names for the list elements based on proportions
  names(result_list) <- paste0(rep(proportions, each = 10), "_", rep(1:10, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}

# Assign random 1s and 0s based on the specified proportions to a list of dataframes
props<-as.data.frame(seq(0.1,1.0, by = 0.1))
names(props)<-"Trawl_id"

#match the structure of the catch data
props<-rep(props,length(tows))

tows_assigned<-map2(tows, props, include_or_exclude)

#remove replicates of the 1 effort level
tows_assigned<- lapply(tows_assigned,function(x){
  x<- x[1:91]
  return(x)
})

tows_assigned_resampled<- purrr::map(tows_assigned, function(x){
  purrr::map(x, function(y){
    y[y$RandomAssignment == 1,]
  })
  }
)

tows_assigned_resampled<-unlist(tows_assigned_resampled, recursive = F)

#merge with catch
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  merged_dfs <- lapply(list_of_dfs, function(df) {
    merged_df <- merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
}

#split by species
split_spp<-function(x){
  spp_list<- split(x,x$Common_name)
  return(spp_list)
}

####### calculate model based indices for all species and effort levels ######################################
####### generic functions
#dataframe of the fit
fit_df_fn<- function(fit){
  fit_df<-tidy(fit, conf.int = T)
  return(fit_df)
  
  filename <- paste0(name(fit), "_fit_df.csv")
  write.csv(fit_df, file = filename, row.names = F)
}

#parameter estimates
fit_pars_fn<- function(fit){
  fit_pars<-tidy(fit, effects = "ran_pars", conf.int = T)
  return(fit_pars)
  
  filename <- paste0(name(fit), "_fit_pars.csv")
  write.csv(index, file = filename, row.names = F)
}

#diagnostics
fit_check_fn<- function(fit){
  fit_check<- sanity(fit)
  return(fit_check)
  
  filename <- paste0(name(fit), "_fit_check.csv")
  write.csv(index, file = filename, row.names = F)
}

#get index
index_fn<- function(fit, x, names){ 
  p_grid<-predict(fit, newdata = x)
  
  grid_yrs <- replicate_df(p_grid, "year", unique(x$Year))
  
  p_sbf <- predict(fit, newdata = x, 
                   return_tmb_object = TRUE)
  
  index <- get_index(p_sbf, area = 4, bias_correct = T)
  
  saveRDS(index, paste0("index_",names,".rds"))
  
  return(index)
}

#rbind and bring effort and replicates in as columns from rownames
bind_fn<-function(x){
  y<- do.call(rbind, x)
  y$effort<- substr(row.names(y), start = 5, stop = 9)
  y$replicate<- substr(y$effort, start = 3, stop = 5)
  y$effort<- substr(y$effort, start = 1, stop = 3)
  y$effort <- gsub("\\.([^0-9])", ".", y$effort)
  y$effort<- sub("_.*", "", y$effort)
  y$effort<-as.numeric(y$effort)
  y$replicate<- sub("\\..*", "", y$replicate)
  y$replicate<- sub(".*_", "", y$replicate)
  y$replicate<-as.numeric(y$replicate)
  return(y)
}

#special bind function for fit check dfs
bind_fit_check<-function(x){
  y<- do.call(rbind, x)
  y<-as.data.frame(y)
  y$effort<- substr(row.names(y), start = 5, stop = 9)
  y$replicate<- substr(y$effort, start = 3, stop = 5)
  y$effort<- substr(y$effort, start = 1, stop = 3)
  y$effort <- gsub("\\.([^0-9])", ".", y$effort)
  y$effort<- sub("_.*", "", y$effort)
  y$effort<-as.numeric(y$effort)
  y$replicate<- sub("\\..*", "", y$replicate)
  y$replicate<- sub(".*_", "", y$replicate)
  y$replicate<-as.numeric(y$replicate)
  y<-apply(y,2,as.character)
  return(y)
}

bind_index_fn<-function(x){
  y<- do.call(rbind, x)
  y$effort<- substr(row.names(y), start = 7, stop = 11)
  y$replicate<- substr(y$effort, start = 3, stop = 5)
  y$effort<- substr(y$effort, start = 1, stop = 3)
  y$effort <- gsub("\\.([^0-9])", ".", y$effort)
  y$effort<- sub("_.*", "", y$effort)
  y$effort<-as.numeric(y$effort)
  y$replicate<- sub("\\..*", "", y$replicate)
  y$replicate<- sub(".*_", "", y$replicate)
  y$replicate<-as.numeric(y$replicate)
  return(y)
}

#read in files that contain specified character strings. To bring in fit and index files. 
pull_files <- function(directory, string) {
  # List all files in the directory
  files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)
  
  # Filter files that contain the search string
  filtered_files <- files[grepl(string, files)]
  
  # Read all filtered files into a list of data frames
  data_list <- data_list <- setNames(lapply(filtered_files, readRDS), basename(filtered_files))
  
  return(data_list)
}

####specific functions for filtering by latitude and depth
#remove south of 33.5 lat
lat_filter_335<-function(x){
  x[x$Latitude_dd> 33.5,]
}

#remove south of 34 lat
lat_filter_34<-function(x){
  x[x$Latitude_dd> 34,]
}

#remove north of 34 lat
lat_filter_34_max<-function(x){
  x[x$Latitude_dd< 34,]
}

lat_filter_35<-function(x){
  x[x$Latitude_dd> 35,]
}

#remove deeper than 275 m
depth_filter_275<-function(x){
  x[x$Depth_m< 275,]
}

#remove deeper than 425 m
depth_filter_425<-function(x){
  x[x$Depth_m< 425,]
}

#remove deeper than 450 m
depth_filter_450<-function(x){
  x[x$Depth_m< 450,]
}

#remove deeper than 500 m
depth_filter_500<-function(x){
  x[x$Depth_m< 500,]
}

#remove deeper than 675 m
depth_filter_675<-function(x){
  x[x$Depth_m< 675,]
}

#remove deeper than 700 m 
depth_filter_700<-function(x){
  x[x$Depth_m< 700,]
}

#### Arrowtooth flounder ##########################################################################################################
#Define SDM function
arrowtooth_sdm_fn<-function(x,y){
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

#Fit arrowtooth SDMs
fit_arrowtooth_sdms <- function(tows_assigned, catch) { 
  # Print diagnostic message
  print("Starting function execution")
  
  ### Prepare data -- general processing
  alldata_resampled <- join_dfs(tows_assigned, catch, "Trawl_id")
  print("Data joined successfully")
  
  adr_split <- lapply(alldata_resampled, split_spp)
  print("Data split by species successfully")
  
  adr_split <- unlist(adr_split, recursive = FALSE)
  print("Data unlisted successfully")
  
  names(adr_split) <- substr(names(adr_split), 6, 50)
  print("Renamed dataframes successfully")
  
  ### Species-specific processing
  arrowtooth_all_yrs <- adr_split[grep("arrowtooth flounder", names(adr_split))]
  print("Arrowtooth flounder data extracted")
  
  arrowtooth_all_yrs <- arrowtooth_all_yrs |> bind_rows(.id = "source")
  print("Arrowtooth data bound into a single dataframe")
  
  arrowtooth_all_yrs <- split(arrowtooth_all_yrs, arrowtooth_all_yrs$source)
  print("Data split by source")
  
  arrowtooth_names <- grep("arrowtooth flounder", names(arrowtooth_all_yrs), value = TRUE)
  print("Arrowtooth names extracted")
  
  arrowtooth_dfs <- arrowtooth_all_yrs[names(arrowtooth_all_yrs) %in% arrowtooth_names]
  arrowtooth_dfs <- lapply(arrowtooth_dfs, lat_filter_34)
  print("Latitude filter applied")
  
  # Make the names file
  arrowtooth_files <- as.list(arrowtooth_names)
  
  # Reduce the number of DFs for testing
  if (length(arrowtooth_dfs) < 91) {
    stop("Insufficient data frames for the specified range. Check the indices.")
  }
  
  #arrowtooth_dfs <- arrowtooth_dfs[87:91]
  #arrowtooth_files <- arrowtooth_files[87:91]
  #print("Reduced number of data frames for testing")
  
  # Fit arrowtooth SDMs in parallel
  print("Starting parallel SDM processing")
  
  arrowtooth_sdms <- foreach(
    i = seq_along(arrowtooth_dfs), 
    .combine = 'list', 
    .packages = c('foreach', 'doParallel', 'sdmTMB'), 
    .errorhandling = "remove",
    .export = c("arrowtooth_sdm_fn", "lat_filter_34")
  ) %dopar% {
    print(paste("Processing SDM:", i))
    result <- arrowtooth_sdm_fn(arrowtooth_dfs[[i]], arrowtooth_files[[i]])
    print(paste("Result for SDM", i, ":", result))
    result
  }
  
  print("Parallel SDM processing complete")
  print(arrowtooth_sdms)
  
  #calculate indices
  print("Starting parallel index calculation")
  
  arrowtooth_indices<-foreach(
    i = seq_along(arrowtooth_dfs),
    .combine = 'list',
    .packages = c('foreach','doParallel','sdmTMB'),
    .errorhandling = "remove") %dopar% {
    print(paste("Processing index:", i))
    result<- index_fn(arrowtooth_sdms[[i]],arrowtooth_dfs[[i]], arrowtooth_files[[i]])
    print(paste("Result for index", i, ":", result))
    result
  }  
  print("Parallel index calculation complete")
  print(arrowtooth_indices)
  return(arrowtooth_sdms)
  return(arrowth_indices)
}

#fit arrowtooth sdms in parallel
ncores <- parallelly::availableCores(omit = 1)
future::plan(multisession, workers = ncores)
# cores=detectCores()
# cl <- makeCluster(cores[1])#-1) #to not overload your computer
# registerDoParallel(cl)

setwd(arrowtooth)

fit_arrowtooth_sdms(tows_assigned_resampled,catch)

future::plan(sequential)
# stopCluster(cl)

#####read in fit and index files
arrowtooth_sdms<-pull_files(arrowtooth,"fit")
arrowtooth_indices<-pull_files(arrowtooth,"index")

#extract outputs
arrowtooth_fits<-lapply(arrowtooth_sdms, fit_df_fn)
arrowtooth_fit_df<- bind_fn(arrowtooth_fits)

arrowtooth_pars<- lapply(arrowtooth_sdms, fit_pars_fn)
arrowtooth_pars_df<- bind_fn(arrowtooth_pars)

arrowtooth_fit_check<- lapply(arrowtooth_sdms, fit_check_fn)
arrowtooth_fit_check_df<- bind_fit_check(arrowtooth_fit_check)

arrowtooth_indices_df<- bind_index_fn(arrowtooth_indices)

setwd(arrowtooth)
write.csv(arrowtooth_fit_df, "arrowtooth_fit_df.csv",row.names = F)
write.csv(arrowtooth_pars_df, "arrowtooth_pars_df.csv",row.names = F)
write.csv(arrowtooth_fit_check_df, "arrowtooth_fit_check_df.csv",row.names = F)
write.csv(arrowtooth_indices_df, "arrowtooth_indices_df.csv",row.names = F)

# #### Bocaccio #############################################################################################################
# bocaccio_names<- grep("bocaccio", names(species_all_yrs),value = T)
# bocaccio_dfs<-species_all_yrs[names(species_all_yrs)%in% bocaccio_names]

# bocaccio_dfs<- lapply(bocaccio_dfs,depth_filter_500)

# #make the names file
# bocaccio_files<-as.list(bocaccio_names)

# #fit SDMs
# bocaccio_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#     )
  
#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit) 
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(bocaccio)

# #with predefined function
# bocaccio_sdms<-foreach(i = seq_along(bocaccio_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   bocaccio_sdm_fn(bocaccio_dfs[[i]],bocaccio_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# bocaccio_sdms<-pull_files(bocaccio,"fit")

# #extract outputs
# bocaccio_fits<-lapply(bocaccio_sdms, fit_df_fn)
# bocaccio_fit_df<- bind_fn(bocaccio_fits)

# bocaccio_pars<- lapply(bocaccio_sdms, fit_pars_fn)
# bocaccio_pars_df<- bind_fn(bocaccio_pars)

# bocaccio_fit_check<- lapply(bocaccio_sdms, fit_check_fn)
# bocaccio_fit_check_df<- bind_fit_check(bocaccio_fit_check)

# #bocaccio_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(bocaccio)

# #with predefined function
# bocaccio_indices<-foreach(i = seq_along(bocaccio_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(bocaccio_sdms[[i]],bocaccio_dfs[[i]], bocaccio_files[[i]])
# }  

# stopCluster(cl)

# #####read in index files
# bocaccio_indices<-pull_files(bocaccio,"index")

# bocaccio_indices_df<- bind_index_fn(bocaccio_indices)

# setwd(bocaccio)
# write.csv(bocaccio_fit_df, "bocaccio_fit_df.csv",row.names = F)
# write.csv(bocaccio_pars_df, "bocaccio_pars_df.csv",row.names = F)
# write.csv(bocaccio_fit_check_df, "bocaccio_fit_check_df.csv",row.names = F)
# write.csv(bocaccio_indices_df, "bocaccio_indices_df.csv",row.names = F)

# #### Canary rockfish ##########################################################################################################
# canary_names<- grep("canary rockfish", names(species_all_yrs),value = T)
# canary_dfs<-species_all_yrs[names(species_all_yrs)%in% canary_names]

# canary_dfs<- lapply(canary_dfs,depth_filter_275)

# #make the names file
# canary_files<-as.list(canary_names)

# #fit SDMs
# canary_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = FALSE,
#     spatiotemporal = as.list(c("iid","off"))
#   )
  
#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
  
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(canary)

# #with predefined function
# canary_sdms<-foreach(i = seq_along(canary_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   canary_sdm_fn(canary_dfs[[i]],canary_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# canary_sdms<-pull_files(canary,"fit")

# #extract outputs
# canary_fits<-lapply(canary_sdms, fit_df_fn)
# canary_fit_df<- bind_fn(canary_fits)

# canary_pars<- lapply(canary_sdms, fit_pars_fn)
# canary_pars_df<- bind_fn(canary_pars)

# canary_fit_check<- lapply(canary_sdms, fit_check_fn)
# canary_fit_check_df<- bind_fit_check(canary_fit_check)

# #canary_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(canary)

# #with predefined function
# canary_indices<-foreach(i = seq_along(canary_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(canary_sdms[[i]],canary_dfs[[i]], canary_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# canary_indices<-pull_files(canary,"index")

# canary_indices_df<- bind_index_fn(canary_indices)

# setwd(canary)
# write.csv(canary_fit_df, "canary_fit_df.csv",row.names = F)
# write.csv(canary_pars_df, "canary_pars_df.csv",row.names = F)
# write.csv(canary_fit_check_df, "canary_fit_check_df.csv",row.names = F)
# write.csv(canary_indices_df, "canary_indices_df.csv",row.names = F)

# #### Darkblotched rockfish ####################################################################################################
# darkblotched_names<- grep("darkblotched rockfish", names(species_all_yrs),value = T)
# darkblotched_dfs<-species_all_yrs[names(species_all_yrs)%in% darkblotched_names]

# darkblotched_dfs<- lapply(darkblotched_dfs,lat_filter_335)

# darkblotched_dfs<- lapply(darkblotched_dfs,depth_filter_675)

# #make the names file
# darkblotched_files<-as.list(darkblotched_names)

# #fit SDMs
# darkblotched_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 250)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("off","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
  
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(darkblotched)

# #with predefined function
# darkblotched_sdms<-foreach(i = seq_along(darkblotched_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   darkblotched_sdm_fn(darkblotched_dfs[[i]],darkblotched_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# darkblotched_sdms<-pull_files(darkblotched,"fit")

# #extract outputs
# darkblotched_fits<-lapply(darkblotched_sdms, fit_df_fn)
# darkblotched_fit_df<- bind_fn(darkblotched_fits)

# darkblotched_pars<- lapply(darkblotched_sdms, fit_pars_fn)
# darkblotched_pars_df<- bind_fn(darkblotched_pars)

# darkblotched_fit_check<- lapply(darkblotched_sdms, fit_check_fn)
# darkblotched_fit_check_df<- bind_fit_check(darkblotched_fit_check)

# #darkblotched_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(darkblotched)

# #with predefined function
# darkblotched_indices<-foreach(i = seq_along(darkblotched_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(darkblotched_sdms[[i]],darkblotched_dfs[[i]], darkblotched_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# darkblotched_indices<-pull_files(darkblotched,"index")
# darkblotched_indices_df<- bind_index_fn(darkblotched_indices)

# setwd(darkblotched)
# write.csv(darkblotched_fit_df, "darkblotched_fit_df.csv",row.names = F)
# write.csv(darkblotched_pars_df, "darkblotched_pars_df.csv",row.names = F)
# write.csv(darkblotched_fit_check_df, "darkblotched_fit_check_df.csv",row.names = F)
# write.csv(darkblotched_indices_df, "darkblotched_indices_df.csv",row.names = F)

# #### Dover sole ###############################################################################################################
# dover_names<- grep("Dover sole", names(species_all_yrs),value = T)
# dover_dfs<-species_all_yrs[names(species_all_yrs)%in% dover_names]

# #make the names file
# dover_files<-as.list(dover_names)

# #fit SDMs
# dover_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(dover)

# #with predefined function
# dover_sdms<-foreach(i = seq_along(dover_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   dover_sdm_fn(dover_dfs[[i]],dover_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# dover_sdms<-pull_files(dover,"fit")

# #extract outputs
# dover_fits<-lapply(dover_sdms, fit_df_fn)
# dover_fit_df<- bind_fn(dover_fits)

# dover_pars<- lapply(dover_sdms, fit_pars_fn)
# dover_pars_df<- bind_fn(dover_pars)

# dover_fit_check<- lapply(dover_sdms, fit_check_fn)
# dover_fit_check_df<- bind_fit_check(dover_fit_check)

# #dover_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(dover)

# #with predefined function
# dover_indices<-foreach(i = seq_along(dover_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(dover_sdms[[i]],dover_dfs[[i]], dover_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# dover_indices<-pull_files(dover,"index")
# dover_indices_df<- bind_index_fn(dover_indices)

# setwd(dover)
# write.csv(dover_fit_df, "dover_fit_df.csv",row.names = F)
# write.csv(dover_pars_df, "dover_pars_df.csv",row.names = F)
# write.csv(dover_fit_check_df, "dover_fit_check_df.csv",row.names = F)
# write.csv(dover_indices_df, "dover_indices_df.csv",row.names = F)

# #### Lingcod North ############################################################################################################
# lingcod_n_names<- grep("lingcod", names(species_all_yrs),value = T)
# lingcod_n_dfs<-species_all_yrs[names(species_all_yrs)%in% lingcod_n_names]

# lingcod_n_dfs<- lapply(lingcod_n_dfs,lat_filter_34)

# lingcod_n_dfs<- lapply(lingcod_n_dfs,depth_filter_450)

# #make the names file; edit to include n
# lingcod_n_names<-gsub('lingcod','lingcod_n', lingcod_n_names)

# lingcod_n_files<-as.list(lingcod_n_names)

# #fit SDMs
# lingcod_n_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500) #or split based on area?
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )
  
#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(lingcod_n)

# #with predefined function
# lingcod_n_sdms<-foreach(i = seq_along(lingcod_n_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   lingcod_n_sdm_fn(lingcod_n_dfs[[i]],lingcod_n_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# lingcod_n_sdms<-pull_files(lingcod_n,"fit")

# #extract outputs
# lingcod_n_fits<-lapply(lingcod_n_sdms, fit_df_fn)
# lingcod_n_fit_df<- bind_fn(lingcod_n_fits)

# lingcod_n_pars<- lapply(lingcod_n_sdms, fit_pars_fn)
# lingcod_n_pars_df<- bind_fn(lingcod_n_pars)

# lingcod_n_fit_check<- lapply(lingcod_n_sdms, fit_check_fn)
# lingcod_n_fit_check_df<- bind_fit_check(lingcod_n_fit_check)

# #lingcod_n_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(lingcod_n)

# #with predefined function
# lingcod_n_indices<-foreach(i = seq_along(lingcod_n_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(lingcod_n_sdms[[i]],lingcod_n_dfs[[i]], lingcod_n_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# lingcod_n_indices<-pull_files(lingcod_n,"index")
# lingcod_n_indices_df<- bind_index_fn(lingcod_n_indices)

# setwd(lingcod_n)
# write.csv(lingcod_n_fit_df, "lingcod_n_fit_df.csv",row.names = F)
# write.csv(lingcod_n_pars_df, "lingcod_n_pars_df.csv",row.names = F)
# write.csv(lingcod_n_fit_check_df, "lingcod_n_fit_check_df.csv",row.names = F)
# write.csv(lingcod_n_indices_df, "lingcod_n_indices_df.csv",row.names = F)

# #### Lingcod South ###############################################################################################
# lingcod_s_names<- grep("lingcod", names(species_all_yrs),value = T)
# lingcod_s_dfs<-species_all_yrs[names(species_all_yrs)%in% lingcod_s_names]

# lingcod_s_dfs<- lapply(lingcod_s_dfs,lat_filter_34_max)

# lingcod_s_dfs<- lapply(lingcod_s_dfs,depth_filter_450)

# #make the names file; edit to include s
# lingcod_s_names<-gsub('lingcod','lingcod_s', lingcod_s_names)

# lingcod_s_files<-as.list(lingcod_s_names)

# #fit SDMs
# lingcod_s_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500) #or split based on area?
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(lingcod_s)

# #with predefined function
# lingcod_s_sdms<-foreach(i = seq_along(lingcod_s_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   lingcod_s_sdm_fn(lingcod_s_dfs[[i]],lingcod_s_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# lingcod_s_sdms<-pull_files(lingcod_s,"fit")

# #extract outputs
# lingcod_s_fits<-lapply(lingcod_s_sdms, fit_df_fn)
# lingcod_s_fit_df<- bind_fn(lingcod_s_fits)

# lingcod_s_pars<- lapply(lingcod_s_sdms, fit_pars_fn)
# lingcod_s_pars_df<- bind_fn(lingcod_s_pars)

# lingcod_s_fit_check<- lapply(lingcod_s_sdms, fit_check_fn)
# lingcod_s_fit_check_df<- bind_fit_check(lingcod_s_fit_check)

# #lingcod_s_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(lingcod_s)

# #with predefined function
# lingcod_s_indices<-foreach(i = seq_along(lingcod_s_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(lingcod_s_sdms[[i]],lingcod_s_dfs[[i]], lingcod_s_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# lingcod_s_indices<-pull_files(lingcod_s,"index")
# lingcod_s_indices_df<- bind_index_fn(lingcod_s_indices)

# setwd(lingcod_s)
# write.csv(lingcod_s_fit_df, "lingcod_s_fit_df.csv",row.names = F)
# write.csv(lingcod_s_pars_df, "lingcod_s_pars_df.csv",row.names = F)
# write.csv(lingcod_s_fit_check_df, "lingcod_s_fit_check_df.csv",row.names = F)
# write.csv(lingcod_s_indices_df, "lingcod_s_indices_df.csv",row.names = F)

# #### Longnose Skate ###########################################################################################################
# longnose_names<- grep("longnose skate", names(species_all_yrs),value = T)
# longnose_dfs<-species_all_yrs[names(species_all_yrs)%in% longnose_names]

# #make the names file
# longnose_files<-as.list(longnose_names)

# #fit SDMs
# longnose_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(longnose)

# #with predefined function
# longnose_sdms<-foreach(i = seq_along(longnose_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   longnose_sdm_fn(longnose_dfs[[i]],longnose_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# longnose_sdms<-pull_files(longnose,"fit")

# #extract outputs
# longnose_fits<-lapply(longnose_sdms, fit_df_fn)
# longnose_fit_df<- bind_fn(longnose_fits)

# longnose_pars<- lapply(longnose_sdms, fit_pars_fn)
# longnose_pars_df<- bind_fn(longnose_pars)

# longnose_fit_check<- lapply(longnose_sdms, fit_check_fn)
# longnose_fit_check_df<- bind_fit_check(longnose_fit_check)

# #longnose_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(longnose)

# #with predefined function
# longnose_indices<-foreach(i = seq_along(longnose_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(longnose_sdms[[i]],longnose_dfs[[i]], longnose_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# longnose_indices<-pull_files(longnose,"index")
# longnose_indices_df<- bind_index_fn(longnose_indices)

# setwd(longnose)
# write.csv(longnose_fit_df, "longnose_fit_df.csv",row.names = F)
# write.csv(longnose_pars_df, "longnose_pars_df.csv",row.names = F)
# write.csv(longnose_fit_check_df, "longnose_fit_check_df.csv",row.names = F)
# write.csv(longnose_indices_df, "longnose_indices_df.csv",row.names = F)

# #### Pacific ocean perch ######################################################################################################
# pop_names<- grep("Pacific ocean perch", names(species_all_yrs),value = T)
# pop_dfs<-species_all_yrs[names(species_all_yrs)%in% pop_names]

# pop_dfs<- lapply(pop_dfs,lat_filter_35)

# pop_dfs<- lapply(pop_dfs,depth_filter_500)

# #make the names file
# pop_files<-as.list(pop_names)

# #fit SDMs
# pop_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(pop)

# #with predefined function
# pop_sdms<-foreach(i = seq_along(pop_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   pop_sdm_fn(pop_dfs[[i]],pop_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# pop_sdms<-pull_files(pop,"fit")

# #extract outputs
# pop_fits<-lapply(pop_sdms, fit_df_fn)
# pop_fit_df<- bind_fn(pop_fits)

# pop_pars<- lapply(pop_sdms, fit_pars_fn)
# pop_pars_df<- bind_fn(pop_pars)

# pop_fit_check<- lapply(pop_sdms, fit_check_fn)
# pop_fit_check_df<- bind_fit_check(pop_fit_check)

# #pop_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(pop)

# #with predefined function
# pop_indices<-foreach(i = seq_along(pop_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(pop_sdms[[i]],pop_dfs[[i]], pop_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# pop_indices<-pull_files(pop,"index")
# pop_indices_df<- bind_index_fn(pop_indices)

# setwd(pop)
# write.csv(pop_fit_df, "pop_fit_df.csv",row.names = F)
# write.csv(pop_pars_df, "pop_pars_df.csv",row.names = F)
# write.csv(pop_fit_check_df, "pop_fit_check_df.csv",row.names = F)
# write.csv(pop_indices_df, "pop_indices_df.csv",row.names = F)

# #### Pacific spiny dogfish ########################################################################################################
# dogfish_names<- grep("Pacific spiny dogfish", names(species_all_yrs),value = T)
# dogfish_dfs<-species_all_yrs[names(species_all_yrs)%in% dogfish_names]

# dogfish_dfs<- lapply(dogfish_dfs,depth_filter_700)

# #make the names file
# dogfish_files<-as.list(dogfish_names)

# #fit SDMs
# dogfish_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(dogfish)

# #with predefined function
# dogfish_sdms<-foreach(i = seq_along(dogfish_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   dogfish_sdm_fn(dogfish_dfs[[i]],dogfish_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# dogfish_sdms<-pull_files(dogfish,"fit")

# #extract outputs
# dogfish_fits<-lapply(dogfish_sdms, fit_df_fn)
# dogfish_fit_df<- bind_fn(dogfish_fits)

# dogfish_pars<- lapply(dogfish_sdms, fit_pars_fn)
# dogfish_pars_df<- bind_fn(dogfish_pars)

# dogfish_fit_check<- lapply(dogfish_sdms, fit_check_fn)
# dogfish_fit_check_df<- bind_fit_check(dogfish_fit_check)

# #dogfish_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(output)

# #with predefined function
# dogfish_indices<-foreach(i = seq_along(dogfish_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(dogfish_sdms[[i]],dogfish_dfs[[i]], dogfish_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# dogfish_indices<-pull_files(dogfish,"index")
# dogfish_indices_df<- bind_index_fn(dogfish_indices)

# setwd(output)
# write.csv(dogfish_fit_df, "dogfish_fit_df.csv",row.names = F)
# write.csv(dogfish_pars_df, "dogfish_pars_df.csv",row.names = F)
# write.csv(dogfish_fit_check_df, "dogfish_fit_check_df.csv",row.names = F)
# write.csv(dogfish_indices_df, "dogfish_indices_df.csv",row.names = F)

# #### Petrale sole #############################################################################################################
# petrale_names<- grep("petrale sole", names(species_all_yrs),value = T)
# petrale_dfs<-species_all_yrs[names(species_all_yrs)%in% petrale_names]

# petrale_dfs<- lapply(petrale_dfs,depth_filter_675)

# #make the names file
# petrale_files<-as.list(petrale_names)

# #fit SDMs
# petrale_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(petrale)

# #with predefined function
# petrale_sdms<-foreach(i = seq_along(petrale_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   petrale_sdm_fn(petrale_dfs[[i]],petrale_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# petrale_sdms<-pull_files(petrale,"fit")

# #extract outputs
# petrale_fits<-lapply(petrale_sdms, fit_df_fn)
# petrale_fit_df<- bind_fn(petrale_fits)

# petrale_pars<- lapply(petrale_sdms, fit_pars_fn)
# petrale_pars_df<- bind_fn(petrale_pars)

# petrale_fit_check<- lapply(petrale_sdms, fit_check_fn)
# petrale_fit_check_df<- bind_fit_check(petrale_fit_check)

# #petrale_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(petrale)

# #with predefined function
# petrale_indices<-foreach(i = seq_along(petrale_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(petrale_sdms[[i]],petrale_dfs[[i]], petrale_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# petrale_indices<-pull_files(petrale,"index")
# petrale_indices_df<- bind_index_fn(petrale_indices)

# setwd(petrale)
# write.csv(petrale_fit_df, "petrale_fit_df.csv",row.names = F)
# write.csv(petrale_pars_df, "petrale_pars_df.csv",row.names = F)
# write.csv(petrale_fit_check_df, "petrale_fit_check_df.csv",row.names = F)
# write.csv(petrale_indices_df, "petrale_indices_df.csv",row.names = F)

# #### Rex sole #################################################################################################################
# rex_names<- grep("rex sole", names(species_all_yrs),value = T)
# rex_dfs<-species_all_yrs[names(species_all_yrs)%in% rex_names]

# rex_dfs<- lapply(rex_dfs,depth_filter_700)

# #make the names file
# rex_files<-as.list(rex_names)

# #fit SDMs
# rex_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(rex)

# #with predefined function
# rex_sdms<-foreach(i = seq_along(rex_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   rex_sdm_fn(rex_dfs[[i]],rex_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# rex_sdms<-pull_files(rex,"fit")

# #extract outputs
# rex_fits<-lapply(rex_sdms, fit_df_fn)
# rex_fit_df<- bind_fn(rex_fits)

# rex_pars<- lapply(rex_sdms, fit_pars_fn)
# rex_pars_df<- bind_fn(rex_pars)

# rex_fit_check<- lapply(rex_sdms, fit_check_fn)
# rex_fit_check_df<- bind_fit_check(rex_fit_check)

# #rex_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(rex)

# #with predefined function
# rex_indices<-foreach(i = seq_along(rex_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(rex_sdms[[i]],rex_dfs[[i]], rex_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# rex_indices<-pull_files(rex,"index")
# rex_indices_df<- bind_index_fn(rex_indices)

# setwd(rex)
# write.csv(rex_fit_df, "rex_fit_df.csv",row.names = F)
# write.csv(rex_pars_df, "rex_pars_df.csv",row.names = F)
# write.csv(rex_fit_check_df, "rex_fit_check_df.csv",row.names = F)
# write.csv(rex_indices_df, "rex_indices_df.csv",row.names = F)

# #### Sablefish ##################################################################################################################
# sablefish_names<- grep("sablefish", names(species_all_yrs),value = T)
# sablefish_dfs<-species_all_yrs[names(species_all_yrs)%in% sablefish_names]

# #make the names file
# sablefish_files<-as.list(sablefish_names)

# #fit SDMs
# sablefish_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#     )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(sablefish)

# #with predefined function
# sablefish_sdms<-foreach(i = seq_along(sablefish_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   sablefish_sdm_fn(sablefish_dfs[[i]],sablefish_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# sablefish_sdms<-pull_files(sablefish,"fit")

# #extract outputs
# sablefish_fits<-lapply(sablefish_sdms, fit_df_fn)
# sablefish_fit_df<- bind_fn(sablefish_fits)

# sablefish_pars<- lapply(sablefish_sdms, fit_pars_fn)
# sablefish_pars_df<- bind_fn(sablefish_pars)

# sablefish_fit_check<- lapply(sablefish_sdms, fit_check_fn)
# sablefish_fit_check_df<- bind_fit_check(sablefish_fit_check)

# #sablefish_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(sablefish)

# #with predefined function
# sablefish_indices<-foreach(i = seq_along(sablefish_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(sablefish_sdms[[i]],sablefish_dfs[[i]], sablefish_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# sablefish_indices<-pull_files(sablefish,"index")
# sablefish_indices_df<- bind_index_fn(sablefish_indices)

# setwd(sablefish)
# write.csv(sablefish_fit_df, "sablefish_fit_df.csv",row.names = F)
# write.csv(sablefish_pars_df, "sablefish_pars_df.csv",row.names = F)
# write.csv(sablefish_fit_check_df, "sablefish_fit_check_df.csv",row.names = F)
# write.csv(sablefish_indices_df, "sablefish_indices_df.csv",row.names = F)

# #### Shortspine thornyhead ####################################################################################################
# shortspine_names<- grep("shortspine thornyhead", names(species_all_yrs),value = T)
# shortspine_dfs<-species_all_yrs[names(species_all_yrs)%in% shortspine_names]

# #make the names file
# shortspine_files<-as.list(shortspine_names)

# #fit SDMs
# shortspine_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass + Depth_m + (Depth_m^2), #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_lognormal(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(shortspine)

# #with predefined function
# shortspine_sdms<-foreach(i = seq_along(shortspine_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   shortspine_sdm_fn(shortspine_dfs[[i]],shortspine_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# shortspine_sdms<-pull_files(shortspine,"fit")

# #extract outputs
# shortspine_fits<-lapply(shortspine_sdms, fit_df_fn)
# shortspine_fit_df<- bind_fn(shortspine_fits)

# shortspine_pars<- lapply(shortspine_sdms, fit_pars_fn)
# shortspine_pars_df<- bind_fn(shortspine_pars)

# shortspine_fit_check<- lapply(shortspine_sdms, fit_check_fn)
# shortspine_fit_check_df<- bind_fit_check(shortspine_fit_check)

# #shortspine_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(shortspine)

# #with predefined function
# shortspine_indices<-foreach(i = seq_along(shortspine_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(shortspine_sdms[[i]],shortspine_dfs[[i]], shortspine_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# shortspine_indices<-pull_files(shortspine,"index")
# shortspine_indices_df<- bind_index_fn(shortspine_indices)

# setwd(shortspine)
# write.csv(shortspine_fit_df, "shortspine_fit_df.csv",row.names = F)
# write.csv(shortspine_pars_df, "shortspine_pars_df.csv",row.names = F)
# write.csv(shortspine_fit_check_df, "shortspine_fit_check_df.csv",row.names = F)
# write.csv(shortspine_indices_df, "shortspine_indices_df.csv",row.names = F)

# #### Widow rockfish ###########################################################################################################
# widow_names<- grep("widow rockfish", names(species_all_yrs),value = T)
# widow_dfs<-species_all_yrs[names(species_all_yrs)%in% widow_names]

# widow_dfs<- lapply(widow_dfs,lat_filter_335)

# widow_dfs<- lapply(widow_dfs,depth_filter_675)

# #make the names file
# widow_files<-as.list(widow_names)

# #fit SDMs
# widow_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 200)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("off","off"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(widow)

# #with predefined function
# widow_sdms<-foreach(i = seq_along(widow_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   widow_sdm_fn(widow_dfs[[i]],widow_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# widow_sdms<-pull_files(widow,"fit")

# #extract outputs
# widow_fits<-lapply(widow_sdms, fit_df_fn)
# widow_fit_df<- bind_fn(widow_fits)

# widow_pars<- lapply(widow_sdms, fit_pars_fn)
# widow_pars_df<- bind_fn(widow_pars)

# widow_fit_check<- lapply(widow_sdms, fit_check_fn)
# widow_fit_check_df<- bind_fit_check(widow_fit_check)

# #widow_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(widow)

# #with predefined function
# widow_indices<-foreach(i = seq_along(widow_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(widow_sdms[[i]],widow_dfs[[i]], widow_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# widow_indices<-pull_files(widow,"index")
# widow_indices_df<- bind_index_fn(widow_indices)

# setwd(widow)
# write.csv(widow_fit_df, "widow_fit_df.csv",row.names = F)
# write.csv(widow_pars_df, "widow_pars_df.csv",row.names = F)
# write.csv(widow_fit_check_df, "widow_fit_check_df.csv",row.names = F)
# write.csv(widow_indices_df, "widow_indices_df.csv",row.names = F)

# #### Yellowtail rockfish ######################################################################################################
# yellowtail_names<- grep("yellowtail rockfish", names(species_all_yrs),value = T)
# yellowtail_dfs<-species_all_yrs[names(species_all_yrs)%in% yellowtail_names]

# yellowtail_dfs<- lapply(yellowtail_dfs,lat_filter_335)

# yellowtail_dfs<- lapply(yellowtail_dfs,depth_filter_425)

# #make the names file
# yellowtail_files<-as.list(yellowtail_names)

# #fit SDMs
# yellowtail_sdm_fn<- function(x,y){ 
#   #make mesh
#   mesh<- make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
#   #fit model
#   fit<- sdmTMB(
#     total_catch_wt_kg ~ 0 + factor(Year) + Pass, #fyear and pass_scaled were specified in the configs doc.
#     data = x,
#     mesh = mesh,
#     family = delta_gamma(),
#     time = "Year",
#     anisotropy = TRUE,
#     spatiotemporal = as.list(c("iid","iid"))
#   )

#   #save file
#   saveRDS(fit, paste0("fit_",y,".rds"))
  
#   return(fit)
# }

# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(yellowtail)

# #with predefined function
# yellowtail_sdms<-foreach(i = seq_along(yellowtail_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   yellowtail_sdm_fn(yellowtail_dfs[[i]],yellowtail_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# yellowtail_sdms<-pull_files(yellowtail,"fit")

# #extract outputs
# yellowtail_fits<-lapply(yellowtail_sdms, fit_df_fn)
# yellowtail_fit_df<- bind_fn(yellowtail_fits)

# yellowtail_pars<- lapply(yellowtail_sdms, fit_pars_fn)
# yellowtail_pars_df<- bind_fn(yellowtail_pars)

# yellowtail_fit_check<- lapply(yellowtail_sdms, fit_check_fn)
# yellowtail_fit_check_df<- bind_fit_check(yellowtail_fit_check)

# #yellowtail_indices requires parallel processing for efficiency
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #to not overload your computer
# registerDoParallel(cl)

# setwd(yellowtail)

# #with predefined function
# yellowtail_indices<-foreach(i = seq_along(yellowtail_dfs), .combine = 'list',.packages = c('foreach','doParallel','sdmTMB'), .errorhandling = "remove") %dopar% {
#   index_fn(yellowtail_sdms[[i]],yellowtail_dfs[[i]], yellowtail_files[[i]])
# }  

# stopCluster(cl)

# #####read in .rds if already fit
# yellowtail_indices<-pull_files(yellowtail,"index")
# yellowtail_indices_df<- bind_index_fn(yellowtail_indices)

# setwd(yellowtail)
# write.csv(yellowtail_fit_df, "yellowtail_fit_df.csv",row.names = F)
# write.csv(yellowtail_pars_df, "yellowtail_pars_df.csv",row.names = F)
# write.csv(yellowtail_fit_check_df, "yellowtail_fit_check_df.csv",row.names = F)
# write.csv(yellowtail_indices_df, "yellowtail_indices_df.csv",row.names = F)

# #### calculate metrics ############################################################################################################

# #get the index dfs
# string <- grep("indices_df", ls(), value = TRUE)
# all_indices <- lapply(string, get)

# #calculate CV based on CI width
# CV_fn<- function(x){
#   x$CV_CI<- (x$upr - x$lwr/x$est)
#   return(x)
# }

# all_indices<- lapply(all_indices,CV_fn)

# #make one big DF for plotting
# index_df<- do.call(rbind,all_indices)

# #make a species column
# index_df$species<- rownames(index_df)
# index_df$species <- gsub("[^[:alpha:] ]", "", index_df$species)

# #calculate CV
# index_df$CV<-(100*index_df$se)/index_df$log_est

# #####calculate relative error for metrics
# #get reference vales for the calculation
# #reference se
# index_df<-index_df |>
#   group_by(Year,species) |>
#   mutate(reference_se = se[effort == "1"][1]) |>
#   ungroup()

# #reference biomass
# index_df<-index_df |>
#   group_by(Year,species) |>
#   mutate(reference_biomass = est[effort == "1"][1]) |>
#   ungroup()

# #relative error of SE
# index_df<-index_df |>
#   group_by(Year,species) |>
#   mutate(se_relative_error = ((se-reference_se)/reference_se)*100)
  
# #relative error of biomass
# index_df<-index_df |>
#   group_by(Year,species) |>
#   mutate(biomass_relative_error = ((est-reference_biomass)/reference_biomass)*100)

# #absolute relative error SE
# index_df$absolute_relative_error_se<-abs(index_df$se_relative_error)

# #absolute relative error biomass
# index_df$absolute_relative_error_biomass<-abs(index_df$biomass_relative_error)

# #write CSV
# setwd(output)
# write.csv(index_df,"all_NWFSC_BT_focal_FMP_spp_sdmTMB_indices.csv", row.names = F)

# ##### make figures ##############################################################################################################
# #fix funky widow instance with negative CV
# #index_df$CV[index_df$CV<0]<-NA

# #make year a factor
# index_df$Year<-factor(index_df$Year)

# #plot index SE
# ggplot(data=index_df, aes(x=effort, y=se, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Index Standard Error")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_index_SE.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #index CV
# ggplot(data=index_df, aes(x=effort, y=CV, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Index CV")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_CV.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #biomass ests
# ggplot(data=index_df, aes(x=effort, y=est, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Biomass estimate")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_biomass_estimate.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #relative error of SE
# ggplot(data=index_df, aes(x=effort, y=se_relative_error, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   xlim(0,0.9)+
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Relative error of SE (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_SE_relative_error.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #relative error of biomass
# ggplot(data=index_df, aes(x=effort, y=biomass_relative_error, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   xlim(0,0.9)+
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Relative error of biomass (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_biomass_relative_error.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #absolute relative error of biomass
# ggplot(data=index_df, aes(x=effort, y=absolute_relative_error_biomass, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   xlim(0,0.9)+
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Absolute relative error of biomass (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_biomass_absolute_relative_error.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# ##### edit plotting window ###################################################################################################
# #plot index SE
# ggplot(data=index_df, aes(x=effort, y=se, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   ylim(0,1) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Index Standard Error")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_index_SE_window_edit.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #index CV
# ggplot(data=index_df, aes(x=effort, y=CV, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   ylim(0,15) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Index CV")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_CV_window_edit.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #log biomass ests
# ggplot(data=index_df, aes(x=effort, y=log_est, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Log (biomass estimate)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_log_biomass_estimate.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #relative error of SE
# ggplot(data=index_df, aes(x=effort, y=se_relative_error, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   ylim(0,350) + xlim(0,0.9) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Relative error of SE (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_SE_relative_error_window_edit.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #relative error of biomass
# ggplot(data=index_df, aes(x=effort, y=biomass_relative_error, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   ylim(-100,100) + xlim(0,0.9) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Relative error of biomass (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_biomass_relative_error_window_edit.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

# #absolute relative error of biomass
# ggplot(data=index_df, aes(x=effort, y=absolute_relative_error_biomass, color = Year)) + geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
#   ylim(0,100) + xlim(0,0.9) +
#   theme_classic() + theme(axis.text=element_text(color = "black", size=8),
#                           axis.title = element_text(color="black",size=16)) +
#   labs(x="Proportion of tows kept", y = "Absolute relative error of biomass (%)")

# setwd(figures)
# ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_sdmTMB_biomass_absolute_relative_error_window_edit.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)
