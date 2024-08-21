#### Functions ####

#' Tow Function
#' 
#' Create a vector of tows for including or excluding.
#' 
#' @param x catch_split data frame
#' 
tow_fn<-function(x){
  tows<- as.data.frame(x$Trawl_id)
  tows<-unique(tows)
  tows<-as.data.frame(tows[!is.na(tows)])
  names(tows)<-"Trawl_id"
  return(tows)
}


#' Include or Exclude
#' 
#' Specify how to downsample. For simple random sampling, a proportion of stations
#' should do.
#' 
#' @param df tows data frame
#' @param proportions proportions developed using: props <- as.data.frame(seq
#' (0.1,1.0, by = 0.1)) replicated by the length of the tows dataframe. The name
#' of the props is "Trawl_id".
#' 
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



#' Join Data Frames
#' 
#' Function to join a list of data frames to a main data frame using a shared column.
#' 
#' @param list_of_d.fs The list of data frames that you would like to join to the 
#' main data frame
#' @param main_df The main data frame that you want to join the list of data frames
#' @param shared_column The column that all the data frames share which will be 
#' used to join the data frames together.
#' 
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  merged_dfs <- lapply(list_of_dfs, function(df) {
    merged_df <- merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
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