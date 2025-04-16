

#' Cleanup species function
#'
#' Filter catch by species, create a vector of tows and give tows a random
#' assignment to be resampled,
#'
#' @param catch full catch df
#' @param spp_info A data frame containing information about the test species.
#' @param seq_from
#' @param seq_to
#' @param seq_by
#' @param tot_dataframes the number of data frames you want to output. effort x replicates - (replicates - 1). 5x3-2
#' @param replicate_num
#' @return List of resampled catch data frames
#'
cleanup_by_species <- function(catch, 
                               spp_info, 
                               seq_from = 0.1, 
                               seq_to = 1.0, 
                               seq_by = 0.1, 
                               tot_dataframes = 91, 
                               replicate_num = 10) {
  
  df <- catch %>% 
    dplyr::filter(
      common_name == spp_info$common_name)
  
  if (!is.na(spp_info$filter_lat_lt) | is.null(spp_info$filter_lat_lt)) {
    df <- df %>% dplyr::filter(latitude_dd < spp_info$filter_lat_lt)
  }
  if (!is.na(spp_info$filter_lat_gt) | is.null(spp_info$filter_lat_gt)) {
    df <- df %>% dplyr::filter(latitude_dd > spp_info$filter_lat_gt)
  }
  if (!is.na(spp_info$filter_depth) | is.null(spp_info$filter_depth)) {
    df <- df %>% dplyr::filter(depth_m < spp_info$filter_depth)
  }
  
  catch_split <- split(df, df$year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "trawlid"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  tows_assigned <- map2(tows, props, include_or_exclude, replicate_num = replicate_num)
  # tows_assigned <- purrr::pmap(list(.x = tows, .y = props, .z = replicate_num), include_or_exclude)
  
  # remove replicates of the 1 effort level
  tows_assigned <- lapply(tows_assigned, function(x) {
    x <- x[1:tot_dataframes]
    return(x)
  })
  
  tows_assigned_resampled <- purrr::map(tows_assigned, function(x) {
    purrr::map(x, function(y) {
      y[y$RandomAssignment == 1, ]
    })
  })
  
  tows_assigned_resampled <- unlist(tows_assigned_resampled, recursive = F)
  
  alldata_resampled <- join_dfs(tows_assigned_resampled, df, "trawlid")
  
  names(alldata_resampled) <- substr(names(alldata_resampled), 6, 50) # it would be good to replace 50 with a logical indicating the end
  
  species_all_yrs <- alldata_resampled %>%
    dplyr::bind_rows(.id = "source")
  
  species_all_yrs <- split(species_all_yrs, species_all_yrs$source)
  names(species_all_yrs) <- gsub(x = names(species_all_yrs), pattern = ".", replacement = "", fixed = TRUE)
  
  rm("df", "catch_split", "tows", "props", "tows_assigned", "alldata_resampled")
  
  return(species_all_yrs)
}
