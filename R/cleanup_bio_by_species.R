#' Cleanup bio data species function
#'
#' Filter biological data by species, apply depth and latitude filters, and 
#' filter biological data by the trawl ids that were resampled in the catch data.
#' This requires cleanup_by_species to be run first on the catch data.
#'
#' @param bio_df Dataframe of biological data - length and age compositions that
#' has a Trawl_id column
#' @param catch_cleaned output of cleanup_by_species
#' @param species character vector of the species name to filter out of all the 
#' biological data if more than one species
#' @param filter_lat_lt numeric value for latitude to filter out of the biological data
#' @param filter_lat_gt numeric value for latitude to filter out of the biological data
#' @param filter_depth numeric value for depth to filter out of the biological data
#' @examples
#' \dontrun{
#' cleanup_bio_by_species() # TO DO: NEED EXAMPLE OF HOW TO USE
#' }
#' 
#' @importFrom dplyr filter mutate
#' @export
#' @return List of resampled biological dataframes that match the catch data
#'
cleanup_bio_by_species <- function(bio_df,
                                   catch_cleaned,
                                   filter_lat_lt,
                                   filter_lat_gt,
                                   filter_depth,
                                   species) {
  
  # Apply species filter
  bio_df <- bio_df |>
    dplyr::filter(common_name == species) 
  
  # Apply depth and latitude filters
  if (!is.na(filter_lat_lt) | is.null(filter_lat_lt)) {
    bio_f <- bio_df |> dplyr::filter(latitude_dd < filter_lat_lt)
  }
  if (!is.na(filter_lat_gt) | is.null(filter_lat_gt)) {
    bio_df <- bio_df |> dplyr::filter(latitude_dd > filter_lat_gt)
  }
  if (!is.na(filter_depth) | is.null(filter_depth)) {
    bio_df <- bio_df |> dplyr::filter(depth_m < filter_depth)
  }
  
  # Get replicate ID from catch
  replicate_id <- unique(catch_cleaned$source)
  
  # Filter bio data based on tow IDs
  bio_df$Trawl_id <- as.double(bio_df$Trawl_id)
  bio_resampled <- bio_df[bio_df$Trawl_id %in% catch_cleaned$Trawl_id, ]
  
  # Add replicate ID as a column
  bio_resampled <- bio_resampled |>
    dplyr::mutate(source = replicate_id)
  
  
  
  return(bio_resampled)
}