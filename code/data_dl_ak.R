# NOTES ------------------------------------------------------------------------
# Resample_survey_data: Multiple species, multiple years
# Em Markowitz March 2025
# Alaska Fisheries
#### Resample_survey_data: Multiple species, multiple years
# ------------------------------------------------------------------------------

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  "devtools", 
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "here",
  "readr",
  "tibble",
  "janitor", 
  "data.table", 
  "jsonlite", 
  "httr"
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    if (p == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (p == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}

# CPUE/Catch Load data ---------------------------------------------------------

  ## Load haul data --------------------------------------------------------------
  api_link_haul <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/'
  
  dat <- data.frame()
  for (i in seq(0, 500000, 10000)){
    ## find how many iterations it takes to cycle through the data
    print(i)
    ## query the API link
    res <- httr::GET(url = paste0(api_link_haul, "?offset=",i,"&limit=10000"))
    ## convert from JSON format
    data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
    
    ## if there are no data, stop the loop
    if (is.null(nrow(data$items))) {
      break
    }
    
    ## bind sub-pull to dat data.frame
    dat <- dplyr::bind_rows(dat, 
                            data$items %>%
                              dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
  }
  
  dat_haul <- dat
  
  ## Load catch data from FOSS ---------------------------------------------------
  
  api_link_catch <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/'
  
  test_species1 <- test_species[test_species$SRVY == "BS",]
  
  dat <- data.frame()
  for (ii in 1:nrow(test_species1)) {
    print(ii)
  for (i in seq(0, 1000000, 10000)){
    ## find how many iterations it takes to cycle through the data
    print(i)
    ## query the API link
    # res <- httr::GET(url = paste0(api_link_catch, "?offset=",i,"&limit=10000"))
    # res <- httr::GET(url = paste0(api_link_haul, '?limit=10000&q={"species_code":2023,"srvy":"EBS"}'))
    res <- httr::GET(url = paste0(api_link_catch, 
                                  '?offset=',i,'&limit=10000&q={"species_code":',test_species1$species_code[ii],'}'))
    # res <- httr::GET(url = 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/?offset=0&limit=10000&q={%22species_code%22:21740,%22species_code%22:21720}')
    ## convert from JSON format
    data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
    
    ## if there are no data, stop the loop
    if (is.null(nrow(data$items))) {
      break
    }
    
    ## bind sub-pull to dat data.frame
    dat <- dplyr::bind_rows(dat, 
                            data$items %>%
                              dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
  }
  }
  dat_catch <- dat
  
  ## Zero filled data ------------------------------------------------------------
  
  data_survey <- noaa_afsc_public_foss <- dplyr::full_join(
    # find all species that have been caught, by survey
    x = dplyr::left_join(dat_catch, dat_haul, by = "hauljoin") %>%
      dplyr::select(survey_definition_id, species_code) %>%
      dplyr::distinct(),
    # find all haul events (hauljoins), by survey
    y = dat_haul %>%
      dplyr::select(survey_definition_id, hauljoin) %>%
      dplyr::distinct(),
    relationship = "many-to-many",
    by = "survey_definition_id"
  ) %>% 
    dplyr::select(-survey_definition_id) # now, redundant
  

  save(list = noaa_afsc_public_foss, file = paste0(wd, "/data/noaa_afsc_public_foss.rda"))

  # TOLEDO 
  # save data to data folder
  # rename columns to match nwfsc data
  # catch <- read.csv(here::here("data","nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
  
  data_survey <- data_survey %>% 
  dplyr::mutate(Reason_station_invalid = 0, 
                cpue_kg_per_ha_der = cpue_kgkm2/100, 
                Legacy_performance_code = -999, 
                Partition = NA, 
                Performance = "Satisfactory", 
                Station_invalid = 0, 
                Partition_sample_types = NA, 
                Subsample_count = NA, 
                Subsample_wt_kg =  NA, 
                Area_swept_ha = area_swept_km2/100, 
                Pass = 1) %>%
  dplyr::select(
    Reason_station_invalid, 
    Common_name = common_name, 
    cpue_kg_per_ha_der, 
    Legacy_performance_code, 
    Partition, 
    Performance, 
    Project = survey, 
    Scientific_name = scientific_name, 
    Station_invalid, 
    Partition_sample_types, 
    Subsample_count, 
    Subsample_wt_kg, 
    total_catch_numbers = count, 
    total_catch_wt_kg = weight_kg, 
    Trawl_id = hauljoin, 
    Vessel = vessel_name, 
    Year = year, 
    Area_swept_ha, 
    Datetime_utc_iso = date_time, 
    Depth_m = depth_m, 
    Latitude_dd = latitude_dd_start, 
    Longitude_dd = longitude_dd_start, 
    Pass, 
    Date = date_time, 
    cpue_kg_km2 = cpue_kgkm2
  )
  
  save(list = noaa_afsc_public_foss, file = paste0(wd, "/data/noaa_afsc_public_foss.rda"))
  

# catch <- read.csv(paste0(wd,"/data/nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024


  ## Metadata file ---------------------------------------------------------------
#'   
#'   column <- metadata_colname %>%
#'     dplyr::filter(metadata_colname %in% toupper(names(noaa_afsc_public_foss))) %>%
#'     dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
#'     dplyr::distinct()
#'   
#'   str0 <- paste0("#' @title Public data from FOSS for EBS walleye pollock, yellowfin sole, and red king crab from 1982 to present
#' #' @description ",metadata_table_comment$COMMENT[metadata_table_comment$TABLE_NAME == "FOSS_CATCH"]," 
#' #' @usage data('noaa_afsc_public_foss')
#' #' @author AFSC Groundfish Assessment Program (nmfs.afsc.gap.metadata AT noaa.gov)
#' #' @format A data frame with ",nrow(noaa_afsc_public_foss)," observations on the following ",
#' ncol(noaa_afsc_public_foss)," variables.
#' #' \\describe{
#' ",
#' paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
#' "#'   }
#' #' @source https://github.com/afsc-gap-products/gap_products
#' #' @keywords species code data
#' #' @examples
#' #' data(noaa_afsc_public_foss)
#' #' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska. 
#' 
#' 'noaa_afsc_public_foss'")
# 
  # 
  # write.table(str0, 
  #             file = here::here("R","noaa_afsc_public_foss.R"), 
  #             sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Load Design-based biomass data for comparison --------------------------------
  
  locations <- c("Z:/Projects/ConnectToOracle.R")
  for (i in 1:length(locations)){
    if (file.exists(locations[i])){
      source(locations[i])
    }
  }
  
  ## Load column metadata table --------------------------------------------------
  
  # metadata_table_comment <- dplyr::bind_rows(
  #   # tables
  #   RODBC::sqlQuery(
  #     channel = channel_products,
  #     query = "SELECT table_name, comments
  # FROM all_tab_comments
  # WHERE owner = 'GAP_PRODUCTS'
  # ORDER BY table_name") %>%
  #   data.frame(),
  # # materialized view
  # RODBC::sqlQuery(
  #   channel = channel_products,
  #   query = "SELECT *FROM user_mview_comments") %>%
  #   data.frame() %>%
  #   dplyr::rename(TABLE_NAME = MVIEW_NAME) )
  # 
  # metadata_colname <- RODBC::sqlQuery(
  #   channel = channel_products,
  #   query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN") %>%
  #   janitor::clean_names()
  
  ## Load biomass data from oracle -----------------------------------------------
  
  noaa_afsc_biomass_estimates <- RODBC::sqlQuery(
    channel = channel_products, 
    query = 
      "SELECT DISTINCT 
bb.SURVEY_DEFINITION_ID,
bb.AREA_ID,
bb.SPECIES_CODE,
bb.YEAR,
bb.BIOMASS_MT,
bb.BIOMASS_VAR,
bb.POPULATION_COUNT,
bb.POPULATION_VAR
FROM GAP_PRODUCTS.AKFIN_BIOMASS bb

WHERE bb.AREA_ID IN (99901, 99902)
AND bb.SPECIES_CODE IN (", paste0(test_species$species_code, collapse = ","),") 
") %>% 
    #   -- WHERE bb.SURVEY_DEFINITION_ID = 98 
    # -- AND bb.SPECIES_CODE IN (21740, 10210, 69322) 
    # -- AND AREA_ID = 99901
    # -- AND bb.YEAR >= 1982
    janitor::clean_names()
  
  # Save table to local directory
  save(noaa_afsc_biomass_estimates, file = here::here("data", "noaa_afsc_biomass_estimates.rda"))
  
  ## Metadata file ---------------------------------------------------------------
  #' 
  #' column <- metadata_colname %>%
  #'   dplyr::filter(metadata_colname %in% toupper(names(noaa_afsc_biomass_estimates))) %>%
  #'   dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
  #'   dplyr::distinct()
  #' 
  #' str0 <- paste0("#' @title Biomass Estimates from AKFIN for EBS walleye pollock, yellowfin sole, and red king crab from 1982 to present
  #' #' @description ",metadata_table_comment$COMMENT[metadata_table_comment$TABLE_NAME == "AKFIN_BIOMASS"]," 
  #' #' @usage data('noaa_afsc_biomass_estimates')
  #' #' @author AFSC Groundfish Assessment Program (nmfs.afsc.gap.metadata AT noaa.gov)
  #' #' @format A data frame with ",nrow(noaa_afsc_biomass_estimates)," observations on the following ",
  #' ncol(noaa_afsc_biomass_estimates)," variables.
  #' #' \\describe{
  #' ",
  #' paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
  #' "#'   }
  #' #' @source https://github.com/afsc-gap-products/gap_products
  #' #' @keywords species code data biomass
  #' #' @examples
  #' #' data(noaa_afsc_biomass_estimates)
  #' #' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska. 
  #' 
  #' 'noaa_afsc_biomass_estimates'")
  #' 
  #' write.table(str0, 
  #'             file = here::here("R","noaa_afsc_biomass_estimates.R"), 
  #'             sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Load in extrapolation grid ---------------------------------------------------

## Depth -----------------------------------------------------------------------

# Here in the Bering sea, the depth rarely changes. The modeler may consider making this variable time-varying as well if they are say, in the Gulf of Alaska or the Aleutian Islands where currents and island formation can markedly change depth. 

# For this, we are going to create a raster of depth in the Bering sea from the survey data so we can merge that into the dataset at the prediction grid lat/lons. 

# project spatial data
crs_proj <- "EPSG:3338" # NAD83 / Alaska Albers
crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees
  
  # Prepare covariate data
  # Here we want to match covariate data to the prediction grid. 
  
  dat_cov <- pred_grid_ebs %>% 
    dplyr::select(-Shape_Area) %>% 
    dplyr::mutate( 
      sx = ((lon - mean(lon, na.rm = TRUE))/1000),
      sy = ((lat - mean(lat, na.rm = TRUE))/1000))
  
  ### get depths ---------------------------------------------------------------
  
  library(sp)
  sp_extrap_raster <- sp::SpatialPoints(
    coords = sp::coordinates(as.matrix(dat_cov[,c("lon", "lat")])), 
    proj4string = sp::CRS(crs_latlon) )
  
  x <- dat_survey %>%
    dplyr::select(Lon = Longitude_dd, Lat = Latitude_dd, BOTTOM_DEPTH = Depth_m) %>%
    stats::na.omit()  %>% 
    sf::st_as_sf(x = ., 
                 coords = c(x = "Lon", y = "Lat"), 
                 crs = sf::st_crs(crs_latlon))
  
  idw_fit <- gstat::gstat(formula = BOTTOM_DEPTH ~ 1,
                          locations = x,
                          nmax = 4)
  
  # stn_predict <- raster::predict(idw_fit, x)
  
  extrap_data0 <- raster::predict(
    idw_fit, sp_extrap_raster) %>%
    # as(sp_extrap_raster, Class = "SpatialPoints")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs_latlon)  %>%
    stars::st_rasterize() 
  
  extrap_data <- stars::st_extract(x = extrap_data0,
                                   at = as.matrix(dat_cov[,c("lon", "lat")]))
  
  # to make future runs of this faster:
  save(extrap_data0, extrap_data, 
       file = paste0(wd, "data/bottom_depth.rdata"))
  
  # plot(extrap_data0, main = "Interpolated Bottom Depths") 
  # 
  # dat_cov <- cbind.data.frame(dat_cov, 
  #                             "BOTTOM_DEPTH" = extrap_data$var1.pred) %>%
  #   stats::na.omit()
  # 
  # head(dat_cov)
  
  ### Coldpool temperature data --------------------------------------------------
  
  ### Data that varies over space and time (bottom temperature)
  
  # Here, bottom temperature, and thereby the cold pool extent, have been show to drive the distribution of many species. This is especially true for walleye pollock. 
  # For this we are going to lean on our in-house prepared validated and pre-prepared [{coldpool} R package](https://github.com/afsc-gap-products/coldpool) (S. Rohan, L. Barnett, and N. Charriere). This data interpolates over the whole area of the survey so there are no missing data. 
  
  # plot(coldpool::ebs_bottom_temperature[[1]]) # Just so we can see what we are looking at: 
  
  tmp <- c()
  for (i in 1:length(YEARS)) {
    tmp <- c(tmp, 
             grep(pattern = YEARS[i], x = names(coldpool::ebs_bottom_temperature)))
  }
  
  extrap_data0 <- coldpool::ebs_bottom_temperature[[tmp]] %>% 
    as(., Class = "SpatialPointsDataFrame") %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs_latlon)  %>%
    stars::st_rasterize() %>% 
    stars::st_extract(x = .,
                      at = as.matrix(dat_cov[,c("lon", "lat")]))
  
  names(extrap_data0) <- paste0("GEAR_TEMPERATURE", YEARS)
  
  dat_cov <- dplyr::bind_cols(dat_cov, extrap_data0) %>% 
    na.omit()
  
  # head(dat_cov)
  
## Extrapolation grid ----------------------------------------------------------



# This is another form of the same grid:
load(here::here("data/pred_grid_ebs.rda")) # object: pred_grid_ebs
#pred_grid_ebs <- read.csv(here::here("data/ebs_2022_epsg3338.csv"),header = TRUE)

get_crs(dat = pred_grid_ebs,ll_names = c("lon","lat"))

grid <- add_utm_columns(pred_grid_ebs, 
                        #ll_crs = 32603, 
                        ll_names = c("lon", "lat"))
grid$area_swept_km2 <- 1 # if you have area swept in your model as an offset (common when you're using CPUE data) you can do this (I think)

range(grid$X)

grid$Shape_Area_ha <- grid$Shape_Area*0.0001 # original Shape_area is in m^2

grid$Longitude_dd<- grid$lon
grid$Latitude_dd<- grid$lat
grid$Pass<- grid$pass_scaled
grid$Depth_m<- grid$depth

# TOLEDO
# NBS + EBS 
# probably from here, but maybe built into sdmtmb
# https://github.com/afsc-gap-products/model-based-indices/tree/main/extrapolation_grids


# ex 
# load(here::here("data","california_current_grid.rda"))

# # set up grid
# #rename x and y cols
# california_current_grid$Longitude_dd<- california_current_grid$longitude
# california_current_grid$Latitude_dd<- california_current_grid$latitude
# california_current_grid$Pass<- california_current_grid$pass_scaled
# california_current_grid$Depth_m<- california_current_grid$depth
# 
# #make gridyrs
# grid_yrs <- replicate_df(california_current_grid, "Year", unique(catch$Year))

