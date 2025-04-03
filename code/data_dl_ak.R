##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  "here",
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
  "httr", 
  "sp", 
  "RODBC"
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

test_species <- dplyr::bind_rows(
  data.frame(
  srvy = "GOA",
  common_name = c("walleye pollock", "Pacific cod", 
                  "Pacific ocean perch", "flathead sole", 
                  "northern rockfish", "arrowtooth flounder"), 
  species_code = as.character(c(21740, 21720, 
                                30060, 10130, 
                                30420, 10110))), 
  data.frame(
  srvy = "EBS",
  common_name = c("walleye pollock", "snow crab", "Pacific cod", 
                  "red king crab", "blue king crab", 
                  "yellowfin sole", "Pacific halibut", 
                  "Alaska plaice", "flathead sole", "northern rock sole", "arrowtooth flounder"), 
  species_code = as.character(c(21740, 68580, 21720, 
                                69322, 69323, 
                                10210, 10120, 
                                10285, 10130, 10261, 10110)))) 
test_species <- unique(test_species[, c("common_name", "species_code")])
  
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

## Load species data --------------------------------------------------------------
api_link_species <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_species/'

## query the API link
res <- httr::GET(url = paste0(api_link_species, "?offset=0&limit=10000"))
## convert from JSON format
data <- jsonlite::fromJSON(base::rawToChar(res$content)) 

## bind sub-pull to dat data.frame
dat_species <- data$items %>%
  dplyr::select(-links) # necessary for API accounting, but not part of the dataset)

## Load catch data from FOSS ---------------------------------------------------
api_link_catch <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/'

dat <- data.frame()
for (ii in 1:nrow(test_species)) {
  print(test_species$common_name[ii])
  for (i in seq(0, 1000000, 10000)){
    ## find how many iterations it takes to cycle through the data
    print(i)
    ## query the API link
    # res <- httr::GET(url = paste0(api_link_catch, "?offset=",i,"&limit=10000"))
    # res <- httr::GET(url = paste0(api_link_haul, '?limit=10000&q={"species_code":2023,"srvy":"EBS"}'))
    res <- httr::GET(url = paste0(api_link_catch, 
                                  '?offset=',i,'&limit=10000&q={"species_code":',test_species$species_code[ii],'}'))
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

noaa_afsc_catch <- dplyr::full_join(
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
  dplyr::select(-survey_definition_id) %>% # now, redundant
  # get full data 
  dplyr::left_join(dat_catch) %>% 
  dplyr::left_join(dat_haul)  %>% 
  dplyr::left_join(dat_species %>% 
                     dplyr::select(species_code, common_name) %>% 
                     dplyr::mutate(species_code = as.numeric(species_code))) %>% 
  # dplyr::filter(srvy == "EBS") %>% 
  dplyr::mutate(
    Pass = 1, 
                Area_swept_ha = area_swept_km2/100) %>%
  dplyr::select(
    srvy, 
    Trawl_id = hauljoin, 
    Common_name = common_name, 
    species_code, 
    total_catch_numbers = count, 
    total_catch_wt_kg = weight_kg, 
    Latitude_dd = latitude_dd_start, 
    Longitude_dd = longitude_dd_start, 
    Year = year, 
    Area_swept_ha, 
    Pass, 
    temperature_bottom = bottom_temperature_c, 
    depth_m)

save(noaa_afsc_catch, file = here::here("data","noaa_afsc_catch.rda"))

# Make extrapolation grids -----------------------------------------------------

extrap_grid <- function(pred_grid, srvy, srvy_out = NULL, noaa_afsc_catch, crs_proj = NULL) {
  
  crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees
  
  if(is.null(srvy_out)) {
    srvy_out <- srvy
  }
  
  pred_grid <- pred_grid %>% 
    dplyr::mutate(Pass = 1, 
                  srvy = srvy_out)
  
  if(!is.null(crs_proj)) {
    sp_extrap_raster <- sp::SpatialPoints(
      coords = coordinates(as.matrix(pred_grid[,c("Longitude_dd", "Latitude_dd")])), 
      proj4string = CRS(crs_proj) ) %>% 
      sp::spTransform(CRSobj = crs_latlon)
  } else {
  sp_extrap_raster <- SpatialPoints(
    coords = coordinates(as.matrix(pred_grid[,c("Longitude_dd", "Latitude_dd")])), 
    proj4string = CRS(crs_latlon) )    
  }
  
  ## Add depth -------------------------------------------------------------------
  
  x <- noaa_afsc_catch %>%
    dplyr::filter(Year > 2000 & srvy %in% c(srvy)) %>% # lets assume our ability to assess depth has improved, technologically since before 2000. Year negotiable if anyone has strong opinions
    dplyr::select(Longitude_dd, Latitude_dd, depth_m) %>%
    stats::na.omit()  %>% 
    sf::st_as_sf(x = ., 
                 coords = c(x = "Longitude_dd", y = "Latitude_dd"), 
                 crs = sf::st_crs(crs_latlon))
  
  idw_fit <- gstat::gstat(formula = depth_m ~ 1,
                          locations = x,
                          nmax = 4)
  
  depth_raster <- raster::predict(
    idw_fit, sp_extrap_raster) %>%
    # as(sp_extrap_raster, Class = "SpatialPoints")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = crs_latlon)  %>%
    stars::st_rasterize() 
  # plot(extrap_data0, main = "Interpolated Bottom Depths") # Just so we can see what we are looking at:
  
  pred_grid_depth <- stars::st_extract(
    x = depth_raster,
    at = as.matrix(pred_grid[,c("Longitude_dd", "Latitude_dd")])) %>% 
    dplyr::bind_cols(pred_grid) %>% 
    dplyr::rename(Depth_m = var1.pred) %>% 
    dplyr::select(-var1.var) %>%
    stats::na.omit()
  
  save(pred_grid_depth, file = here::here("grids",paste0("noaa_afsc_",tolower(srvy_out),"_pred_grid_depth.rdata")))
  save(depth_raster, file = here::here("grids","depth_rasters",paste0("noaa_afsc_", tolower(srvy_out),"_depth_raster.rdata")))
  
  return(list("pred_grid_depth" = pred_grid_depth, 
              "depth_raster" = depth_raster))
}

# EBS Extrapolation grid -------------------------------------------------------

load(here::here("grids", "orig", "eastern_bering_sea_grid.rda"), verbose = TRUE)

pred_grid <- data.frame(eastern_bering_sea_grid) %>% 
  dplyr::rename(Longitude_dd = Lon, 
                Latitude_dd = Lat, 
                area_km = Area_in_survey_km2) 

a <- extrap_grid(pred_grid = pred_grid, srvy = "EBS", noaa_afsc_catch = noaa_afsc_catch)

## Add temperature -------------------------------------------------------------

# ### Coldpool temperature data --------------------------------------------------
# 
# ### Data that varies over space and time (bottom temperature)
# 
# # Here, bottom temperature, and thereby the cold pool extent, have been show to drive the distribution of many species. This is especially true for walleye pollock. 
# # For this we are going to lean on our in-house prepared validated and pre-prepared [{coldpool} R package](https://github.com/afsc-gap-products/coldpool) (S. Rohan, L. Barnett, and N. Charriere). This data interpolates over the whole area of the survey so there are no missing data. 
# 
# noaa_afsc_ebs_pred_grid_temperature <-
#   dplyr::bind_cols(terra::unwrap(coldpool::ebs_bottom_temperature) %>%
#   terra::project(crs_latlon) %>%
#   terra::extract(st_as_sf(sp_extrap_raster)))
# 
# names(noaa_afsc_ebs_pred_grid_temperature) <- paste0("GEAR_TEMPERATURE_", names(noaa_afsc_ebs_pred_grid_temperature))
# 
# noaa_afsc_ebs_pred_grid_depth_temperature <-
#   dplyr::bind_cols(noaa_afsc_ebs_pred_grid_depth,
#                    noaa_afsc_ebs_pred_grid_temperature)
# 
# save(noaa_afsc_ebs_pred_grid_depth_temperature, file = paste0("data/noaa_afsc_ebs_pred_grid_depth_temperature.rdata"))


# GOA Extrapolation grid -------------------------------------------------------

pred_grid <- read.csv(here::here("grids", "orig", "goa_2025_interpolation_grid.csv"))

pred_grid <- pred_grid %>% 
  dplyr::rename(Longitude_dd = lon, 
                Latitude_dd = lat)

a <- extrap_grid(pred_grid = pred_grid, srvy = "GOA", noaa_afsc_catch = noaa_afsc_catch)

# AI Extrapolation grid -------------------------------------------------------

load(here::here("grids", "orig", "aleutian_islands_grid.rda"), verbose = TRUE)

pred_grid <- data.frame(aleutian_islands_grid) %>% 
  dplyr::rename(Longitude_dd = Lon, 
                Latitude_dd = Lat, 
                area_km = Area_km2)

a <- extrap_grid(pred_grid = pred_grid, srvy = "AI", noaa_afsc_catch = noaa_afsc_catch)

# NBS Extrapolation grid -------------------------------------------------------

load(here::here("grids", "orig", "northern_bering_sea_grid.rda"), verbose = TRUE)

pred_grid <- data.frame(northern_bering_sea_grid) %>% 
  dplyr::rename(Longitude_dd = Lon, 
                Latitude_dd = Lat, 
                area_km2 = Area_in_survey_km2) 

a <- extrap_grid(pred_grid = pred_grid, srvy = "NBS", noaa_afsc_catch = noaa_afsc_catch)

# BS (NBS+EBS) Extrapolation grid -------------------------------------------------------

pred_grid <- read.csv(here::here("grids", "orig", "bering_coarse_grid.csv")) %>% 
  dplyr::mutate(Longitude_dd = ((X/10)-360), 
                Latitude_dd = Y/100) %>% 
  dplyr::select(area_km2, Longitude_dd, Latitude_dd)

a <- extrap_grid(pred_grid = pred_grid, 
            srvy = c("NBS", "EBS"), 
            srvy_out = "BS", 
            crs_proj = "EPSG:3338", 
            noaa_afsc_catch = noaa_afsc_catch)

# Load Design-based biomass data for comparison --------------------------------

locations <- c("Z:/Projects/ConnectToOracle.R")
for (i in 1:length(locations)){
  if (file.exists(locations[i])){
    source(locations[i])
  }
}

noaa_afsc_biomass_estimates <- RODBC::sqlQuery(
  channel = channel_products, 
  query = 
    paste0("SELECT DISTINCT 
bb.SURVEY_DEFINITION_ID,
bb.AREA_ID,
bb.SPECIES_CODE,
bb.YEAR,
bb.BIOMASS_MT,
bb.BIOMASS_VAR,
bb.POPULATION_COUNT,
bb.POPULATION_VAR
FROM GAP_PRODUCTS.AKFIN_BIOMASS bb

WHERE bb.AREA_ID IN (99901, 99902, 99903, 99904)
AND bb.SPECIES_CODE IN (", paste0(test_species$species_code, collapse = ","),") 
")) %>% 
  #   -- WHERE bb.SURVEY_DEFINITION_ID = 98 
  # -- AND bb.SPECIES_CODE IN (21740, 10210, 69322) 
  # -- AND AREA_ID = 99901
  # -- AND bb.YEAR >= 1982
  janitor::clean_names()

# Save table to local directory
save(noaa_afsc_biomass_estimates, file = here::here("data", "noaa_afsc_biomass_estimates.rda"))

