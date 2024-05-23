# assumes working directory is Resample-survey-data git repository

require(ggplot2)
require("rnaturalearth")

# downloaded "All Shapefile" zip linked from https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data
# direct link to zip: https://www.boem.gov/renewable-energy/boem-renewable-energy-shapefiles
# then moved required files into this repository
# note: even though only the .shp file is being read, additional files 
# with matching names and different extensions need to be present for read_sf to work

# Oregon areas are call areas at an earlier stage of development
OR_areas <- sf::read_sf("data/wind_area_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp") |>
  dplyr::filter(CATEGORY1 %in% "Oregon Call Area")
# For California, use existing lease areas
CA_areas <- sf::read_sf("data/wind_area_shapefiles/Wind_Lease_Outlines_11_16_2023.shp") |>
  dplyr::filter(STATE == "CA")

# get some info for a map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# # convert shapefiles to polygons to reduce potential for errors associated with
# # ignorance of shapefiles and the sf package
# polygons <- list()
# all_areas_geometry <- c(OR_areas$geometry, CA_areas$geometry)
# all_areas_names <- c(OR_areas$ADDITIONAL, CA_areas$LEASE_NU_1)
# for(i in 1:length(all_areas_geometry)) {
#     polygons[[i]] <-
#       base::as.matrix(all_areas_geometry[[i]])
#     dimnames(polygons[[i]]) <- list( 1:nrow(polygons[[i]]),
#          c("Longitude_dd", "Latitude_dd"))
#     polygons[[i]] <- tibble::as_tibble(polygons[[i]])
# }
# names(polygons) <- all_areas_names

# plot(polygons[[1]])

# read catch for a random species to get locations of all survey hauls
if (!exists("hauls_POP")) {
  hauls_POP <- nwfscSurvey::pull_catch(
    common_name = "Pacific ocean perch",
    survey = "NWFSC.Combo"
  )
  saveRDS(hauls_POP, "data/hauls_POP.rds")
}
hauls_POP <- readRDS("data/hauls_POP.rds")
# convert hauls_POP to a format that works with sf package
hauls_POP_sf <- hauls_POP |>
  dplyr::select(Longitude_dd, Latitude_dd) |>
  sf::st_as_sf(coords = c("Longitude_dd", "Latitude_dd")) |>
  sf::st_set_crs(value = 4269) # set coordinate system to match shapefiles of wind areas
# note: 4269 value is found by looking at sf::st_crs(OR_areas)

# get intersections
# st_intersection returns the points (in sf format)
OR_intersections <- sf::st_intersection(hauls_POP_sf, OR_areas)
CA_intersections <- sf::st_intersection(hauls_POP_sf, CA_areas)

# st_intersects returns NA or a numeric value of which polygon it intersects
OR_intersects <- sf::st_intersects(hauls_POP_sf, OR_areas)
CA_intersects <- sf::st_intersects(hauls_POP_sf, CA_areas)

# get Trawl_id for the points within a wind area
wind_areas_Trawl_id <- hauls_POP$Trawl_id[!is.na(as.numeric(OR_intersects)) |
  !is.na(as.numeric(CA_intersects))]
summary(wind_areas_Trawl_id)
saveRDS(wind_areas_Trawl_id, "data/wind_areas_Trawl_id.rds")

# plot of just Oregon to confirm intersection calculation worked
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = CA_areas, fill = "purple") +
  geom_sf(data = OR_areas, fill = "red") +
  geom_sf(data = hauls_POP_sf, size = 1e-4) +
  geom_sf(data = OR_intersections, col = "green", size = 1e-4) +
  geom_sf(data = CA_intersections, col = "green", size = 1e-4) +
  coord_sf(xlim = c(-126, -123), ylim = c(39.5, 44.5))
# save plot
ggsave("figures/wind_area_map_north.png")

# plot of CA to confirm intersection calculation worked
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = CA_areas, fill = "purple") +
  geom_sf(data = OR_areas, fill = "red") +
  geom_sf(data = hauls_POP_sf, size = 1e-4) +
  geom_sf(data = OR_intersections, col = "green", size = 1e-4) +
  geom_sf(data = CA_intersections, col = "green", size = 1e-4) +
  coord_sf(xlim = c(-125, -120), ylim = c(33, 38))
# save plot
ggsave("figures/wind_area_map_south.png")
