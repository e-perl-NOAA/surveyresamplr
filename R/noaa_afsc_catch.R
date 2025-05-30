#' @title Combined AFSC catch, haul, and species data from FOSS
#' @description The final, validated survey data are publicly accessible soon after surveys are completed on the Fisheries One Stop Shop (FOSS) platform. This data includes catch, haul, and environmental data collected at each station. On the FOSS data platform, users can interactively select, view, and download data. Descriptive documentation and user-examples are available on the metadata page.
#' @usage data('noaa_afsc_catch')
#' @author Alaska Fisheries Science Center, compiled by Emily Markowitz (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 430167 observations on the following 12 variables.
#' \describe{
#'   \item{\code{srvy}}{Abbreviated survey names. Abbreviated survey names. }
#'   \item{\code{trawlid}}{Trawl ID. This is a unique numeric identifier assigned to each (vessel, cruise, and haul) combination.}
#'   \item{\code{common_name}}{Taxon common name. The common name of the marine organism associated with the scientific_name and species_code columns.}
#'   \item{\code{species_code}}{Taxon scientific name. The species code of the organism associated with the common_name and scientific_name columns.}
#'   \item{\code{total_catch_numbers}}{Taxon count. Total whole number of individuals caught in haul or samples collected.}
#'   \item{\code{total_catch_wt_kg}}{Specimen weight (g). Weight of specimen (grams).}
#'   \item{\code{cpue_kgkm2}}{Weight CPUE (kg/km2). Catch weight (kilograms) per unit effort (area swept by the net, units square kilometers).}
#'   \item{\code{latitude_dd}}{Latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree).}
#'   \item{\code{longitude_dd}}{Longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree).}
#'   \item{\code{year}}{Survey year. Year the observation (survey) was collected.}
#'   \item{\code{bottom_temperature_c}}{Bottom temperature (degrees Celsius). Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{depth_m}}{Depth (m). Bottom depth (meters).}#'   }
#' @source https://github.com/afsc-gap-products/gap_products and https://www.fisheries.noaa.gov/foss/f?p=215:28:14951401791129:::::
#' @keywords species code data
#' @examples
#' data(noaa_afsc_catch)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

"noaa_afsc_catch"
