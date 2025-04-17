#' @title AFSC biomass and population estimates data
#' @description The final, validated survey data are publicly accessible soon after surveys are completed on the Fisheries One Stop Shop (FOSS) platform. This data includes catch, haul, and environmental data collected at each station. On the FOSS data platform, users can interactively select, view, and download data. Descriptive documentation and user-examples are available on the metadata page.
#' @usage data('noaa_afsc_biomass')
#' @author Alaska Fisheries Science Center, compiled by Emily Markowitz (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 738 observations on the following 8 variables.
#' \describe{
#'   \item{\code{species_code}}{Taxon scientific name. The species code of the organism associated with the common_name and scientific_name columns.}
#'   \item{\code{year}}{Survey year. Year the observation (survey) was collected.}#'   }
#' @source https://github.com/afsc-gap-products/gap_products
#' @keywords species code data
#' @examples
#' data(noaa_afsc_biomass)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

'noaa_afsc_biomass'
