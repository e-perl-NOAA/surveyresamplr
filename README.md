<!-- README.md is generated from README.Rmd. Please edit that file -->

# [surveyresamplr](https://github.com/emilymarkowitz-noaa/surveyresamplr) <img src="https://github.com/emilymarkowitz-noaa/surveyresamplr/blob/main/inst/img/logo.png?raw=true" alt="Logo." align="right" width="139" height="139"/>

> Last run: April 20, 2025

# Purpose

The ‘get prediction grid’ script should be run before running other
scripts. Use the ‘pull data’ file or go to the ‘data’ folder to get the
data we are working with then run the ‘main’ file. Other scripts provide
supporting functions that are run in the ‘main’ file. Due to the high
number of species, effort scenarios, and replicates, the script is very
computationally intensive. Reduce the number of species/effort
scenarios/replicates when testing.

> This repository is under active development and all code, results, and
> other materials should be considered preliminary. Find code used for
> various versions in the
> \[releases\](<https://github.com/emilymarkowitz-noaa/surveyresamplr/releases>
> section for finalized products and project milestones.

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

Alaska Fisheries Science Center, National Marine Fisheries Service,
National Oceanic and Atmospheric Administration, Seattle, WA 98195

**Derek Bolser** (Derek.Bolser AT noaa.gov;
[@dgbolser-NOAA](https://github.com/dgbolser-NOAA))

Office of Science and Technology, National Marine Fisheries Service,
National Oceanic and Atmospheric Administration, Silver Spring, MD 98195

**Elizabeth Perl** (Elizabeth.Perl AT noaa.gov;
[@e-perl-NOAA](https://github.com/e-perl-NOAA))

Affiliate with ECS Federal in support of the Office of Science and
Technology, National Marine Fisheries Service, National Oceanic and
Atmospheric Administration, Silver Spring, MD 98195

**Ian Taylor** (Ian.Taylor AT noaa.gov;
[@iantaylor-NOAA](https://github.com/iantaylor-NOAA))

Northwest Fisheries Science Center, National Marine Fisheries Service,
National Oceanic and Atmospheric Administration, Seattle, WA 98195

# Table of contents

> - [*Table of contents*](#table-of-contents)
> - [*User Resources*](#user-resources)
> - [*Installation notes*](#installation-notes)
> - [*Windows will not update a package already loaded, so then you have
>   to remove the package and install it from scratch. There are two
>   suggested packages, ‘graph’ and ‘Rgraphviz’, that are on
>   Bioconductor, and you can install those
>   with:*](#windows-will-not-update-a-package-already-loaded,-so-then-you-have-to-remove-the-package-and-install-it-from-scratch.-there-are-two-suggested-packages,-‘graph’-and-‘rgraphviz’,-that-are-on-bioconductor,-and-you-can-install-those-with:)
> - [*Cite data used in analysis*](#cite-data-used-in-analysis)
> - [*Suggestions and Comments*](#suggestions-and-comments)
> - [*R Version Metadata*](#r-version-metadata)
> - [*Legal*](#legal)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

# User Resources

- [GitHub
  repository](https://github.com/emilymarkowitz-noaa/surveyresamplr).

- [Access Tips and Documentation for All AFSC Groundfish Assessment
  Program Production
  Data](https://afsc-gap-products.github.io/gap_products/)

- [AFSC Groundfish Assessment Program Fisheries One Stop Shop
  (FOSS)](https://www.fisheries.noaa.gov/foss)

- [Groundfish Assessment Program Bottom Trawl
  Surveys](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys)

- NWFSC links TBD

- NEFSC links TBD

# Installation notes

``` r
devtools::install_github("emilymarkowitz-noaa/surveyresamplr", build_vignettes = TRUE)
```

``` r
install.packages("INLA",repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
inla.upgrade() # for the stable version
# Windows will not update a package already loaded, so then you have to remove the package and install it from scratch. There are two suggested packages, ‘graph’ and ‘Rgraphviz’, that are on Bioconductor, and you can install those with:

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)


options(repos=c( inlabruorg = "https://inlabru-org.r-universe.dev", INLA = "https://inla.r-inla-download.org/R/testing", CRAN = "https://cran.rstudio.com") )
install.packages("fmesher")
```

# Cite data used in analysis

NOAA Fisheries Alaska Fisheries Science Center (2024)

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-FOSSAFSCData" class="csl-entry">

NOAA Fisheries Alaska Fisheries Science Center. (2024). *Fisheries one
stop shop public data: RACE division bottom trawl survey data query*.
https://www.fisheries.noaa.gov/foss; U.S. Dep. Commer.

</div>

</div>

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/emilymarkowitz-noaa/surveyresamplr/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/emilymarkowitz-noaa/surveyresamplr/issues).

# R Version Metadata

    FALSE R version 4.4.3 (2025-02-28 ucrt)
    FALSE Platform: x86_64-w64-mingw32/x64
    FALSE Running under: Windows 10 x64 (build 19045)
    FALSE 
    FALSE Matrix products: default
    FALSE 
    FALSE 
    FALSE locale:
    FALSE [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    FALSE 
    FALSE time zone: America/Los_Angeles
    FALSE tzcode source: internal
    FALSE 
    FALSE attached base packages:
    FALSE [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
    FALSE 
    FALSE other attached packages:
    FALSE  [1] fontawesome_0.5.3      FishStatsUtils_2.13.2  marginaleffects_0.25.1 units_0.8-7            ggspatial_1.1.9        pkgdown_2.1.1          roxygen2_7.3.2         RODBC_1.3-26          
    FALSE  [9] sp_2.2-0               httr_1.4.7             jsonlite_2.0.0         nwfscSurvey_2.2        coldpool_3.4-3         stringr_1.5.1          reshape2_1.4.4         lubridate_1.9.4       
    FALSE [17] fields_16.3.1          spam_2.11-1            gstat_2.1-3            ggthemes_5.1.0         akgfmaps_4.0.3         terra_1.8-42           stars_0.6-8            abind_1.4-8           
    FALSE [25] sf_1.0-20              readr_2.1.5            plyr_1.8.9             remotes_2.5.0          devtools_2.4.5         usethis_3.1.0          INLA_24.12.11          TMB_1.9.17            
    FALSE [33] cluster_2.1.8          MASS_7.3-64            Matrix_1.7-2           sdmTMB_0.7.0           future.callr_0.8.2     future.apply_1.11.3    arrow_19.0.1.1         sampling_2.10         
    FALSE [41] doParallel_1.0.17      iterators_1.0.14       foreach_1.5.2          furrr_0.3.1            future_1.40.0          purrr_1.0.4            forcats_1.0.0          data.table_1.17.0     
    FALSE [49] janitor_2.2.1          tibble_3.2.1           ggplot2_3.5.2          viridis_0.6.5          viridisLite_0.4.2      tidyr_1.3.1            magrittr_2.0.3         dplyr_1.1.4           
    FALSE [57] surveyresamplr_0.0.1   here_1.0.1            
    FALSE 
    FALSE loaded via a namespace (and not attached):
    FALSE  [1] rstudioapi_0.17.1  estimability_1.5.1 rmarkdown_2.29     fs_1.6.6           vctrs_0.6.5        memoise_2.0.1      htmltools_0.5.8.1  raster_3.6-32      parallelly_1.43.0  KernSmooth_2.23-26
    FALSE [11] htmlwidgets_1.6.4  emmeans_1.11.0     zoo_1.8-14         cachem_1.1.0       mime_0.13          lifecycle_1.0.4    pkgconfig_2.0.3    R6_2.6.1           fastmap_1.2.0      shiny_1.10.0      
    FALSE [21] snakecase_0.11.1   digest_0.6.37      colorspace_2.1-1   ps_1.9.1           chron_2.3-62       rprojroot_2.0.4    pkgload_1.4.0      timechange_0.3.0   mgcv_1.9-1         compiler_4.4.3    
    FALSE [31] proxy_0.4-27       intervals_0.15.5   bit64_4.6.0-1      withr_3.0.2        backports_1.5.0    DBI_1.2.3          pkgbuild_1.4.7     maps_3.4.2.1       sessioninfo_1.2.3  classInt_0.4-11   
    FALSE [41] tools_4.4.3        httpuv_1.6.16      glue_1.8.0         callr_3.7.6        nlme_3.1-167       promises_1.3.2     grid_4.4.3         generics_0.1.3     lpSolve_5.6.23     gtable_0.3.6      
    FALSE [51] tzdb_0.5.0         class_7.3-23       hms_1.1.3          xml2_1.3.8         pillar_1.10.2      later_1.4.2        splines_4.4.3      lattice_0.22-6     FNN_1.1.4.1        bit_4.6.0         
    FALSE [61] tidyselect_1.2.1   miniUI_0.1.2       knitr_1.50         gridExtra_2.3      xfun_0.52          stringi_1.8.7      yaml_2.3.10        evaluate_1.0.3     codetools_0.2-20   cli_3.6.4         
    FALSE [71] xtable_1.8-4       munsell_0.5.1      processx_3.8.6     spacetime_1.3-3    Rcpp_1.0.14        globals_0.17.0     readtext_0.91      ellipsis_0.3.2     fmesher_0.3.0      assertthat_0.2.1  
    FALSE [81] dotCall64_1.2      profvis_0.4.0      urlchecker_1.0.1   listenv_0.9.1      mvtnorm_1.3-3      scales_1.3.0       xts_0.14.1         e1071_1.7-16       rlang_1.1.5        cowplot_1.1.3

# Legal

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
