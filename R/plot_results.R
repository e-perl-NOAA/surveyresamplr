
#' Plot Results and Save Figures
#'
#' This function compiles results from species distribution models (SDMs), generates plots, and saves the figures.
#'
#' @param srvy A character string specifying the survey identifier.
#' @param dir_out A character string specifying the directory for output files.
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Creates a directory for saving images.
#'   \item Searches for relevant files in the output directory.
#'   \item Compiles data from the found files.
#'   \item Saves the compiled data into CSV files.
#'   \item Generates various plots such as boxplots and time series of biomass estimates.
#'   \item Saves the generated plots as PNG files.
#'   \item Saves the list of plots in an RData file.
#' }
#'
plot_results <- function(srvy, dir_out) {
  
  # create directory for images to be saved to
  dir_fig <- paste0(dir_out, paste0(srvy, "_0figures/"))
  dir.create(dir_fig, showWarnings = FALSE)
  
  # find files
  aaa <- base::list.files(path = dir_out, pattern = srvy, full.names = TRUE)
  aaa <- aaa[!base::grepl(pattern = "figures", x = aaa)]
  
  # compile files for each org
  fit_df <- fit_pars <- fit_check <- index <- data.frame()
  for (i in 1:length(aaa)) {
    if (file.exists(paste0(aaa[i], "/fit_df.csv"))) {
      fit_df <- fit_df %>% 
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_df.csv")))
    }    
    if (file.exists(paste0(aaa[i], "fit_pars.csv"))) {
      fit_pars <- fit_pars %>% 
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_pars.csv")))
    }
    if (file.exists(paste0(aaa[i], "/fit_check.csv"))) {
      fit_check <- fit_check %>% 
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_check.csv")))
    }
    if (file.exists(paste0(aaa[i], "/index.csv"))) {
      index <- index %>% 
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/index.csv")))
    }
  }
  
  utils::write.csv(x = fit_df, file = paste0(dir_fig, "/fit_df.csv"))
  utils::write.csv(x = fit_pars, file = paste0(dir_fig, "/fit_pars.csv"))
  utils::write.csv(x = fit_check, file = paste0(dir_fig, "/fit_check.csv"))
  utils::write.csv(x = index, file = paste0(dir_fig, "/index.csv"))
  
  # plotting -------------------------------------------------------------------
  
  plot_list <- c()
  i <- 0
  theme_custom <- ggplot2::theme_bw() + 
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  ## log biomass estimates boxplot ---------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = ggplot2::aes(
      x = as.factor(effort), 
      y = log_est, 
      color = effort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
                  y = "Log biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass.png'
  
  # log(?) SE boxplot ----------------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = ggplot2::aes(
      x = as.factor(effort), 
      y = se, 
      color = effort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
                  y = "Standard error of log biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass_SE.png'
  
  # biomass estimates boxplot --------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = ggplot2::aes(
      x = as.factor(effort), 
      y = est, 
      color = effort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
                  y = "Biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_biomass.png'
  
  # biomass estimates over time ---------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = ggplot2::aes(
      x = year, 
      y = est, 
      color = effort)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "years",
                  y = "Biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom + 
    ggplot2::theme(legend.position = "right")
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_timeseries_biomass.png'
  
  for (ii in 1:length(plot_list)) {
    ggplot2::ggsave(filename = names(plot_list)[ii],
           plot = plot_list[[ii]], 
           path = dir_fig, 
           width = 8, 
           height = 8, 
           device = 'png', 
           dpi = 300)
  }
  
  base::save(plot_list, file = paste0(dir_fig, "figures.rdata"))
}

