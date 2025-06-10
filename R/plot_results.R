plot_results <- function(srvy, dir_out, dir_final = NULL) {
  # If dir_final not provided, construct default
  if (is.null(dir_final)) {
    dir_final <- file.path(dir_out, paste0(srvy, "_0results"))
  }
  dir.create(dir_final, showWarnings = FALSE, recursive = TRUE)

  # Function to combine result tables
  combine_results <- function(filename, srvy, dir_out) {
    # Find all files with this name
    all_files <- list.files(dir_out, pattern = filename, full.names = TRUE, recursive = TRUE)
    results_dirs <- grep(paste0(srvy, "_"), all_files, value = TRUE)

    results_df <- NULL
    if (length(results_dirs) > 0) {
      results_list <- lapply(results_dirs, function(x) utils::read.csv(x))
      results_df <- do.call(rbind, results_list)
    }
    return(results_df)
  }

  # Read and combine all result files
  tables <- list(
    fit_df = combine_results("fit_df.csv", srvy, dir_out),
    fit_pars = combine_results("fit_pars.csv", srvy, dir_out),
    fit_check = combine_results("fit_check.csv", srvy, dir_out),
    index = combine_results("index.csv", srvy, dir_out)
  )

  # Write combined tables to output directory
  lapply(names(tables), function(name) {
    if (!is.null(tables[[name]])) {
      write.csv(tables[[name]], file.path(dir_final, paste0(name, ".csv")), row.names = FALSE)
    }
  })

  # Create plots
  plots <- list()

  if (!is.null(tables$index)) {
    require(ggplot2)

    # Log biomass boxplot
    plots$log_biomass <- ggplot(tables$index, aes(x = factor(year), y = log_est)) +
      geom_boxplot(aes(fill = factor(effort))) +
      theme_minimal() +
      labs(
        title = "Log Biomass by Year and Effort",
        x = "Year", y = "Log Biomass",
        fill = "Effort"
      )
    ggsave(file.path(dir_final, "index_boxplot_log_biomass.png"), plots$log_biomass)

    # Standard error boxplot
    plots$log_biomass_se <- ggplot(tables$index, aes(x = factor(year), y = se)) +
      geom_boxplot(aes(fill = factor(effort))) +
      theme_minimal() +
      labs(
        title = "Standard Error by Year and Effort",
        x = "Year", y = "Standard Error",
        fill = "Effort"
      )
    ggsave(file.path(dir_final, "index_boxplot_log_biomass_SE.png"), plots$log_biomass_se)

    # Raw biomass boxplot
    plots$biomass <- ggplot(tables$index, aes(x = factor(year), y = est)) +
      geom_boxplot(aes(fill = factor(effort))) +
      theme_minimal() +
      labs(
        title = "Biomass by Year and Effort",
        x = "Year", y = "Biomass",
        fill = "Effort"
      )
    ggsave(file.path(dir_final, "index_boxplot_biomass.png"), plots$biomass)

    # Timeseries plot
    plots$timeseries <- ggplot(tables$index, aes(x = year, y = est)) +
      geom_line(aes(group = interaction(file_name, effort), color = factor(effort))) +
      theme_minimal() +
      labs(
        title = "Biomass Timeseries by Effort",
        x = "Year", y = "Biomass",
        color = "Effort"
      )
    ggsave(file.path(dir_final, "index_timeseries_biomass.png"), plots$timeseries)
  }

  # Save all outputs
  save(
    file = file.path(dir_final, "analysisoutput.rdata"),
    list = c("tables", "plots")
  )

  return(list(tables = tables, plots = plots))
}
