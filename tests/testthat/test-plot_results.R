test_that("plot_results creates expected output files", {
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()
  dir_out <- file.path(temp_dir, "output")
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

  # Create test species directory
  test_srvy <- "TEST"
  test_species <- "test_species"
  dir_spp <- file.path(dir_out, paste0(test_srvy, "_", test_species))
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)

  # Create test data files
  # fit_df
  fit_df <- data.frame(
    srvy = rep(test_srvy, 4),
    common_name = rep(test_species, 4),
    file_name = rep(test_species, 4),
    effort = rep(c("0.5", "1.0"), each = 2),
    term = rep(c("factor(year)2020", "factor(year)2021"), 2),
    estimate = runif(4, -1, 1),
    std.error = runif(4, 0.1, 0.5),
    statistic = rnorm(4),
    p.value = runif(4, 0, 0.1),
    conf.low = runif(4, -2, -1),
    conf.high = runif(4, 1, 2)
  )
  write.csv(fit_df, file.path(dir_spp, "fit_df.csv"))

  # fit_pars
  fit_pars <- data.frame(
    srvy = rep(test_srvy, 4),
    common_name = rep(test_species, 4),
    file_name = rep(test_species, 4),
    effort = rep(c("0.5", "1.0"), each = 2),
    term = rep(c("range", "sigma_O"), 2),
    estimate = runif(4, 0.5, 2),
    std.error = runif(4, 0.1, 0.5),
    statistic = rnorm(4),
    p.value = runif(4, 0, 0.1),
    conf.low = runif(4, 0.1, 0.5),
    conf.high = runif(4, 2, 3)
  )
  write.csv(fit_pars, file.path(dir_spp, "fit_pars.csv"))

  # fit_check
  fit_check <- data.frame(
    srvy = rep(test_srvy, 4),
    common_name = rep(test_species, 4),
    file_name = rep(test_species, 4),
    effort = rep(c("0.5", "1.0"), each = 2),
    component = rep(c("Gradient", "Hessian"), 2),
    result = rep(c("OK", "OK"), 2)
  )
  write.csv(fit_check, file.path(dir_spp, "fit_check.csv"))

  # index
  index <- data.frame(
    srvy = rep(test_srvy, 4),
    common_name = rep(test_species, 4),
    file_name = rep(test_species, 4),
    effort = rep(c("0.5", "1.0"), each = 2),
    year = rep(2020:2021, 2),
    est = runif(4, 1000, 5000),
    se = runif(4, 100, 500),
    log_est = log(runif(4, 1000, 5000))
  )
  write.csv(index, file.path(dir_spp, "index.csv"))

  # Run the plot_results function
  result <- plot_results(srvy = test_srvy, dir_out = dir_out)

  # Check that the function returns the expected structure
  expect_type(result, "list")
  expect_equal(names(result), c("plots", "tables"))
  expect_equal(length(result$plots), 4)
  expect_equal(names(result$tables), c("fit_df", "fit_pars", "fit_check", "index"))

  # Check that output directory was created
  dir_final <- file.path(dir_out, paste0(test_srvy, "_0results"))
  expect_true(dir.exists(dir_final))

  # Check that output files were created
  expect_true(file.exists(file.path(dir_final, "fit_df.csv")))
  expect_true(file.exists(file.path(dir_final, "fit_pars.csv")))
  expect_true(file.exists(file.path(dir_final, "fit_check.csv")))
  expect_true(file.exists(file.path(dir_final, "index.csv")))
  expect_true(file.exists(file.path(dir_final, "index_boxplot_log_biomass.png")))
  expect_true(file.exists(file.path(dir_final, "index_boxplot_log_biomass_SE.png")))
  expect_true(file.exists(file.path(dir_final, "index_boxplot_biomass.png")))
  expect_true(file.exists(file.path(dir_final, "index_timeseries_biomass.png")))
  expect_true(file.exists(file.path(dir_final, "analysisoutput.rdata")))
})

test_that("plot_results handles custom output directory", {
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()
  dir_out <- file.path(temp_dir, "output")
  dir_final <- file.path(temp_dir, "custom_results")
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

  # Create test species directory
  test_srvy <- "TEST"
  test_species <- "test_species"
  dir_spp <- file.path(dir_out, paste0(test_srvy, "_", test_species))
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)

  # Create minimal test data file
  index <- data.frame(
    srvy = test_srvy,
    common_name = test_species,
    file_name = test_species,
    effort = "0.5",
    year = 2020,
    est = 1000,
    se = 100,
    log_est = log(1000)
  )
  write.csv(index, file.path(dir_spp, "index.csv"))

  # Run the plot_results function with custom dir_final
  result <- plot_results(srvy = test_srvy, dir_out = dir_out, dir_final = dir_final)

  # Check that the custom output directory was created
  expect_true(dir.exists(dir_final))

  # Check that output files were created in the custom directory
  expect_true(file.exists(file.path(dir_final, "index.csv")))
  expect_true(file.exists(file.path(dir_final, "index_boxplot_log_biomass.png")))
})

test_that("plot_results handles missing files gracefully", {
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()
  dir_out <- file.path(temp_dir, "output_empty")
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

  # Create test species directory with no data files
  test_srvy <- "TEST"
  test_species <- "test_species"
  dir_spp <- file.path(dir_out, paste0(test_srvy, "_", test_species))
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)

  # Run the plot_results function
  expect_no_error(
    result <- plot_results(srvy = test_srvy, dir_out = dir_out)
  )

  # Check that the function returns the expected structure even with no data
  expect_type(result, "list")
  expect_equal(names(result), c("plots", "tables"))

  # Check that output directory was created
  dir_final <- file.path(dir_out, paste0(test_srvy, "_0results"))
  expect_true(dir.exists(dir_final))
})
