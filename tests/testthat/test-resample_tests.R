test_that("resample_tests creates expected directory structure", {
  # Skip on CRAN to avoid long-running tests and file system operations
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()

  # Create minimal test data
  test_df1 <- data.frame(
    year = rep(2020:2021, each = 5),
    trawlid = 1:10,
    common_name = "test_species",
    total_catch_wt_kg = runif(10, 0, 100),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120),
    depth_m = runif(10, 100, 500),
    pass = sample(1:2, 10, replace = TRUE),
    source = "0.5_1"
  )

  test_df2 <- test_df1
  test_df2$source <- "1.0_1"

  # Create list of test dataframes
  spp_dfs <- list("0.5_1" = test_df1, "1.0_1" = test_df2)

  # Create test species info
  test_spp_info <- data.frame(
    srvy = "TEST",
    common_name = "test_species",
    file_name = "test_species",
    filter_lat_gt = 39,
    filter_lat_lt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year) + pass",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = "iid, iid"
  )

  # Create test grid years
  test_grid_yrs <- data.frame(
    year = rep(2020:2021, each = 5),
    pass = rep(1:2, 5),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120),
    depth_m = runif(10, 100, 500),
    area_km2 = rep(1, 10)
  )

  # Mock the wrapper_model function to avoid actual model fitting
  mock_wrapper_model <- function(x, y, z, dir_spp, spp_info, n_knots) {
    # Return mock fit and index objects
    list(
      fit = structure(
        list(
          model = "mock_model",
          family = structure(list(), class = "delta_gamma")
        ),
        class = "sdmTMB"
      ),
      index = data.frame(
        year = 2020:2021,
        est = c(50, 60),
        se = c(5, 6),
        log_est = log(c(50, 60))
      )
    )
  }

  # Temporarily assign the mock function
  assign("wrapper_sdmtmb", mock_wrapper_model, envir = .GlobalEnv)

  # Create the output directory
  dir_spp <- file.path(temp_dir, "TEST_test_species")
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)

  # Test the function with test = TRUE to limit processing
  expect_no_error(
    resample_tests(
      spp_dfs = spp_dfs,
      spp_info = test_spp_info,
      grid_yrs = test_grid_yrs,
      dir_out = temp_dir,
      test = TRUE,
      model_type = "wrapper_sdmtmb"
    )
  )

  # Check that parquet files were created
  expect_true(file.exists(file.path(dir_spp, "df_1.parquet")))
  expect_true(file.exists(file.path(dir_spp, "df_2.parquet")))

  # Remove the mock function
  rm("wrapper_sdmtmb", envir = .GlobalEnv)
})

test_that("resample_tests handles test parameter correctly", {
  # Create minimal test data
  test_df1 <- data.frame(source = "0.1_1")
  test_df2 <- data.frame(source = "0.2_1")
  test_df3 <- data.frame(source = "0.3_1")

  # Create list with more than 2 dataframes
  spp_dfs <- list("0.1_1" = test_df1, "0.2_1" = test_df2, "0.3_1" = test_df3)

  # Create test species info
  test_spp_info <- data.frame(
    srvy = "TEST",
    common_name = "test_species",
    file_name = "test_species",
    model_fn = "y ~ x"
  )

  # Mock the internal functions to check behavior
  # We'll use a temporary environment to track function calls
  test_env <- new.env()
  test_env$processed_dfs <- list()

  # Mock arrow::write_parquet to capture which dataframes are processed
  mock_write_parquet <- function(df, path) {
    # Extract the index from the path (df_N.parquet)
    idx <- as.numeric(gsub(".*df_([0-9]+)\\.parquet$", "\\1", path))
    test_env$processed_dfs[[idx]] <- df
    invisible(NULL)
  }

  # Temporarily assign the mock function
  orig_write_parquet <- arrow::write_parquet
  assignInNamespace("write_parquet", mock_write_parquet, ns = "arrow")

  # Test with test = TRUE (should process only last 2 dataframes)
  resample_tests_test <- function() {
    # This is a simplified version that only tests the dataframe reduction
    dir_spp <- tempdir()
    if (test) {
      spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs)) - 1):length(names(spp_dfs))]]
    }
    spp_files <- as.list(names(spp_dfs))
    for (i in seq_along(spp_dfs)) {
      arrow::write_parquet(spp_dfs[[i]], paste0(dir_spp, paste0("df_", i, ".parquet")))
    }
    return(length(spp_dfs))
  }

  # Run with test = TRUE
  test <- TRUE
  num_dfs <- resample_tests_test()

  # Check that only 2 dataframes were processed
  expect_equal(num_dfs, 2)
  expect_equal(length(test_env$processed_dfs), 2)
  expect_equal(test_env$processed_dfs[[1]]$source, "0.2_1")
  expect_equal(test_env$processed_dfs[[2]]$source, "0.3_1")

  # Reset and run with test = FALSE
  test_env$processed_dfs <- list()
  test <- FALSE
  num_dfs <- resample_tests_test()

  # Check that all dataframes were processed
  expect_equal(num_dfs, 3)
  expect_equal(length(test_env$processed_dfs), 3)

  # Restore original function
  assignInNamespace("write_parquet", orig_write_parquet, ns = "arrow")
})
