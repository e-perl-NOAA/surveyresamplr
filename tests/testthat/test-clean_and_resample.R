test_that("clean_and_resample works with valid inputs", {
  # Skip on CRAN to avoid long-running tests
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()

  # Create minimal test data
  test_catch <- data.frame(
    year = rep(2020:2021, each = 5),
    trawlid = 1:10,
    common_name = "test_species",
    total_catch_wt_kg = runif(10, 0, 100),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120),
    depth_m = runif(10, 100, 500),
    pass = sample(1:2, 10, replace = TRUE)
  )

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

  # Test with test = TRUE to avoid running actual models
  # We're just testing that the function runs without errors
  # and returns the expected structure
  expect_no_error(
    clean_and_resample(
      spp_info = test_spp_info,
      catch = test_catch,
      seq_from = 0.5,
      seq_to = 1,
      seq_by = 0.5,
      tot_dataframes = 2,
      replicate_num = 1,
      grid_yrs = test_grid_yrs,
      dir_out = temp_dir,
      test = TRUE
    )
  )
})

test_that("clean_and_resample validates input variables", {
  # Create test data with missing variables
  test_catch <- data.frame(
    year = rep(2020:2021, each = 5),
    trawlid = 1:10,
    common_name = "test_species",
    total_catch_wt_kg = runif(10, 0, 100),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120)
    # Missing depth_m and pass
  )

  # Create test species info with variables not in catch
  test_spp_info <- data.frame(
    srvy = "TEST",
    common_name = "test_species",
    file_name = "test_species",
    filter_lat_gt = 39,
    filter_lat_lt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year) + pass + depth_m", # pass and depth_m not in test_catch
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = "iid, iid"
  )

  # Create test grid years
  test_grid_yrs <- data.frame(
    year = rep(2020:2021, each = 5),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120)
    # Missing pass and depth_m
  )

  # Test that function errors when variables in model_fn are not in catch
  expect_error(
    clean_and_resample(
      spp_info = test_spp_info,
      catch = test_catch,
      seq_from = 0.5,
      seq_to = 1,
      seq_by = 0.5,
      tot_dataframes = 2,
      replicate_num = 1,
      grid_yrs = test_grid_yrs,
      dir_out = tempdir(),
      test = TRUE
    ),
    "ERROR: Not all variables called in funciton are available"
  )
})
