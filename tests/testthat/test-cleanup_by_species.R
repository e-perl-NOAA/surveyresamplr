test_that("cleanup_by_species filters data correctly", {
  # Create test data
  test_catch <- data.frame(
    year = rep(2020:2021, each = 5),
    trawlid = 1:10,
    common_name = c(rep("species_a", 5), rep("species_b", 5)),
    total_catch_wt_kg = runif(10, 0, 100),
    latitude_dd = c(runif(5, 35, 40), runif(5, 40, 45)),
    longitude_dd = runif(10, -125, -120),
    depth_m = c(runif(5, 100, 200), runif(5, 300, 500))
  )
  
  # Create test species info with latitude filter
  test_spp_info <- data.frame(
    srvy = "TEST",
    common_name = "species_a",
    file_name = "species_a",
    filter_lat_gt = 34,  # Should keep all species_a records
    filter_lat_lt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = "iid, iid"
  )
  
  # Run the function
  result <- cleanup_by_species(
    catch = test_catch,
    spp_info = test_spp_info,
    seq_from = 0.5,
    seq_to = 1.0,
    seq_by = 0.5,
    tot_dataframes = 2,
    replicate_num = 1
  )
  
  # Check that result is a list
  expect_type(result, "list")
  
  # Check that we have the expected number of dataframes
  expect_equal(length(result), 2)
  
  # Check that only species_a is included
  expect_equal(unique(result[[1]]$common_name), "species_a")
  
  # Test with depth filter
  test_spp_info2 <- data.frame(
    srvy = "TEST",
    common_name = "species_a",
    file_name = "species_a",
    filter_lat_gt = NA,
    filter_lat_lt = NA,
    filter_depth = 150,  # Should filter out some records
    model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = "iid, iid"
  )
  
  # Run the function with depth filter
  result2 <- cleanup_by_species(
    catch = test_catch,
    spp_info = test_spp_info2,
    seq_from = 0.5,
    seq_to = 1.0,
    seq_by = 0.5,
    tot_dataframes = 2,
    replicate_num = 1
  )
  
  # Check that depth filtering worked
  if (length(result2) > 0 && nrow(result2[[1]]) > 0) {
    expect_true(all(result2[[1]]$depth_m < 150))
  }
})

test_that("tow_fn creates correct tow dataframe", {
  # Create test data
  test_data <- data.frame(
    trawlid = c(1, 2, 3, NA, 5),
    other_col = letters[1:5]
  )
  
  # Run tow_fn
  result <- tow_fn(test_data)
  
  # Check result structure
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), "trawlid")
  
  # Check that NA values are removed
  expect_equal(nrow(result), 4)  # One NA removed
  
  # Check that values are unique
  expect_equal(length(unique(result$trawlid)), nrow(result))
})

test_that("join_dfs correctly merges dataframes", {
  # Create main dataframe
  main_df <- data.frame(
    trawlid = 1:5,
    value = letters[1:5]
  )
  
  # Create list of dataframes to join
  df1 <- data.frame(trawlid = c(1, 3, 5), flag = TRUE)
  df2 <- data.frame(trawlid = c(2, 4), flag = FALSE)
  list_dfs <- list(df1, df2)
  
  # Run join_dfs
  result <- join_dfs(list_dfs, main_df, "trawlid")
  
  # Check result structure
  expect_type(result, "list")
  expect_equal(length(result), 2)
  
  # Check first joined dataframe
  expect_equal(nrow(result[[1]]), 3)
  expect_equal(result[[1]]$trawlid, c(1, 3, 5))
  expect_equal(result[[1]]$value, c("a", "c", "e"))
  expect_equal(result[[1]]$flag, c(TRUE, TRUE, TRUE))
  
  # Check second joined dataframe
  expect_equal(nrow(result[[2]]), 2)
  expect_equal(result[[2]]$trawlid, c(2, 4))
  expect_equal(result[[2]]$value, c("b", "d"))
  expect_equal(result[[2]]$flag, c(FALSE, FALSE))
})