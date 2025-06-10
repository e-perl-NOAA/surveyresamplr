test_that("include_or_exclude generates correct structure", {
  # Create test dataframe
  test_df <- data.frame(
    trawlid = 1:5
  )
  
  # Test with single proportion
  proportions <- 0.5
  replicate_num <- 2
  
  result <- include_or_exclude(test_df, proportions, replicate_num)
  
  # Check result structure
  expect_type(result, "list")
  expect_equal(length(result), proportions * replicate_num)
  expect_equal(names(result), c("0.5_1", "0.5_2"))
  
  # Check each dataframe in the result
  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "data.frame")
    expect_equal(nrow(result[[i]]), nrow(test_df))
    expect_equal(ncol(result[[i]]), ncol(test_df) + 1)  # Original columns + RandomAssignment
    expect_true("RandomAssignment" %in% names(result[[i]]))
    expect_true(all(result[[i]]$RandomAssignment %in% c(0, 1)))
  }
  
  # Test with multiple proportions
  proportions <- c(0.3, 0.7)
  replicate_num <- 3
  
  result <- include_or_exclude(test_df, proportions, replicate_num)
  
  # Check result structure
  expect_type(result, "list")
  expect_equal(length(result), length(proportions) * replicate_num)
  expect_equal(
    names(result), 
    c("0.3_1", "0.3_2", "0.3_3", "0.7_1", "0.7_2", "0.7_3")
  )
})

test_that("include_or_exclude uses seed for reproducibility", {
  # Create test dataframe
  test_df <- data.frame(
    trawlid = 1:10
  )
  
  # Run function twice with same parameters
  proportions <- 0.5
  replicate_num <- 1
  
  result1 <- include_or_exclude(test_df, proportions, replicate_num)
  result2 <- include_or_exclude(test_df, proportions, replicate_num)
  
  # Check that random assignments are the same
  expect_equal(
    result1[[1]]$RandomAssignment,
    result2[[1]]$RandomAssignment
  )
})

test_that("include_or_exclude respects proportion parameter", {
  # Create larger test dataframe for better proportion testing
  test_df <- data.frame(
    trawlid = 1:100
  )
  
  # Test with different proportions
  proportions <- c(0.2, 0.8)
  replicate_num <- 5
  
  result <- include_or_exclude(test_df, proportions, replicate_num)
  
  # Check proportions of 1s in each result
  for (i in 1:5) {
    # For 0.2 proportion
    prop_1s <- mean(result[[paste0("0.2_", i)]]$RandomAssignment)
    expect_true(abs(prop_1s - 0.2) < 0.15)  # Allow some variance due to randomness
    
    # For 0.8 proportion
    prop_1s <- mean(result[[paste0("0.8_", i)]]$RandomAssignment)
    expect_true(abs(prop_1s - 0.8) < 0.15)  # Allow some variance due to randomness
  }
})