test_that("wrapper_tinyVAST has expected structure", {
  skip_if_not_installed("sdmTMB")
  skip_on_cran()

  # Check that the function exists
  expect_true(exists("wrapper_tinyVAST"))

  # Check that the function has the expected parameters
  fn_params <- names(formals(wrapper_tinyVAST))
  expected_params <- c("x", "y", "z", "dir_spp", "spp_info", "n_knots")
  expect_equal(fn_params, expected_params)

  # Check default value for n_knots
  expect_equal(formals(wrapper_tinyVAST)$n_knots, 500)
})

test_that("wrapper_tinyVAST creates expected output structure", {
  skip_if_not_installed("sdmTMB")
  skip_on_cran()

  # Create temporary directory for outputs
  temp_dir <- tempdir()
  dir_spp <- file.path(temp_dir, "test_species")
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)

  # Create minimal test data
  set.seed(123)
  test_data <- data.frame(
    year = rep(2020:2021, each = 10),
    total_catch_wt_kg = runif(20, 0, 100),
    latitude_dd = runif(20, 40, 45),
    longitude_dd = runif(20, -125, -120),
    pass = sample(1:2, 20, replace = TRUE)
  )

  # Create test grid data
  test_grid <- data.frame(
    year = rep(2020:2021, each = 5),
    latitude_dd = runif(10, 40, 45),
    longitude_dd = runif(10, -125, -120),
    pass = rep(1:2, 5),
    area_km2 = rep(1, 10)
  )

  # Create test species info
  test_spp_info <- data.frame(
    model_fn = "total_catch_wt_kg ~ 0 + factor(year) + pass",
    model_family = "gaussian",
    model_anisotropy = FALSE,
    model_spatiotemporal = "off"
  )

  # Mock sdmTMB functions to avoid actual model fitting
  mock_make_mesh <- function(x, xy_cols, n_knots) {
    structure(list(mesh = "mock_mesh"), class = "sdmTMBmesh")
  }

  mock_sdmTMB <- function(...) {
    structure(
      list(
        model = "mock_model",
        family = structure(list(), class = "gaussian")
      ),
      class = "sdmTMB"
    )
  }

  mock_predict <- function(object, newdata, return_tmb_object = FALSE) {
    structure(
      list(
        data = newdata,
        est = runif(nrow(newdata), 10, 100)
      ),
      class = "sdmTMB_prediction"
    )
  }

  mock_get_index <- function(pred_obj, area, bias_correct = TRUE) {
    data.frame(
      year = unique(pred_obj$data$year),
      est = aggregate(pred_obj$est, by = list(pred_obj$data$year), mean)$x,
      se = runif(length(unique(pred_obj$data$year)), 1, 10),
      log_est = log(aggregate(pred_obj$est, by = list(pred_obj$data$year), mean)$x)
    )
  }

  # Temporarily assign the mock functions
  orig_make_mesh <- sdmTMB::make_mesh
  orig_sdmTMB <- sdmTMB::sdmTMB
  orig_predict <- stats::predict
  orig_get_index <- sdmTMB::get_index

  assignInNamespace("make_mesh", mock_make_mesh, ns = "sdmTMB")
  assignInNamespace("sdmTMB", mock_sdmTMB, ns = "sdmTMB")
  # We can't easily mock stats::predict, so we'll work around it
  mock_env <- new.env()
  mock_env$predict <- mock_predict
  mock_env$get_index <- mock_get_index

  # Modify the wrapper function temporarily for testing
  test_wrapper <- function(x, y, z, dir_spp, spp_info, n_knots = 500) {
    # make mesh
    mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude_dd", "latitude_dd"), n_knots = n_knots)

    eval(parse(text = paste0(
      "fit <- sdmTMB::sdmTMB(
      ", spp_info$model_fn, ",
      data = x,
      mesh = mesh,
      family = ", spp_info$model_family, '(),
      time = "year",
      anisotropy = ', spp_info$model_anisotropy, ',
      spatiotemporal = as.list(c("',
      gsub(pattern = ", ", replacement = '", "', x = spp_info$model_spatiotemporal), '"))
    )'
    )))

    # get the index using our mock functions
    predictions <- mock_env$predict(fit, newdata = z, return_tmb_object = TRUE)
    index <- mock_env$get_index(predictions, area = z$area_km2, bias_correct = TRUE)

    # save file
    out <- list(
      "fit" = fit,
      "predictions" = predictions,
      "index" = index
    )
    saveRDS(
      out,
      paste0(dir_spp, "modelout_", y, ".rds")
    )

    return(out)
  }

  # Run the test wrapper
  result <- test_wrapper(
    x = test_data,
    y = "test_run",
    z = test_grid,
    dir_spp = dir_spp,
    spp_info = test_spp_info,
    n_knots = 100
  )

  # Check result structure
  expect_type(result, "list")
  expect_equal(names(result), c("fit", "predictions", "index"))
  expect_s3_class(result$fit, "sdmTMB")
  expect_s3_class(result$predictions, "sdmTMB_prediction")
  expect_s3_class(result$index, "data.frame")

  # Check that output file was created
  expect_true(file.exists(file.path(dir_spp, "modelout_test_run.rds")))

  # Restore original functions
  assignInNamespace("make_mesh", orig_make_mesh, ns = "sdmTMB")
  assignInNamespace("sdmTMB", orig_sdmTMB, ns = "sdmTMB")
})
