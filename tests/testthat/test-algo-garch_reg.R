# ---- STANDARD ARIMA ----
context("TEST garch_reg: stan")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- garch_reg(
    arch_order               = 1,
    garch_order              = 1,
    mgarch_order             = 1, 
    non_seasonal_ar          = 1,
    non_seasonal_ma          = 1,
    pred_seed                = 123
) %>%
    set_engine("stan")


# PARSNIP ----

# * NO XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("garch_reg: stan, (No xregs), Test Model Fit Object", {
    
    testthat::expect_s3_class(model_fit$fit, "garch_stan_fit_impl")
    
    # $fit
    
    testthat::expect_s3_class(model_fit$fit$models$model_1, "varstan")
    
    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")
    
    testthat::expect_equal(names(model_fit$fit$data)[1], "date")
    
    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))
    
    # $preproc
    
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
})

test_that("garch_reg: stan, (No xregs), Test Predictions", {
    
    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)
    
})

# * XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + month(date, label = TRUE), data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("garch_reg: stan, (XREGS), Test Model Fit Object", {
    
    testthat::expect_s3_class(model_fit$fit, "garch_stan_fit_impl")
    
    # $fit
    
    testthat::expect_s3_class(model_fit$fit$models$model_1, "varstan")
    
    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")
    
    testthat::expect_equal(names(model_fit$fit$data)[1], "date")
    
    testthat::expect_true(!is.null(model_fit$fit$extras$xreg_recipe))
    
    # $preproc
    
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
})

test_that("sarima_reg: stan (XREGS), Test Predictions", {
    
    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)
    
})


# ---- WORKFLOWS ----

# Model Spec
model_spec <- garch_reg(
    arch_order               = 1,
    garch_order              = 1,
    mgarch_order             = 1, 
    non_seasonal_ar          = 1,
    non_seasonal_ma          = 1,
    pred_seed                = 123
) %>%
    set_engine("stan")

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_log(value, skip = FALSE)

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

wflw_fit <- wflw %>%
    fit(training(splits))

# Forecast
predictions_tbl <- wflw_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits), actual_data = training(splits)) %>%
    mutate_at(vars(.value), exp)



# TESTS
test_that("garch_reg: stan (workflow), Test Model Fit Object", {
    
    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "garch_stan_fit_impl")
    
    # $fit
    
    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "varstan")
    
    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")
    
    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")
    
    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))
    
    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")
    
})

test_that("garch_reg: stan (workflow), Test Predictions", {
    
    full_data <- bind_rows(training(splits), testing(splits))
    
    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)
    
})
