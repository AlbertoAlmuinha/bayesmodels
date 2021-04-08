testthat::context("TEST garch_reg: GARCH")

# SETUP---

rIBM_extended <- rIBM %>%
    future_frame(.length_out = 24, .bind_data = TRUE) 

rIBM_train  <- rIBM_extended %>% drop_na()
rIBM_future <- rIBM_extended %>% filter(is.na(daily_returns))

model_spec_garch <-garchmodels::garch_reg(mode = "regression",
                                         arch_order = 1,
                                         garch_order = 1) %>%
                    set_engine("rugarch") 


# PARSNIP ----

test_that("garch_reg: Regression Parsnip Test", {
    
    testthat::skip_on_cran()
    
    model_fit <- model_spec_garch %>%
        fit(daily_returns ~ date, data = rIBM_train)
    
    predictions <- predict(model_fit, rIBM_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "rugarch_fit_impl")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$models$model_1@model$modeldesc$vmodel, "sGARCH")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "daily_returns")
    
    
    # Structure
    testthat::expect_identical(nrow(rIBM_future), nrow(predictions %>% pull(.pred, 1) %>% .$seriesFor))
    testthat::expect_identical(nrow(rIBM_future), nrow(predictions %>% pull(.pred, 1) %>% .$sigmaFor))
    testthat::expect_identical(rIBM_train$date, model_fit$fit$data$date)
    
    # Out-of-Sample Accuracy Tests
    
    resid <- model_fit$fit$data$.residuals
    
    # - Max Error less than 2000
    testthat::expect_lte(max(abs(resid)), 0.15)
    
    # - MAE less than 1200
    testthat::expect_lte(mean(abs(resid)), 0.02)
    
})



