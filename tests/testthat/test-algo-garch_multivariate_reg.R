testthat::context("TEST garch_multivariate_reg: GARCH")

# SETUP---

rX_longer_extended <- rX_longer %>%
    group_by(id) %>%
    future_frame(.length_out = 24, .bind_data = TRUE) %>%
    ungroup()

rX_train  <- rX_longer_extended %>% drop_na()
rX_future <- rX_longer_extended %>% filter(is.na(value))

# PARSNIP ----

test_that("garch_multivariate_reg: Rugarch Engine", {
    
    testthat::skip_on_cran()
    
    model_fit <- garch_multivariate_reg(mode = "regression", type = "ugarchspec") %>%
        set_engine("rugarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                            spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                            spec3 = list(mean.model = list(armaOrder = c(1,0)))),
        ) %>%
        fit(value ~ date + id, data = rX_train)
    
    predictions <- predict(model_fit, rX_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "rugarch_multi_fit_impl")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$extras$names_from, "id")
    testthat::expect_equal(model_fit$fit$extras$values_from, "value")
    testthat::expect_equal(model_fit$fit$extras$index, "date")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
    
    # Structure
    testthat::expect_identical(nrow(rX_future), as.integer(nrow(predictions)/2))
    
    # In-Sample Accuracy Tests
    
    resid <- model_fit$fit$models$model_1@fit[[1]]@fit$residuals
    
    # - Max Error less than 2000
    testthat::expect_lte(max(abs(resid)), 0.15)
    
    # - MAE less than 1200
    testthat::expect_lte(mean(abs(resid)), 0.0097)
    
    testthat::expect_error({
        garch_multivariate_reg(mode = "regression", type = "ugarchspec") %>%
            set_engine("rugarch") %>%
            fit(value ~ date + id, data = rX_train)
    })
    
})



test_that("garch_multivariate_reg: DCC Engine", {
    
    testthat::skip_on_cran()
    
    model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
        set_engine("dcc_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                                spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                                spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                                   dcc_specs = list(dccOrder = c(2,2), distribution = "mvlaplace")) %>%
        fit(value ~ date + id, data = rX_train)
    
    predictions <- predict(model_fit, rX_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "dcc_rmgarch_multi_fit_impl")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$extras$names_from, "id")
    testthat::expect_equal(model_fit$fit$extras$values_from, "value")
    testthat::expect_equal(model_fit$fit$extras$index, "date")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
    dimension <- predictions %>% pull(.pred) %>% .[[1]] %>% .[[1]] %>% attr(., "dim")
    
    # Structure
    testthat::expect_identical(nrow(rX_future), dimension[2]*dimension[3])
    
    # In-Sample Accuracy Tests
    
    correlations <- model_fit$fit$models$model_1@mfit$R[[1]]
    
    # - Diagonal Must Sum 3
    testthat::expect_equal(correlations %>% diag() %>% sum(), 3)
    
    #
    testthat::expect_lte(mean(abs(correlations)), 0.60)
    
    testthat::expect_error({
        garch_multivariate_reg(mode = "regression", type = "ugarchspec") %>%
            set_engine("rugarch") %>%
            fit(value ~ date + id, data = rX_train)
    })
    
    testthat::expect_error({
        garch_multivariate_reg(type = "arfima") %>%
            set_engine("dcc_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                                    spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                                    spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                       dcc_specs = list(dccOrder = c(2,2), distribution = "mvlaplace")) %>%
            fit(value ~ date + id, data = rX_train)
    })
    
})



test_that("garch_multivariate_reg: Copula Engine", {
    
    testthat::skip_on_cran()
    
    model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
        set_engine("c_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                              spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                              spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                                 c_specs = list(dccOrder = c(2,2))) %>%
        fit(value ~ date + id, data = rX_train)
    
    #predictions <- predict(model_fit, rX_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "c_rmgarch_multi_fit_impl")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$extras$names_from, "id")
    testthat::expect_equal(model_fit$fit$extras$values_from, "value")
    testthat::expect_equal(model_fit$fit$extras$index, "date")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")

    
    # In-Sample Accuracy Tests
    
    stdresid <- model_fit$fit$models$model_1@mfit$stdresid[,1]
    
    # - Diagonal Must Sum 3
    testthat::expect_lte(max(abs(stdresid)), 10.50)
    
    #
    testthat::expect_lte(mean(abs(stdresid)), 0.71)
    
    testthat::expect_error({
        predict(model_fit, rX_future)
    })
    
    testthat::expect_error({
        garch_multivariate_reg(type = "arfima") %>%
            set_engine("c_rmgarch" , specs = list(spec1 = list(mean.model = list(armaOrder = c(1,0))),
                                                  spec2 = list(mean.model = list(armaOrder = c(1,0))),
                                                  spec3 = list(mean.model = list(armaOrder = c(1,0)))),
                       c_specs = list(dccOrder = c(2,2))) %>%
            fit(value ~ date + id, data = rX_train)
    })
    
    testthat::expect_error({
        garch_multivariate_reg(type = "ugarchspec") %>%
            set_engine("c_rmgarch" , c_specs = list(dccOrder = c(2,2))) %>%
            fit(value ~ date + id, data = rX_train)
    })
    
})


test_that("garch_multivariate_reg: Go-Garch Engine", {
    
    testthat::skip_on_cran()
    
    model_fit <- garch_multivariate_reg(type = "ugarchspec") %>%
        set_engine("gogarch_rmgarch" , gogarch_specs = list(variance.model = list(garchOrder = c(2,2)))) %>%
        fit(value ~ date + id, data = rX_train)
    
    predictions <- predict(model_fit, rX_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "gogarch_rmgarch_multi_fit_impl")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$extras$names_from, "id")
    testthat::expect_equal(model_fit$fit$extras$values_from, "value")
    testthat::expect_equal(model_fit$fit$extras$index, "date")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
    dimension <- predictions %>% pull(.pred) %>% .[[1]] %>% dim()
    
    # Structure
    testthat::expect_identical(nrow(rX_future), dimension[1]*dimension[2])
    
    # In-Sample Accuracy Tests
    
    resid <- model_fit$fit$models$model_1@mfit$residuals
    
    # - Diagonal Must Sum 3
    testthat::expect_lte(max(abs(resid)), 0.20)
    
    #
    testthat::expect_lte(mean(abs(resid)), 0.012)
    
})
