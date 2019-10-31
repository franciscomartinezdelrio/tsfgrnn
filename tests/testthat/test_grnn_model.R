context("Building a GRNN model")

expect_error(grnn_model(ts(1:5), lags = 0:3, sigma = 5))

expect_error(grnn_model(ts(1:5), lags = 3:5, sigma = 1),
             "Impossible to create one example")
