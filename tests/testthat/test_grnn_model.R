
expect_error(grnn_model(ts(1:5), lags = 0:3,
                        sigma = 5, transform = "none"))

expect_error(grnn_model(ts(1:5), lags = 3:5,
                        sigma = 1, transform = "none"),
             "Impossible to create one example")
