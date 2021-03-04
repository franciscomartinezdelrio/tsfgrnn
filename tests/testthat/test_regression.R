
test_that("grnn regression with one target", {
  model <- grnn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)),
                      lags = 1:2, sigma = 1, transform = "none")
  expect_equal(round(regression(model, c(1, 2))$prediction, 3), 1.801)
})

 test_that("knn regression with multiple targets", {
   model <- grnn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2,
                       sigma = 1, nt = 2, transform = "none")
   expect_equal(round(regression(model, c(1, 2))$prediction, 3), c(1.801, 4.690))
})
