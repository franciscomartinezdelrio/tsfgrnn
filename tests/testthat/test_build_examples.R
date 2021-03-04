test_that("build_examples with one target", {
  patterns <- rbind(1:2, 2:3, 3:4)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- matrix(3:5, ncol = 1)
  colnames(targets) <- "H1"
  result <- list(
    patterns = patterns,
    targets = targets
  )
  expect_equal(build_examples(ts(1:5), 2:1, nt = 1, transform = "none"), result)
})

test_that("build_examples with two targets", {
  patterns <- rbind(1:2, 2:3)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- rbind(3:4, 4:5)
  colnames(targets) <- paste0("H", 1:2)
  result <- list(
    patterns = patterns,
    targets = targets
  )
  expect_equal(build_examples(ts(1:5), 2:1, nt = 2, transform = "none"), result)
})

test_that("build_examples with two targets and multiplicative transformation", {
  patterns <- rbind(1:2 / 1.5, 2:3 / 2.5)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- rbind(3:4 / 1.5, 4:5 / 2.5)
  colnames(targets) <- paste0("H", 1:2)
  result <- list(
    patterns = patterns,
    targets = targets
  )
  expect_equal(build_examples(ts(1:5), 2:1, nt = 2, transform = "multiplicative"), result)
})
