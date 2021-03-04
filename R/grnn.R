# Build the examples.
#
# Build the examples for a GRNN model to forecast a time series using
# lag values of the series as autoregressive features.
#
# @param timeS The time series.
# @param lags An integer vector with the lags used as feature vector in
#             decreasing order.
# @param nt The number of targets.
#
# @return A list with two fields: 1) a matrix with the features of the
#         examples and 2) a matrix with the targets of the examples
# @examples
# build_examples(ts(1:5), lags = 2:1)
# build_examples(ts(1:5), lags = 2:1, nt = 2)
build_examples <- function(timeS, lags, nt = 1, transform) {
  # MAXLAG   <- lags[1]
  # NCOL     <- length(lags)
  # NROW     <- length(timeS) - MAXLAG - nt + 1
  # patterns <- matrix(0, nrow = NROW, ncol = NCOL)
  # targets  <- matrix(0, nrow = NROW, ncol = nt)
  # row <- 1
  # for (ind in seq(MAXLAG + nt, length(timeS))) {
  #   the_mean <- mean(timeS[ind - nt + 1 - lags])
  #   patterns[row, ] <- timeS[ind - nt + 1 - lags] / the_mean
  #   targets[row, ] <- timeS[(ind - nt + 1):ind] / the_mean
  #   row <- row + 1
  # }
  if (transform == "none") {
    r <- build_examples2(timeS, lags, nt)
  } else if (transform == "multiplicative") {
    r <- build_examples_m(timeS, lags, nt)
  }else if (transform == "additive") {
    r <- build_examples_a(timeS, lags, nt)
  }
  colnames(r$patterns) <- paste0("Lag", lags)
  colnames(r$targets)  <- paste0("H", 1:nt)
  r
  # list(
  #   patterns = patterns,
  #   targets = targets
  # )
}

# Create a GRNN model.
#
# Build a GRNN model to forecast a time series using autoregressive features.
#
# @param timeS The time series.
# @param lags An integer vector with the lags used as feature vector in
#             increasing order.
# @param sigma The sigma parameter.
# @param nt The number of targets (amount of horizons to be forecast).
# @return An object of type grnnModel.
#
grnn_model <- function(timeS, lags, sigma, nt = 1, transform) {
  lags <- rev(lags)
  stopifnot(utils::tail(lags, 1) >= 1)
  MAXLAG <- lags[1]
  if (MAXLAG + nt > length(timeS)) stop("Impossible to create one example")
  examples <- build_examples(timeS, lags, nt, transform)
  structure(
    list(
      ts = timeS,
      lags = lags,
      examples = examples,
      sigma = sigma
    ),
    class = "grnnModel"
  )
}

# Predicts one example doing GRNN regression.
#
# @param model The GRNN model (its class should be grnnModel).
# @param example The features of the example whose target is to be predicted.
#
# @examples
# model <- grnn_model(ts(c(2, 3, 1, 5, 4, 0, 7, 1, 2)), lags = 1:2, sigma = 1)
# regression(model, c(1, 2))
regression <- function(model, example) {
  regression_2(model$sigma, model$examples$patterns, model$example$targets,
                     example)
}
