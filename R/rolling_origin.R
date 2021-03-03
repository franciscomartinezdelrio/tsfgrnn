#' Assessing forecasting accuracy with rolling origin
#'
#' It uses the model and the time series associated with a \code{grnnForecast}
#' object to assess the forecasting accuracy of the model using the last
#' \code{h} values of the time series to build test sets applying a rolling
#' origin evaluation.
#'
#' This function assesses the forecast accuracy of the model used by the
#' \code{grnnForecast} object. It uses \code{h} different test and training
#' sets. The first test set consists of the last \code{h} values of the time
#' series (the training set is formed by the previous values). The next test set
#' consists of the last \eqn{h - 1} values of the time series and so on (the
#' last test set is formed by the last value of the time series).
#'
#' @param grnnf A \code{grnnForecast} object.
#' @param h A positive integer. The forecast horizon. If \code{NULL} (the
#'   default) the prediction horizon of the \code{gnnForecast} object is used.
#' @param rolling A logical. If \code{TRUE} (the default), forecasting horizons
#'   from 1 to \code{h} are used. Otherwise, only horizon \code{h} is used.
#' @return A list containing at least the following fields:
#'
#'   \item{\code{test_sets}}{a matrix containing the test sets used in the
#'   evaluation. Every row contains a different test set.}
#'   \item{\code{predictions}}{The predictions for the test sets.}
#'   \item{\code{errors}}{The errors for the test sets.}
#'   \item{\code{global_accu}}{Different measures of accuracy applied to all the
#'   errors.} \item{\code{h_accu}}{Different measures of accuracy applied to all
#'   the errors for every forecasting horizon.}
#'
#' @examples
#' pred <- grnn_forecasting(UKgas, h = 4, lags = 1:4)
#' ro <- rolling_origin(pred)
#' print(ro$global_accu)
#' @export
rolling_origin <- function(grnnf, h = NULL, rolling = TRUE) {
  # Check grnnf parameter
  stopifnot(inherits(grnnf, "grnnForecast"))

  # Check h parameter
  if (is.null(h)) h <- length(grnnf$prediction)
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # Check rolling parameter
  stopifnot(is.logical(rolling), length(rolling) == 1)

  if (rolling) {
    horizons <- seq(h)
  } else {
    horizons <- h
  }
  timeS <- grnnf$orig_timeS
  test_sets <- matrix(NA, nrow = length(horizons), ncol = h)
  predictions <- test_sets
  ind <- nrow(test_sets)
  for (hor in horizons) {
    tt <- train_test(timeS, hor)
    test_sets[ind, 1:hor] <- tt$test
    pred <- grnn_forecasting(tt$training,
                             h = hor,
                             lags = rev(grnnf$model$lags),
                             sigma = grnnf$model$sigma,
                             msas = grnnf$msas
    )
    predictions[ind, 1:hor] <- pred$prediction
    ind <- ind - 1
  }
  colnames(test_sets)   <-  paste("h=", 1:h, sep = "")
  colnames(predictions) <-  paste("h=", 1:h, sep = "")
  errors  <- test_sets - predictions
  g_rmse  <- sqrt(mean(errors ^ 2, na.rm = TRUE))
  g_mae   <- mean(abs(errors), na.rm = TRUE)
  g_mape  <- mean(abs(100*errors/test_sets), na.rm = TRUE)
  g_smape <- mean(abs(test_sets - predictions)/(abs(test_sets)+abs(predictions))*200, na.rm = TRUE)
  global_accu <- c(g_rmse, g_mae, g_mape, g_smape)
  names(global_accu) <- c("RMSE", "MAE", "MAPE", "SMAPE")

  accu <- function(c) {
    rmse  <- sqrt(mean(errors[, c] ^ 2, na.rm = TRUE))
    mae   <- mean(abs(errors[, c]), na.rm = TRUE)
    mape  <- mean(abs(100*errors[, c]/test_sets[, c]), na.rm = TRUE)
    smape <- mean(abs(test_sets[, c] - predictions[, c])/(abs(test_sets[, c])+abs(predictions[, c]))*200, na.rm = TRUE)
    c(rmse, mae, mape, smape)
  }
  h_accu <- sapply(1:h, accu)
  colnames(h_accu) <-  paste("h=", 1:h, sep = "")
  rownames(h_accu) <- c("RMSE", "MAE", "MAPE", "SMAPE")

  structure(
    list(
      grnnf = grnnf,
      test_sets = test_sets,
      predictions = predictions,
      errors = errors,
      global_accu = global_accu,
      h_accu = h_accu
    ),
    class = "grnnForecastRO"
  )
}

#' Plot the prediction for a test set
#'
#' It plots the forecast associated with a test set generated with the function
#' \code{\link{rolling_origin}}.
#'
#' @param x the object obtained from a call to \code{\link{rolling_origin}}.
#'
#' @param h an integer. The forecasting horizon. If \code{NULL} (the default),
#'    the maximum forecasting horizon of all the test sets is used.
#' @param ... Other plotting parameters to affect the plot.
#' @return None
#'
#' @export
plot.grnnForecastRO <- function(x, h = NULL, ...) {
  # Check h parameter
  if (is.null(h))
    h <- ncol(x$test_sets)
  stopifnot(is.numeric(h), length(h) == 1, h >= 1, h <= ncol(x$test_sets))
  if (nrow(x$test_sets) == 1) stopifnot(h == ncol(x$test_sets))

  if (nrow(x$test_sets) == 1) {
    the_row <- 1
  } else {
    the_row <- nrow(x$test_sets) - h + 1
  }

  timeS <- x$grnnf$orig_timeS
  graphics::plot(timeS, type = "o", pch = 20, ylab = "")
  prediction <- timeS
  prediction[1:(length(timeS) - 1)] <- rep(NA, length(timeS) - 1)
  prediction[(length(timeS) - h + 1):length(timeS)] <-
    x$predictions[the_row, 1:h]
  graphics::lines(prediction, col = my_colours("red"))
  graphics::points(prediction, col = my_colours("red"), pch = 20)
}


