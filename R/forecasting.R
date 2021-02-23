#' Time series forecasting using GRNN regression
#'
#' It applies GRNN regression to forecast the future values of a time series.
#' The lags used as autoregressive variables are set with the \code{lags}
#' parameter. If the user does not set the lags, these values are selected
#' automatically.
#'
#' @param timeS A numeric vector or time series of class \code{ts}.
#' @param h A positive integer. Number of periods for forecasting.
#' @param lags An integer vector in increasing order expressing the lags used as
#'   autoregressive variables.
#' @param sigma A positive real value or a character value. The smoothing
#'   parameter in GRNN regression. Two character values are possible, "ROLLING"
#'   (the default) and "FIXED", in which case the parameter is chosen using an
#'   optimization tool with rolling origin evaluation or fixed origin
#'   evaluation.
#' @param msas A string indicating the Multiple-Step Ahead Strategy used when
#'   more than one value is predicted. It can be "MIMO" or "recursive" (the
#'   default).
#' @param scale A logical. If \code{TRUE} (the default), the time series is
#'   scaled to the range [0, 1].
#' @return An object of class \code{"grnnForecast"}. The function
#'   \code{\link[base]{summary}} can be used to obtain or print a summary of the
#'   results. An object of class \code{"gnnForecast"} is a list containing at
#'   least the following components:
#'
#'   \item{\code{call}}{the matched call.} \item{\code{msas}}{the Multi-Step
#'   Ahead Strategy.} \item{\code{prediction}}{a time series with the forecast.}
#'   \item{\code{model}}{an object of class \code{"grnnModel"} with the GRNN
#'   model}
#'
#' @examples
#' pred <- grnn_forecasting(USAccDeaths, h = 12, lags = 1:12)
#' plot(pred)
#' @export
grnn_forecasting <- function(timeS, h, lags = NULL, sigma = "ROLLING",
                            msas = c("recursive", "MIMO"),
                            scale = TRUE) {
  # Check timeS parameter
  stopifnot(stats::is.ts(timeS) || is.vector(timeS, mode = "numeric"))
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)
  orig_timeS <- timeS

  prepro <- list()
  # Check scale parameter
  stopifnot(is.logical(scale), length(scale) == 1)
  if (scale) {
    minimum <- min(orig_timeS)
    maximum <- max(orig_timeS)
    pre_timeS <- (orig_timeS - minimum) / (maximum - minimum)
    prepro$scale <- list(scale = TRUE, minimum = minimum, maximum = maximum)
  } else {
    prepro$scale <- list(scale = FALSE)
    pre_timeS <- orig_timeS
  }

  # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  # msas parameter
  msas <- match.arg(msas)

  # Check lags parameter
  stopifnot(is.null(lags) || is.vector(lags, mode = "numeric"))
  if (is.null(lags)) {
    if (stats::frequency(timeS) > 1) {
      lags <- 1:stats::frequency(timeS)
    } else {
      partial <- stats::pacf(timeS, plot = FALSE)
      lags <- which(partial$acf > 2/ sqrt(length(timeS)))
      if (length(lags) == 0) {
        lags = 1:5
      }
    }
  }

  if (is.unsorted(lags)) stop("lags should be a vector in increasing order")
  stopifnot(lags[1] >= 1)

  # Check sigma parameter
  stopifnot(is.numeric(sigma) && sigma > 0 ||
              is.character(sigma) && sigma %in% c("ROLLING", "FIXED"))
  if (is.character(sigma)) {
    f <- grnn_forecasting(pre_timeS, h = h, lags = lags, sigma = 3, msas = msas, scale = FALSE)
    opt <- function(sigmav) {
      f$model$sigma <- sigmav
      r <- rolling_origin(f, rolling = sigma == "ROLLING")
      r$global_accu["RMSE"]
    }
    x <- stats::optim(3, opt, method = "Brent", lower = 0, upper = 1e6)
    sigma <- x$par
    # mini = -1
    # for (x in seq(0.1, 1.2, by = 0.1)) {
    #   f$model$sigma <- x
    #   r <- rolling_origin(f)
    #   if (mini == -1 || r$global_accu["RMSE"] < mini) {
    #     sigma <- x
    #     mini <- r$global_accu["RMSE"]
    #   }
    # }
    print(sigma)
  }
  stopifnot(is.numeric(sigma))
  if (sigma[1] < 0) stop("sigma should be positive")

  if (msas == "recursive") {
    fit <- grnn_model(pre_timeS, lags = lags, sigma = sigma, nt = 1)
  } else { # MIMO
    fit <- grnn_model(pre_timeS, lags = lags, sigma = sigma, nt = h)
  }
  r <- structure(
    list(
      call = match.call(),
      model = fit,
      msas = msas,
      prepro = prepro,
      orig_timeS = orig_timeS
    ),
    class = "grnnForecast"
  )
  predict(r, h)
}

#' Predict method for GRNN models for time series forecasting.
#'
#' Predicted values based on a GRNN model for time series forecasting.
#'
#' If the models uses the MIMO strategy for multiple-step ahead prediction,
#' the forecasting horizon is fixed to the model forecasting horizon.
#'
#' @param object a \code{grnnForecast} object obtained by a call to the
#'    \code{\link{grnn_forecasting}} function.
#' @param h an integer. The forecasting horizon.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a \code{grnnForecast} object with the prediction and information
#' about the GRNN model, see the documentation of \code{\link{grnn_forecasting}}
#' for the structure of \code{grnnForecast} objects.
#'
#' @examples
#' pred <- grnn_forecasting(UKgas, h = 4, msas = "MIMO")
#' new_pred <- predict(pred, h = 4)
#' print(new_pred$prediction)
#' plot(new_pred) # To see a plot with the forecast
#'
#' @importFrom stats predict
#' @export
predict.grnnForecast <- function(object, h, ...) {
  # Check h parameter
  stopifnot(is.numeric(h), length(h) == 1, h >= 1)

  if (object$msas == "recursive") {
    ts <- object$model$ts
    reg <- recPrediction(object$model, h = h)
  } else {# MIMO
    ts <- object$model$ts
    hor = ncol(object$model$examples$targets)
    if (h != hor)
      stop(paste("The model only predicts horizon", hor))
    example <- as.vector(ts[(length(ts) + 1) - object$model$lags])
    reg <- regression(object$model, example)
  }
  # else { # direct
  #   ts <- object$model[[1]]$model$ts
  #   hor = length(object$model)
  #   if (h > hor)
  #     stop(paste("The model predicts horizon", hor, "at most"))
  #   prediction <- numeric(h)
  #   weights <- list()
  #   for (hor in 1:h) {
  #     example <- ts[(length(ts) + hor) - object$model[[hor]]$model$lags]
  #     reg <- regression(object$model[[hor]]$model, example)
  #     prediction[hor] <- reg$prediction
  #     weights[[hor]] <- reg$weights
  #   }
  #   reg <- list(prediction = prediction, weights = weights)
  # }
  temp <- stats::ts(1:2,
                    start = stats::end(ts),
                    frequency = stats::frequency(ts)
  )
  prediction <- stats::ts(reg$prediction,
                          start = stats::end(temp),
                          frequency = stats::frequency(ts)
  )
  if (object$prepro$scale$scale) {
    minimum <- object$prepro$scale$minimum
    maximum <- object$prepro$scale$maximum
    orig_prediction <- prediction * (maximum - minimum) + minimum
  } else {
    orig_prediction <- prediction
  }

  r <- object
  r$prediction = orig_prediction
  r$pre_prediction = prediction
  r$weights = reg$weights
  r
}

recPrediction <- function(model, h) {
  prediction <- numeric(h)
  weights <- matrix(nrow = h, ncol = nrow(model$examples$patterns))
  values <- as.vector(model$ts)
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - model$lags]
    reg <- regression(model, example)
    prediction[hor] <- reg$prediction
    weights[hor, ] <- reg$weights
    values <- c(values, prediction[hor])
  }
  return(list(
    prediction = prediction,
    weights = weights
  ))
}

#' Training examples and their corresponding weights used in a prediction
#'
#' It shows the input vector and the weights of the training examples used in a
#' prediction associated with a "grnnForecast" object.
#'
#' @param forecast A \code{grnnForecast} object.
#' @return A list including the input vectors used in GRNN regression and the
#'   training examples, with their weights, used in the prediction.
#'
#' @examples
#' pred <- grnn_forecasting(UKgas, h = 4, lags = 1:4, msas = "MIMO")
#' grnn_weights(pred)
#' @export
grnn_weights <- function(forecast) {
  stopifnot(inherits(forecast, "grnnForecast"))

  if (forecast$msas == "recursive") {
    return(examples_recursive(forecast))
  } else {
    timeS <- forecast$model$ts
    example <- timeS[length(timeS) + 1 - forecast$model$lags]
    names(example) <- paste("Lag", forecast$model$lags)
    n <- c(colnames(forecast$model$examples$patterns),
           colnames(forecast$model$examples$targets), "weight")
    examples = cbind(forecast$model$examples$patterns,
                     forecast$model$examples$targets,
                     forecast$weights)
    colnames(examples) <- n
    return(list(
      input = example,
      examples = examples
    ))
  }
}

examples_recursive <- function(forecast) {
  result <- list()
  timeS <- forecast$model$ts
  temp <- c(timeS, forecast$prediction)
  n <- c(colnames(forecast$model$examples$patterns),
         colnames(forecast$model$examples$targets), "weight")
  examples = cbind(forecast$model$examples$patterns,
                   forecast$model$examples$targets,
                   forecast$weights[1, ])
  colnames(examples) <- n
  for (h in 1:nrow(forecast$weights)){
    # extract the example
    example <- temp[length(timeS) + h - forecast$model$lags]
    names(example) <- paste("Lag", forecast$model$lags)
    examples[, "weight"] <- forecast$weights[h, ]

    result[[h]] <- list(
      input = example,
      examples = examples
    )
  }
  return(result)
}

#' Examples of a GRNN model
#'
#' It shows the examples of the model associated to a
#' \code{grnnForecast} object.
#'
#' @param forecast A \code{grnnForecast} object.
#' @return A matrix including the features and targets of the examples
#'    associated with the model of a \code{grnnForecast} object.
#'
#' @examples
#' pred <- grnn_forecasting(ts(1:8), h = 1, lags = 1:2)
#' grnn_examples(pred)
#' @export
grnn_examples <- function(forecast) {
  stopifnot(inherits(forecast, "grnnForecast"))
  cbind(forecast$model$examples$patterns, forecast$model$examples$targets)
}

# direct <- function(timeS, h, lags = NULL, sigma = NULL,
#                    msas = c("MIMO", "recursive"),
#                    scale = TRUE) {
#   if (is.null(lags))
#     lags <- list(1:5)
#   stopifnot(is.list(lags) || is.integer(lags) || is.double(lags))
#   if (!is.list(lags))
#     lags <- list(lags)
#   lapply(seq_along(lags),
#          function(ind) stopifnot(is.integer(lags[[ind]]) || is.double(lags[[ind]]))
#   )
#   stopifnot(length(lags) == h || length(lags) == 1)
#   if (length(lags) == 1)
#     lags <- lapply(1:h, function(n) n - 1 + lags[[1]])
#   models <- list()
#   for (ind in seq(lags)) {
#     models[[ind]] <- grnn_forecasting(timeS = timeS,
#                                       h = 1,
#                                       lags = lags[[ind]],
#                                       sigma = sigma,
#                                       msas = "MIMO",
#                                       scale = scale)
#   }
#   r <- structure(
#     list(
#       call = match.call(),
#       model = models,
#       msas = "direct",
#       prepro = models[[1]]$prepro,
#       orig_timeS = timeS
#     ),
#     class = "grnnForecast"
#   )
#   r
# }
