#' @export
print.grnnForecast <- function (x, ...) {
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Multiple-Step Ahead Strategy:", x$msas, "\n")
  cat("Sigma (smoothing parameter):", x$model$sigma, "\n")
  cat("Autoregressive lags:", rev(x$model$lags), "\n")
  cat("Number of examples:", nrow(x$model$examples$patterns), "\n")
  invisible(x)
}

#' @export
summary.grnnForecast <- function (object, ...) {
  structure(
    list(
      call = object$call,
      sigma = object$model$sigma,
      msas = object$msas,
      lags = rev(object$model$lags),
      prediction = object$prediction,
      prepro = object$prepro
    ),
    class = "summary.grnnForecast"
  )
}

#' @export
print.summary.grnnForecast <- function (x, ...) {
  stopifnot(inherits(x, "summary.grnnForecast"))
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Multiple-Step Ahead Strategy:", x$msas, "\n")
  cat("Sigma (smoothing parameter):", x$sigma, "\n")
  cat("Autoregressive lags:", x$lags, "\n")
  cat("Forecasting horizon:", length(x$prediction), "\n")
  cat("Forecast:\n")
  print(x$prediction)
  cat("Preprocessing:\n")
  if (x$prepro$scale$scale) {
    cat("   The time series has been scaled to range [0, 1]\n")
  } else {
    cat("   The time series has not been scaled\n")
  }
  invisible(x)
}

