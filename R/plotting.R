my_colours <- function(name) {
  col_l <- list("blue" = "#000099",
                "red" = "#CC0000",
                "green" = "#339900",
                "orange" = "#CC79A7"
  )
  return(col_l[[name]])
}

#' @importFrom graphics plot
#' @export
plot.grnnForecast <- function(x, y, ...) {
  timeS <- combine(x$orig_timeS, x$prediction)
  graphics::plot(timeS, type = "n", ylab = "")
  graphics::lines(x$orig_timeS, type = "o", pch = 20)
  graphics::lines(x$prediction, type = "o",
                  col = my_colours("red"),
                  pch = 20)
}

#' Create a ggplot object from a grnnForecast object
#'
#' It uses an object of class `grnnForecast` to create a `ggplot` object that
#' plots a time series and its forecast using GRNN regression.
#'
#' @details Commonly used parameters are:
#'
#' * `highlight`. A character string indicating what elements should be highlighted. Possible values are
#'   `"none"` and `"points"`. The default value is `"none"`.
#'
#' @param object An object of class `grnnForecast`.
#' @param ... additional parameter, see details.
#'
#' @return The `ggplot` object representing a plotting with the forecast.
#'
#' @examples
#' pred <- grnn_forecasting(USAccDeaths, h = 12, lags = 1:12, sigma = 50)
#' library(ggplot2)
#' autoplot(pred)
#' @export
#' @importFrom ggplot2 autoplot
autoplot.grnnForecast <- function(object, ...) {
  # check ... parameter
  l <- list(...)
  if (length(l) > 0) {
    valid_n <- c("highlight") # valid parameter names, apart from object
    if(! all(names(l) %in% valid_n))
      stop(paste0("Parameters ", setdiff(names(l), valid_n), " not supported"))
    if ("highlight" %in% names(l) && (!is.character(l$highlight) || length(l$highlight) > 1)) {
      stop("highlight parameter should be a character string of length 1")
    if (! (l$highligth %in% c("none", "points")))
      stop("Possible values of highlight parameter are 'none' and 'points'")
    }
  }

  forecast <- object
  highlight <- if ("highlight" %in% names(l)) l$highlight else "none"

  # extract the time series
  timeS <- data.frame(
    x = as.vector(stats::time(forecast$orig_timeS)),
    y = as.vector(forecast$orig_timeS)
  )

  # extract the forecast
  pred <- data.frame(
    x = as.vector(stats::time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )

  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Original"))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Forecast"), data = pred)
  if (highlight == "points") {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Original"))
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast"), data = pred)
  }
  breaks <- c("Original", "Forecast")
  colours <- c("Original" = "black", "Forecast" = my_colours("red"))
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p <- p + ggplot2::labs(x = "Time", y = NULL, colour = "Time series")
  p
}

#' Plot an example used in a prediction of a grnnForecast object
#'
#' This function is useful to see how the forecast has been computed. An ordinal
#' specifying the order of the weight has to be supplied and the function plots
#' the training pattern associated with that ordinal.
#'
#' @param forecast The grnnForecast object.
#' @param position An integer. It is an ordinal number indicating what training
#'   pattern to plot. For instance, if \code{position} is 1 it means that the
#'   training pattern with the greatest weight should be plotted. If
#'   \code{position} is 2 the training pattern with the second greatest weight
#'   is plotted and so on.
#' @param h An integer. This value is only useful when the recursive strategy is
#'   being used. It indicates the forecasting horizon
#' @return A ggplot object representing an example used in the prediction.
#'
#' @examples
#' pred <- grnn_forecasting(USAccDeaths, h = 12, lags = 1:12, sigma = 50)
#' library(ggplot2)
#' plot_example(pred, 1)
#' @export
plot_example <- function(forecast, position, h = 1) {

  # Check position parameter
  stopifnot(is.numeric(position), length(position) == 1, position >= 1)
  if (position > nrow(forecast$model$examples$patterns))
    stop(paste("There are only", nrow(forecast$model$examples$patterns),
               "training patterns"))

  # extract the time series
  timeS <- data.frame(
    x = as.vector(stats::time(forecast$orig_timeS)),
    y = as.vector(forecast$orig_timeS)
  )
  # extract the forecast
  pred <- data.frame(
    x = as.vector(stats::time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )

  if (forecast$msas == "recursive") {
    return(plot_example_recursive(forecast, timeS, pred, position, h))
  } else {
    return(plot_example_MIMO(forecast, timeS, pred, position))
  }
}

plot_example_MIMO <- function(forecast, timeS, pred, position) {
  # extract the instance
  instance <- timeS[nrow(timeS) + 1 - forecast$model$lags, ]

  # extract the example
  value <- order(forecast$weights, decreasing = TRUE)[position]
  example <- timeS[value + rev(forecast$model$lags) - 1, ]

  # extract the target
  h <- ncol(forecast$model$examples$targets)
  target <- timeS[value + forecast$model$lags[1] -1 + 1:h, ]
  plot_the_example(forecast, timeS, pred, pred, instance, example, target)
}

plot_example_recursive <- function(forecast, timeS, pred, position, h) {
  # extract the instance
  temp <- rbind(timeS, pred)
  instance <- temp[nrow(timeS) + h - forecast$model$lags, ]

  # extract the example
  value <- order(forecast$weights[h, ], decreasing = TRUE)[position]
  example <- timeS[value + rev(forecast$model$lags) - 1, ]

  # extract the target
  target <- timeS[value + forecast$model$lags[1], ]
  plot_the_example(forecast, timeS, pred, pred[h, ], instance, example, target)
}

plot_the_example <- function(forecast, timeS, pred, pred2, instance, example, target) {

  # plot the time series
  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line()

  # plot the forecast
  if (nrow(pred) > 1)
    p <- p + ggplot2::geom_line(data = pred, colour = my_colours("red"))
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast",
                                            shape = "Forecast"), data = pred2)
  # plot the instance
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Input",
                                            shape = "Input"),
                               data = instance,
                               size = 2
  )

  # plot the example
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Training pattern",
                                            shape = "Training pattern"),
                               data = example,
                               size = 2
  )

  # plot the target
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Training target",
                                            shape = "Training target"),
                               data = target,
                               size = 2
  )

  shapes <- c("Training pattern" = 1, "Training target" = 0, "Input" = 18,
              "Forecast" = 16)
  breaks <- c("Training pattern", "Training target", "Input", "Forecast")
  p <- p + ggplot2::scale_shape_manual(values = shapes, breaks = breaks)
  colours <- c("Training pattern" = my_colours("blue"),
               "Training target" = my_colours("green"),
               "Input" = my_colours("orange"),
               "Forecast" = my_colours("red")
  )
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  g <- ggplot2::guide_legend("Data point")
  p <- p + ggplot2::guides(colour = g, shape = g)
  p <- p + ggplot2::labs(x = "Time", y = NULL)
  p
}
