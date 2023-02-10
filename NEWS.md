## tsfgrnn 1.0.3

* citation information has been added

## tsfgrnn 1.0.2

* References to the paper describing this package have been added in the documentation

## tsfgrnn 1.0.1

* The default transformation is now additive instead of multiplicative.

## tsfgrnn 1.0.0

* Automatic computation of smoothing parameter can be done with rolling origin or fixed origin (less accurate, but faster).

* Lags can be selected using forward selection or backward elimination (feature selection techniques)

* Time series cannot be scaled

* The model is built using faster Rcpp code

* An optional transformation to the training samples has been added. It improves forecast accuracy for time series with a trend

## tsfgrnn 0.2.0

* Uses Rcpp for speeding up computation of forecasts.


## tsfgrnn 0.1.0

* This is the first release of this package.
