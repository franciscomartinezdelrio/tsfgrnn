
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsfgrnn

<!-- badges: start -->

<!-- badges: end -->

The goal of tsfgrnn is to forecast time series using GRNN regression.

## Installation

You can install the released version of tsfgrnn from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tsfgrnn")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("franciscomartinezdelrio/tsfgrnn")
```

## Example

This is a basic example which shows how to forecast with tsfgrnn:

``` r
library(tsfgrnn)
pred <- grnn_forecasting(USAccDeaths, h = 12)
pred$prediction # To see a time series with the forecasts
#>            Jan       Feb       Mar       Apr       May       Jun       Jul
#> 1979  8156.514  7303.029  8123.393  7870.155  9386.960  9555.996 10093.013
#>            Aug       Sep       Oct       Nov       Dec
#> 1979  9620.001  8285.021  8466.004  8160.001  8034.015
plot(pred) # To see a plot with the forecast
```

<img src="man/figures/README-example-1.png" width="100%" />

To know more, read the package’s vignette.

## Acknowledgements

Funds: This work was partially supported by the project TIN2015-68854-R
(FEDER Founds) of the Spanish Ministry of Economy and Competitiveness.
