
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
#> Warning: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame'
#> when loading 'dplyr'
pred <- grnn_forecasting(USAccDeaths, h = 12)
pred$prediction # To see a time series with the forecasts
#>            Jan       Feb       Mar       Apr       May       Jun       Jul
#> 1979  8141.528  7216.485  8123.807  8432.028  9466.795  9829.624 10958.570
#>            Aug       Sep       Oct       Nov       Dec
#> 1979 10246.196  9490.048  9439.096  8994.771  9615.380
plot(pred)      # To see a plot with the forecast
```

<img src="man/figures/README-example-1.png" width="100%" />

To know more, read the package’s vignette.

## Acknowledgements

Funds: This work was partially supported by the project TIN2015-68854-R
(FEDER Founds) of the Spanish Ministry of Economy and Competitiveness.
