
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spiro <img src="man/figures/spiro_logo.png" align="right" width = "160" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/spiro)](https://CRAN.R-project.org/package=spiro)
<!-- badges: end -->

## Overview

The goal of `spiro` is to enable a fast and standardised workflow with
raw data files from cardiopulmonary exercise testing. The package
provides simple tools for data import, processing, summary and
visualisation.

> WARNING: This package is currently under early development. It is
> recommended to use this software for demonstrational or testing
> purposes only. We advise against using it for scientific analyses at
> the moment, as at this stage of development major changes in the
> package’s computations may occur.

## Installation

Install the current development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("smnnlt/spiro")
```

## Usage

-   Use `spiro()` for one-step data import and processing from raw
    metabolic cart files.
-   Use `spiro_summary()` and `spiro_glance()` for a stepwise or overall
    summary of the imported data.
-   Use `spiro_plot_*()` as a shortcut for visualising the imported
    data.

#### Metabolic Carts

The following metabolic carts are currently supported by `spiro`:

-   CORTEX
-   COSMED
-   ZAN

## Example

``` r
library(spiro)

# get data for example
file <- spiro_example("zan_gxt")

gxt_data <- spiro(file)
spiro_summary(gxt_data)
#> for pre-measures, interval was set to length of measures
#>    step_number load     VE     VO2    VCO2  RER VO2_rel     RE  CHO   FO
#> 1            0  0.0  13.03  500.19  411.74 0.82    7.58     NA 0.27 0.15
#> 2            1  2.0  39.87 1860.92 1585.75 0.85   28.20 234.97 1.27 0.46
#> 3            2  2.4  44.63 2097.82 1805.27 0.86   31.79 220.73 1.51 0.49
#> 4            3  2.8  52.63 2413.01 2122.17 0.88   36.56 217.62 1.95 0.48
#> 5            4  3.2  57.19 2710.68 2319.93 0.86   41.07 213.91 1.89 0.65
#> 6            5  3.6  67.45 3048.75 2684.87 0.88   46.19 213.86 2.47 0.60
#> 7            6  4.0  75.91 3404.02 3026.70 0.89   51.58 214.90 2.90 0.62
#> 8            7  4.4  88.36 3724.37 3383.64 0.91   56.43 213.75 3.50 0.56
#> 9            8  4.8 106.44 4223.82 3993.55 0.95   64.00 222.21 4.68 0.37
#> 10           9  5.2 127.54 4573.91 4488.36 0.98   69.30 222.12 5.82 0.12

spiro_plot_VO2(gxt_data)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contributing

If you encounter any bug or want to request new features please, please
[submit an issue](https://github.com/smnnlt/spiro/issues). If you want
your type of metabolic cart to be supported by this package, consider
writing me a message with an example data file. If you are familiar with
writing R code, feel free to submit any pull requests.

Please note that this package is released with a [Contributor Code of
Conduct](https://smnnlt.github.io/spiro/CODE_OF_CONDUCT.html). By
contributing to `spiro` project, you agree to abide by its terms.
