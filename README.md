
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rabc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rabc is to facilitate the development of animal behaviour
classification models using accelerometer data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YuHuiDeakin/rabc", build_vignette = TRUE)
# It will take several minutes to install the package because of vignette building. 
```

## Workflow of the package

Here is the function workflow of the package:

![](rabc%20Diagram.jpeg)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rabc)
## basic example code

## order data by behaviour labels 
whitestork_acc_sorted <- order_acc(whitestork_acc)

## calculate ACC data into time domain features
df_time <- calculate_feature_time(whitestork_acc_sorted, winlen_dba = 11)
#> Warning in calculate_feature_time(whitestork_acc_sorted, winlen_dba = 11):
#> Suggestion: transform raw data into 1g = 9.8 m/s2 for consistency

## use time domain features to train a model and output results
predictions <- plot_confusion_matrix(df_time, vec_label = whitestork_acc_sorted$V121)
```

<img src="man/figures/README-example-1.png" width="100%" />
