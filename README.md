
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Purpose of the package

The goal of `shapleyDecompo` is to compute the Shapley decomposition of
the inequality of an outcome variable across different attributes from
an econometric model.

## Installation instructions

You can install the development version of `shapleyDecompo` from
[GitHub](https://github.com/) with:

``` r
# from CRAN
install.packages("devRate")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("kevinomics/shapleyDecompo")
```

## Overview

The package can be used to compute the Shapley decomposition from a GLM
(see `?stats::glm()`), or a Tobit II model obtained with the
`sampleSelection` package (see `?sampleSelection::selection()`). The
package provides with a function called `shapleyDecompo()` which can be
used to compute the Shapley decomposition.

``` r
library("shapleyDecompo")
```

``` r
data(Mroz87)
exTobitModel <- sampleSelection::selection(
  lfp ~ age + I(age^2) + faminc + kids5 + educ,
  wage ~ exper + I(exper^2) + educ + city,
  data = Mroz87)
shapleyDecompo(database = Mroz87,
               model_eco = exTobitModel,
               equation = "outcome",
               equaGame = TRUE,
               correction = NA,
               data_weights = rep(1, nrow(Mroz87)),
               residuals = TRUE,
               transfo = NULL,
               measure = Gini_w,
               theta = NULL)
#> $IMPORTANCE
#>                exper      educ       city Residuals      ineq
#> shapley    0.1601363 0.1646356 -0.0991494 0.1351867 0.3608091
#> shapleyRel 0.4438255 0.4562955 -0.2747974 0.3746764 1.0000000
#> 
#> $INTERACTIONS
#>                  exper       educ          city   Residuals
#> exper      0.461500220 0.25692027 -0.0067629221  0.05120659
#> educ       0.256920271 0.66178250  0.1866944840  0.05353218
#> city      -0.006762922 0.18669448 -0.0006002239 -0.08138238
#> Residuals  0.051206594 0.05353218 -0.0813823840  0.15854305
#> 
#> $INTERACTIONS_r
#>                 exper      educ        city  Residuals
#> exper      1.27907033 0.7120670 -0.01874377  0.1419216
#> educ       0.71206704 1.8341624  0.51743285  0.1483670
#> city      -0.01874377 0.5174329 -0.00166355 -0.2255552
#> Residuals  0.14192157 0.1483670 -0.22555524  0.4394098
```

## Citation

To cite this package please use:

``` r
citation("shapleyDecompo") 
#> To cite package 'shapleyDecompo' in publications use:
#> 
#>   Fourrey K, Regnier B (2023). _shapleyDecompo: Compute a
#>   Regression-Based Shapley Decomposition for Inequality Measures_. R
#>   package version 0.0.0.9000,
#>   <https://github.com/kevinomics/shapleyDecompo/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {shapleyDecompo: Compute a Regression-Based Shapley Decomposition for Inequality Measures},
#>     author = {Kevin Fourrey and Baptiste Regnier},
#>     year = {2023},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://github.com/kevinomics/shapleyDecompo/},
#>   }
```

and/or: Fourrey, K. (2023). A Regression-Based Shapley Decomposition for
Inequality Measures. Annals of Economics and Statistics, 149, 39–62.
<https://doi.org/10.2307/48718079>
