
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Purpose of the package

The goal of `shapleyDecompo` is to compute the Shapley decomposition of
the inequality of an outcome variable across different attributes from
an econometric model.

## Installation instructions

You can install the development version of shapleyDecompo from
[GitHub](https://github.com/) with:

``` r
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
shapleyDecompo(database = exData,
               model_eco = exTobitModel,
               equation = "outcome",
               equaGame = TRUE,
               correction = NA,
               data_weights = exData$extridf,
               residuals = TRUE,
               transfo = exp,
               measure = Atkinson,
               theta = 1)
#> $IMPORTANCE
#>                  SEXE  POT_EXP  Residuals       ineq
#> shapley    -0.4665829 0.486386 0.07776029 0.09756347
#> shapleyRel -4.7823522 4.985330 0.79702261 1.00000000
#> 
#> $INTERACTIONS
#>                 SEXE      POT_EXP   Residuals
#> SEXE      -0.9024365 -0.467023958  0.03117030
#> POT_EXP   -0.4670240  0.003769765 -0.01559232
#> Residuals  0.0311703 -0.015592320  0.09333827
#> 
#> $INTERACTIONS_r
#>                 SEXE    POT_EXP  Residuals
#> SEXE      -9.2497381 -4.7868733  0.3194874
#> POT_EXP   -4.7868733  0.0386391 -0.1598172
#> Residuals  0.3194874 -0.1598172  0.9566928
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
