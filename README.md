
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Purpose of the package

The goal of `shapleyDecompo` is to compute the Shapley decomposition of
the inequality of an outcome variable across different attributes from
an econometric model.

## Installation instructions

You can install the development version of `shapleyDecompo` from
[GitHub](https://github.com/kevinomics/shapleyDecompo) with:

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

The function `shapleyDecompo()` is wrap function of several functions
used to compute the decomposition.

The function `getFactorList()` is used to extract the list of factors in
the model.

``` r
factors <- getFactorList(
   equation = "outcome",
   model_eco = exTobitModel,
   database = Mroz87)
factors
#> $exper
#> [1] "exper"      "I(exper^2)"
#> 
#> $educ
#> [1] "educ"
#> 
#> $city
#> [1] "city"
```

The function `getCoalitions()` is used to obtain all possible coalitions
from a list of factors.

``` r
coa <- getCoalitions(factors_list = factors)
coa
#>   exper educ city name
#> 1     1    0    0  100
#> 2     0    1    0  010
#> 3     1    1    0  110
#> 4     0    0    1  001
#> 5     1    0    1  101
#> 6     0    1    1  011
#> 7     1    1    1  111
```

The function `getShapleyDistrib()` is used to obtain the model matrix
needed to perform the decomposition.

``` r
distrib <- getShapleyDistrib(model_eco = exTobitModel,
                             database = Mroz87,
                             equation = "outcome")
```

The function `getInequality()` is used to compute the inequality for a
given coalition. Several functions in the package can be used to compute
corresponding measures (see `?Atkinson()`, `?Gini_G()`, `?Gini_w()`,
`?Var()`, `?Entropy()`, `?Kolm()`).

``` r
getInequality(coalition = coa[1, ],
              factors_list = factors,
              model_eco = exTobitModel,
              equation = "outcome",
              measure = Kolm,
              database = Mroz87,
              mXOutcome = distrib,
              theta = 1)
#> [1] 0.05905898
```

The function `getInequality()` can be used in a loop to compute the
inequalities of all coalitions.

``` r
ineq <- vector()
for(i in 1:nrow(coa)){
  ineq[i] <- getInequality(coalition = coa[i, ],
              factors_list = factors,
              model_eco = exTobitModel,
              equation = "outcome",
              measure = Kolm,
              database = Mroz87,
              mXOutcome = distrib,
              theta = 1)
}
ineq
#> [1] 0.0590589800 0.0134484195 1.4306429142 0.0005982804 0.1186432101
#> [6] 0.0134351911 1.4172637305
```

The function `getMarginalContrib()` can be used to compute the marginal
contributions of each attribute in all coalitions.

``` r
margContrib <- getMarginalContrib(inequality = ineq,
                   coalitions = coa,
                   nVar = length(factors),
                   wMC = TRUE)
margContrib
#>        exper        educ          city         ineq
#> 1 0.01968633 0.000000000  0.000000e+00 0.0590589800
#> 2 0.00000000 0.004482806  0.000000e+00 0.0134484195
#> 3 0.23619908 0.228597322  0.000000e+00 1.4306429142
#> 4 0.00000000 0.000000000  1.994268e-04 0.0005982804
#> 5 0.01967415 0.000000000  9.930705e-03 0.1186432101
#> 6 0.00000000 0.002139485 -2.204738e-06 0.0134351911
#> 7 0.46794285 0.432873507 -4.459728e-03 1.4172637305
```

The function `getShapleyVal()` can be used to compute the Shapley values
from marginal contributions of attributes.

``` r
getShapleyVal(marginalContrib = margContrib)
#>                exper      educ        city     ineq
#> shapley    0.7435024 0.6680931 0.005668199 1.417264
#> shapleyRel 0.5246041 0.4713965 0.003999396 1.000000
```

The function `getInteractionTerms()` can be used to extract the
interaction terms from the marginal contributions of attributes in each
coalition.

``` r
getInteractionTerms(factors_list = factors,
                    coalitions = coa,
                    marginalContrib = margContrib)
#> $INTERACTIONS
#>             exper        educ         city
#> exper 1.403828539  0.65495046  0.005375673
#> educ  0.654950456  1.29862052 -0.024423056
#> city  0.005375673 -0.02442306 -0.013379184
#> 
#> $INTERACTIONS_r
#>             exper        educ         city
#> exper 0.990520331  0.46212320  0.003792994
#> educ  0.462123204  0.91628713 -0.017232542
#> city  0.003792994 -0.01723254 -0.009440151
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
