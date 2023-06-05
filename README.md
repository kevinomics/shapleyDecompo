
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
               equaGame = FALSE,
               correction = NA,
               data_weights = rep(1, nrow(Mroz87)),
               residuals = FALSE,
               transfo = NULL,
               measure = Gini_w,
               theta = NULL)
#> $IMPORTANCE
#>                exper      educ        city      ineq
#> shapley    0.3265863 0.3492574 0.005118859 0.6809626
#> shapleyRel 0.4795951 0.5128878 0.007517092 1.0000000
#> 
#> $INTERACTIONS
#>             exper       educ        city
#> exper 0.693653169 0.36084139 0.006225495
#> educ  0.360841390 0.72285118 0.012752363
#> city  0.006225495 0.01275236 0.024096716
#> 
#> $INTERACTIONS_r
#>             exper       educ        city
#> exper 1.018636271 0.52989901 0.009142199
#> educ  0.529899011 1.06151382 0.018726966
#> city  0.009142199 0.01872697 0.035386257
```

The function `shapleyDecompo()` is wrap function of several functions
used to compute the decomposition:

1.  The function `getFactorList()` is used to extract the list of
    factors in the model.

``` r
factors <- getFactorList(
   equation = "outcome",
   model_eco = exTobitModel,
   database = Mroz87,
   residuals = FALSE)
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

2.  The function `getCoalitions()` is used to obtain all possible
    coalitions from a list of factors.

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

3.  The function `getShapleyDistrib()` is used to obtain the model
    matrix needed to perform the decomposition.

``` r
distrib <- getShapleyDistrib(model_eco = exTobitModel,
                             database = Mroz87,
                             equation = "outcome")
```

4.  The function `getInequality()` is used to compute the inequality for
    a given coalition. Several functions in the package can be used to
    compute corresponding measures (see `?Atkinson()`, `?Gini_G()`,
    `?Gini_w()`, `?Var()`, `?Entropy()`, `?Kolm()`).

``` r
getInequality(coalition = coa[1, ],
              factors_list = factors,
              model_eco = exTobitModel,
              equaGame = FALSE,
              errors = NA,
              equation = "outcome",
              measure = Gini_w,
              database = Mroz87,
              mXOutcome = distrib,
              transfo = NULL,
              correction = NA,
              theta = NULL,
              weights = rep(1, nrow(Mroz87)))
#> [1] -0.02883296
```

The function `getInequality()` can be used in a loop to compute the
inequalities of all coalitions.

``` r
ineq <- vector()
for(i in 1:nrow(coa)){
  ineq[i] <- getInequality(coalition = coa[i, ],
              factors_list = factors,
              model_eco = exTobitModel,
              equaGame = FALSE,
              errors = NA,
              equation = "outcome",
              measure = Gini_w,
              database = Mroz87,
              mXOutcome = distrib,
              transfo = NULL,
              correction = NA,
              theta = NULL,
              weights = rep(1, nrow(Mroz87)))
}
ineq
#> [1] -0.028832965 -0.012688692  0.656865849 -0.002211362 -0.041888611
#> [6] -0.012690603  0.680962566
```

5.  The function `getMarginalContrib()` can be used to compute the
    marginal contributions of each attribute in all coalitions.

``` r
margContrib <- getMarginalContrib(inequality = ineq,
                   coalitions = coa,
                   nVar = length(factors),
                   wMC = TRUE)
margContrib
#>          exper         educ          city         ineq
#> 1 -0.009610988  0.000000000  0.000000e+00 -0.028832965
#> 2  0.000000000 -0.004229564  0.000000e+00 -0.012688692
#> 3  0.111592424  0.114283136  0.000000e+00  0.656865849
#> 4  0.000000000  0.000000000 -7.371206e-04 -0.002211362
#> 5 -0.006612875  0.000000000 -2.175941e-03 -0.041888611
#> 6  0.000000000 -0.001746540 -3.185278e-07 -0.012690603
#> 7  0.231217723  0.240950392  8.032239e-03  0.680962566
```

6.  The function `getShapleyVal()` can be used to compute the Shapley
    values from marginal contributions of attributes.

``` r
getShapleyVal(marginalContrib = margContrib)
#>                exper      educ        city      ineq
#> shapley    0.3265863 0.3492574 0.005118859 0.6809626
#> shapleyRel 0.4795951 0.5128878 0.007517092 1.0000000
```

7.  The function `getInteractionTerms()` can be used to extract the
    interaction terms from the marginal contributions of attributes in
    each coalition.

``` r
getInteractionTerms(factors_list = factors,
                    coalitions = coa,
                    marginalContrib = margContrib)
#> $INTERACTIONS
#>             exper       educ        city
#> exper 0.693653169 0.36084139 0.006225495
#> educ  0.360841390 0.72285118 0.012752363
#> city  0.006225495 0.01275236 0.024096716
#> 
#> $INTERACTIONS_r
#>             exper       educ        city
#> exper 1.018636271 0.52989901 0.009142199
#> educ  0.529899011 1.06151382 0.018726966
#> city  0.009142199 0.01872697 0.035386257
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
