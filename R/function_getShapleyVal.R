#' Compute the Shapley values from marginal contributions of attributes.
#'
#' @param marginalContrib A data.frame wit marginal contributions.
#'
#' @return A data.frame with Shapley value for each attribute and relative value.
#' @export
#'
#' @examples
#'factors <- getFactorList(
#'  equation = "outcome",
#'  model_eco = exTobitModel,
#'  database = exData,
#'  residuals = FALSE)
#'coa <- getCoalitions(factors_list = factors)
#'distrib <- getShapleyDistrib(
#'  model_eco = exTobitModel,
#'  equation = "outcome",
#'  database = exData)
#'ineq <- vector()
#'for(i in 1:3){
#'  ineq[i] <- getInequality(
#'    coalition = coa[i, ],
#'    factors_list = factors,
#'    model_eco = exTobitModel,
#'    equation = "outcome",
#'    measure = Atkinson,
#'    database = exData,
#'    transfo = exp,
#'    mXOutcome = distrib,
#'    correction = NA,
#'    errors = NA,
#'    equaGame = FALSE,
#'    theta = 1,
#'    weights = exData$extridf)
#'}
#'margContrib <- getMarginalContrib(
#'  inequality = ineq,
#'  coalitions = coa,
#'  nVar = length(factors))
#'getShapleyVal(marginalContrib = margContrib)
getShapleyVal <- function(marginalContrib){
  n <- ncol(marginalContrib)
  results <- data.frame(matrix(NA, ncol = (n), nrow = 2))
  #Shapley value: Sum of the weighted marginal contributions
  results[1, ] <- c(apply(marginalContrib[, 1:n-1], 2, sum),
                    marginalContrib[nrow(marginalContrib), "ineq"])
  #Include the total inequality
  results[1, n]  <- marginalContrib[nrow(marginalContrib), "ineq"]
  #Include the results:
  results[2, ] <- results[1, ] / results[1, n]
  colnames(results) <- colnames(marginalContrib)
  rownames(results) <- c("shapley", "shapleyRel")
  return(results)
}
