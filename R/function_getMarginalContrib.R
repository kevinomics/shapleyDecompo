#' Compute the marginal contributions of each attribute in all coalitions.
#'
#' @param inequality A vector containing all inequalities.
#' @param coalitions A data.frame with all coalitions used for the computation of the inequality.
#' @param nVar The number of factors.
#'
#' @return A data.frame with the marginal contributions and inequalities for all attributes.
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
#'getMarginalContrib(
#'  inequality = ineq,
#'  coalitions = coa,
#'  nVar = length(factors))
getMarginalContrib <- function(inequality, coalitions, nVar){
  coa <- coalitions
  coaux <- coalitions
  coaux$name <- inequality
  colnames(coaux)[ncol(coaux)] <- "ineq"
  n <- nVar
  #Compute the marginal contributions:
  #For each attribute i and each coalition j
  #Compute the difference of inequality when i is in j and when it is no longer
  for (j in 1:(2^n - 1)) {
    for (i in 1:n) {
      if (coa[j, i] == 1) {
        #Find the number of the row of considered coalition where i is no longer in
        col_j <- coa[j, n+1]
        substr(col_j, i, i) <- "0"
        #If the coalition is empty after i left, the inequality is null
        if (as.numeric(col_j) == 0) {
          ineq_out <- 0
        }else { #Else, we take the inequality value of the considered coalition in coaux
          ineq_out <- inequality[which(coa$name == col_j)]
        }
        s <- length(which(coa[j, 1:n] != 0))
        #Use the shapley weight : it gives the shapley weigthed marginal contribution
        coaux[j, i] <- (factorial(s - 1) * factorial(n - s)) / factorial(n) * (inequality[j] - ineq_out)
      }
    }
  }
  return(coaux)
}
