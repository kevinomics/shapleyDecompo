#' Compute the Shapley decomposition of the inequality of an outcome variable across different attributes from an econometric model.
#'
#' @param database A data.frame with data used to estimate the model.
#' @param model_eco An econometric model.
#' @param equation Either the "selection" equation or the "outcome" equation from a Tobit II model.
#' @param correction To keep the sum of the outcome constant across coalitions (either NA by default, "abs" or "rel").
#' @param transfo Specify a function used to transform the counterfactual outcome distribution before computing the inequality (default is NULL).
#' @param data_weights A vector of individuals weights (vector of 1 by default).
#' @param residuals Whether the residuals of the model are considered as an attribute in the decomposition or not (TRUE by default, or FALSE).
#' @param measure The function to use to compute the inequality, either Gini(),
#'    Gini_G(), Entropy(), Atkinson(), Kolm(), Var().
#' @param theta The sensibility parameter value for the computation of the inequality (1 by default).
#' @param equaGame Whether the game is equalized or not (either TRUE, by default, or FALSE).
#' @return Returns a list with: 1) The Shapley value for each attribute, 2) The pure marginal contribution of each attribute, 3) The pairwise interaction terms.
#'
#' @import sampleSelection
#' @importFrom stats residuals
#' @export
#'
#' @examples
#' shapleyDecompo(database = exData,
#'                model_eco = exTobitModel,
#'                model = "outcome",
#'                equaGame = TRUE,
#'                correction = NA,
#'                data_weights = exData$extridf,
#'                residuals = TRUE,
#'                transfo = exp,
#'                measure = Atkinson,
#'                theta = 1)
shapleyDecompo <- function(database,
                           model_eco,
                           model = "selection",
                           equaGame = TRUE, # default value is TRUE (equalized game)
                           correction = NA, # default value is NA (no correction)
                           transfo = NULL,
                           data_weights = rep(1, nrow(database)), # default value is 1 (no weights)
                           residuals = FALSE,
                           measure,
                           theta = NULL){

  # call functions
  # source("script/functions.R")

  # get factors list from the difference between model variables and database factors
  factors_list <- getFactorList(model_eco, database, residuals, model)

  #Individual weights vector:
  database$weights <- data_weights

  #Add the residuals term in the decomposition
  if(residuals == TRUE){
    if(class(model_eco)[1] == "selection"){
      if(model == "selection"){
        error <- stats::residuals(model_eco, type = "response", part = "selection")
      }else{
        error <- stats::residuals(model_eco)
      }
    }
    if(class(model_eco)[1] == "glm"){
      error <- stats::residuals(model_eco)
    }
  }else{
    error <- NA
  }

#Number of considered attributes for the decomposition
n <- length(factors_list)

# Get distributions used for decomposition
distrib <- getShapleyDistrib(model_eco = model_eco,
                             model = model,
                             database = database)

#Define the set of possible coalitions (null coalition excluded)
coa <- getCoalitions(factors_list = factors_list)

#Coaux is a copy of coa and it will be used to include the marginal contributions
coaux <- coa
colnames(coaux)[ncol(coaux)] <- "ineq"

# ----------------------------------------------------------------------------
#Compute the total inequality for each coalition :
# ineq <- lapply(sample(1:nrow(coa), size = 10), function(i){ # quick test

# can be parallelized (see ?parLapply)
ineq <- lapply(1:nrow(coa), function(i){
  # print(i)
  getInequality(coalition = coa[i, ],
                factors_list = factors_list,
                model_eco = model_eco,
                model = model,
                database = database,
                transfo = transfo,
                measure = measure,
                mXOutcome = distrib,
                correction = correction,
                errors = error,
                equaGame = equaGame,
                theta = theta,
                weights = database$weights)
})

#Inequality of each coalition:
ineq <- unlist(ineq)

coaux <- getMarginalContrib(inequality = ineq, coalitions = coa, nVar = n)

results <- getShapleyVal(marginalContrib = coaux)

#Compute the interactions terms:
intTerms <- getInteractionTerms(factors_list = factors_list, coalitions = coa, marginalContrib = coaux)

return(list("IMPORTANCE" = results, "INTERACTIONS" = intTerms[[1]], "INTERACTIONS_r" = intTerms[[2]]))

}
