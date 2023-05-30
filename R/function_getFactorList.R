#' Extract the list of factors from an econometric model
#'
#' @param model_eco An econometric model.
#' @param database A data.frame with data used to calibrate the model.
#' @param model The type of model (either "selection" or "outcome").
#' @param residuals Whether residuals are considered or not.
#'
#' @return A list of factors.
#' @export
#'
#' @examples
#' getFactorList(model_eco = exTobitModel,
#'               database = exData,
#'               residuals = TRUE,
#'               model = "outcome")
getFactorList <- function(model_eco, database, residuals = FALSE, model = NA){
  if(class(model_eco)[1] == "glm"){
    allFactors <-  paste0(attr(model_eco$terms, "variables"))[c(-1, -2)]
    dataBaseFactors <- colnames(stats::get_all_vars(model_eco$call,
                                                    data = database))[-1]
  }
  if(class(model_eco)[1] == "selection"){
    if(model == "selection"){
      allFactors <-  paste0(attr(model_eco$termsS, "variables"))[c(-1, -2)]
      dataBaseFactors <- colnames(stats::get_all_vars(model_eco$call$selection,
                                                      data = database))[-1]
    }
    if(model == "outcome"){
      allFactors <- paste0(attr(model_eco$termsO, "variables"))[c(-1, -2)]
      dataBaseFactors <- colnames(stats::get_all_vars(model_eco$call$outcome,
                                                      data = database))[-1]
    }
  }
  if(!identical(dataBaseFactors, allFactors)){
    factors_list <- lapply(dataBaseFactors, function(factorX){
      if(length(grep(pattern = factorX, x = allFactors, fixed = TRUE)) >= 2){
        return(allFactors[grep(pattern = factorX, x = allFactors, fixed = TRUE)])
      }else{
        return(factorX)
      }
    })
  }else{
    factors_list <- as.list(dataBaseFactors)
  }
  names(factors_list) <- lapply(factors_list,"[[",1)
  if(residuals){
    factors_list <- c(factors_list, Residuals = "Residuals")
  }
  return(factors_list)
}
