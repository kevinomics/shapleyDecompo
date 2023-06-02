#' Extract the list of factors from an econometric model
#'
#' @param model_eco An econometric model.
#' @param database A data.frame with data used to calibrate the model.
#' @param equation Either the "selection" equation or the "outcome" equation from a Tobit II model.
#' @param residuals Whether residuals are considered or not.
#'
#' @return The list of factors in the model.
#' @export
#'
#' @examples
#' data(Mroz87)
#' exTobitModel <- sampleSelection::selection(lfp ~ age + I(age^2) + faminc + kids5 + educ,
#'     wage ~ exper + I(exper^2) + educ + city,
#'     data = Mroz87)
#' getFactorList(model_eco = exTobitModel,
#'               database = Mroz87,
#'               residuals = TRUE,
#'               equation = "outcome")
getFactorList <- function(model_eco, database, residuals = FALSE, equation){
  if(class(model_eco)[1] == "glm"){
    allFactors <-  paste0(attr(model_eco$terms, "variables"))[c(-1, -2)]
    dataBaseFactors <- colnames(stats::get_all_vars(model_eco$call,
                                                    data = database))[-1]
  }
  if(class(model_eco)[1] == "selection"){
    if(equation == "selection"){
      allFactors <-  paste0(attr(model_eco$termsS, "variables"))[c(-1, -2)]
      dataBaseFactors <- colnames(stats::get_all_vars(model_eco$call$selection,
                                                      data = database))[-1]
    }
    if(equation == "outcome"){
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
