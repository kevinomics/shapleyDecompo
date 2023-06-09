#' Obtain the distributions used in the computation of inequalities.
#'
#' @param model_eco An econometric model.
#' @param equation Either the "selection" equation or the "outcome" equation from a Tobit II model.
#' @param database A data.frame with data used to calibrate the model.
#'
#' @return A list with 1) the distribution of xxx, 2) the distribution of xxx.
#'
#' @export
#' @examples
#' data(Mroz87)
#' exTobitModel <- sampleSelection::selection(lfp ~ age + I(age^2) + faminc + kids5 + educ,
#'     wage ~ exper + I(exper^2) + educ + city,
#'     data = Mroz87)
#' factors <- getFactorList(model_eco = exTobitModel,
#'               database = Mroz87,
#'               residuals = TRUE,
#'               equation = "outcome")
#' coa <- getCoalitions(factors_list = factors)
#' getShapleyDistrib(
#'  model_eco = exTobitModel,
#'  equation = "outcome",
#'  database = Mroz87)
getShapleyDistrib <- function(model_eco, database, equation){
  if(class(model_eco)[1] == "glm"){
    database <- stats::na.omit(database)
  }
  # The matrix of X values vector for each parameter ---------------------------
  if(class(model_eco)[1] == "glm"){
    temp <- eval(model_eco$call)
    form <- stats::as.formula(temp)[-2]
    mf <- stats::model.frame(form, data = database)
    mXOutcome <- stats::model.matrix(form, mf)
  }
  if(class(model_eco)[1] == "selection"){
    if (equation == "selection"){
      temp <- eval(model_eco$call$selection)
      form <- stats::as.formula(temp)[-2]
      mf <- stats::model.frame(form, data = database)
      mXOutcome <- stats::model.matrix(form, mf)
    } else {
      temp <- eval(model_eco$call$outcome)
      form <- stats::as.formula(temp)[-2]
      mf <- stats::model.frame(form, data = database)
      mXOutcome <- stats::model.matrix(form, mf)
    }
  }
  # delete useless attributes
  mXOutcome <- mXOutcome[1:nrow(mXOutcome), ]

  return(mXOutcome)
}
