#' Compute the inequality for a given coalition and a given econometric model.
#'
#' @param coalition The coalition used for the computation of the inequality.
#' @param factors_list A list of factors in the econemetric model.
#' @param model_eco An econometric model.
#' @param equation Either the "selection" equation or the "outcome" equation from a Tobit II model.
#' @param measure The function to use to compute the inequality, either Gini(),
#'    Gini_G(), Entropy(), Atkinson(), Kolm(), Var().
#' @param database A data.frame with data used to calibrate the model.
#' @param transfo Specify the inverse of the function used to transform the data.
#' @param mXOutcome A data.frame with xxxxx
#' @param correction Whether the results need to be corrected or not (either NA,
#'    "abs" or "rel").
#' @param errors A vector with the model residuals.
#' @param equaGame Whether the game is equalized or not (either TRUE or FALSE).
#' @param theta A value of sensibility for the computation of the inequality.
#' @param weights A vector with the weights used in the econometric model (if
#'    there are none use NA).
#'
#' @return The value of inequality for the considered coalition.
#' @export
#'
#' @examples
#' factors <- getFactorList(
#'   equation = "outcome",
#'   model_eco = exTobitModel,
#'   database = exData)
#' coa <- getCoalitions(factors_list = factors)
#' distrib <- getShapleyDistrib(
#'   model_eco = exTobitModel,
#'   equation = "outcome",
#'   database = exData)
#' getInequality(
#'   coalition = coa[1, ],
#'   factors_list = factors,
#'   model_eco = exTobitModel,
#'   equation = "outcome",
#'   measure = Atkinson,
#'   database = exData,
#'   transfo = exp,
#'   mXOutcome = distrib,
#'   correction = NA,
#'   errors = NA,
#'   equaGame = FALSE,
#'   theta = 1,
#'   weights = exData$extridf)
getInequality <- function(coalition,
                          factors_list,
                          model_eco,
                          equation,
                          measure = Atkinson,
                          database,
                          transfo = NULL,
                          mXOutcome,
                          correction = NA,
                          errors,
                          equaGame = FALSE,
                          theta = NULL,
                          weights){
  database$weights <- weights
  if(class(model_eco)[1] == "glm"){
    database <- stats::na.omit(database)
  }
  if(class(model_eco)[1] == "selection"){
    if(any(is.na(errors)) && length(errors) > 1){
      database <- database[!is.na(errors), ]
      mXOutcome <- mXOutcome[!is.na(errors), ]
      weights <- weights[!is.na(errors)]
      errors <- errors[!is.na(errors)]
    }
  }
  mXOutcomeSave <- mXOutcome

  # List cross terms -----------------------------------------------------------
  if(class(model_eco)[1] == "glm"){
    modFactors <- attr(model_eco$terms, "factors")
  }

  if(class(model_eco)[1] == "selection"){
    if(equation == "selection"){
      modFactors <- attr(model_eco$termsS, "factors")
    }
    if(equation == "outcome"){
      modFactors <- attr(model_eco$termsO, "factors")
    }
  }
  if(sum(apply(modFactors, 2, sum)) > length(unlist(factors_list))){
    crossTerms <- TRUE
  }else{
    crossTerms <- FALSE
  }
  if (crossTerms){
    # Extract the relevant estimated parameters
    if(class(model_eco)[1] == "glm"){
      cross_terms <- names(
        stats::coef(model_eco))[1L +
                                  which(
                                    colSums(
                                      attr(
                                        model_eco$terms,
                                        "factors"
                                      )[, attr(
                                        stats::model.matrix(model_eco),
                                        "assign"
                                      )[-1L]]
                                    ) >= 2L
                                  )]
    }

    if(class(model_eco)[1] == "selection"){
      if (equation == "selection"){
        vIndexBeta <- model_eco$param$index$betaS
        cross_terms <- names(
          stats::coef(model_eco)[vIndexBeta])[1L +
                                                which(
                                                  colSums(
                                                    attr(
                                                      model_eco$termsS,
                                                      "factors"
                                                    )[, attr(
                                                      stats::model.matrix(model_eco$termsS,
                                                                          data = database),
                                                      "assign"
                                                    )[-1L]]
                                                  ) >= 2L
                                                )]
      } else {
        vIndexBeta <- model_eco$param$index$betaO
        cross_terms <- names(
          stats::coef(model_eco)[vIndexBeta])[1L +
                                                which(
                                                  colSums(
                                                    attr(
                                                      model_eco$termsO,
                                                      "factors"
                                                    )[, attr(
                                                      stats::model.matrix(model_eco$termsO,
                                                                          data = database),
                                                      "assign"
                                                    )[-1L]]
                                                  ) >= 2L
                                                )]
      }
    }
    mXOutcome <- mXOutcome[, !(colnames(mXOutcome) %in% cross_terms)]
  }

  n <- length(factors_list)
  l <- coalition
  #Define the size of the coalition
  s <- length(which(l[1:n] != 0))

  #Define the set of attributes out of the coalition
  #For the time being, we out aside the residuals attribute, we will consider it
  #later
  var_out <- unlist(factors_list[names(l[which(l[1:n] == 0)])])

  if(length(var_out) == 1 && var_out == "Residuals"){
    var_out <- NULL
  }else{
    var_out <- var_out[!(var_out %in% "Residuals")]
  }

  var_in <- unlist(factors_list[names(l[which(l[1:n] == 1)])])
  if(length(var_in) == 1 && var_in == "Residuals"){
    var_in <- NULL
  }else{
    var_in <- var_in[!(var_in %in% "Residuals")]
  }
  if(class(model_eco)[1] == "selection"){
    #List of modalities associated to each covariates, whether there are in or out
    if(equation == "selection"){
      dClassOut <- attr(model_eco$termsS, "dataClasses")[var_out]
      dClassIn <- attr(model_eco$termsS, "dataClasses")[var_in]
    }else{
      dClassOut <- attr(model_eco$termsO, "dataClasses")[var_out]
      dClassIn <- attr(model_eco$termsO, "dataClasses")[var_in]
    }
  }
  if(class(model_eco)[1] == "glm"){
    dClassOut <- attr(model_eco$terms, "dataClasses")[var_out]
    dClassIn <- attr(model_eco$terms, "dataClasses")[var_in]
  }

  if(!is.null(var_out)){
    list_moda_all_out <- lapply(1:c(length(dClassOut)), function(x){
      if(dClassOut[var_out[x]] == "factor"){
        paste0(var_out[x],levels(database[, var_out[x]]))
      }else{
        var_out[x]
      }
    })
    list_moda_all_out <- unlist(list_moda_all_out)
  }else{
    list_moda_all_out <- NULL
  }
  if(!is.null(var_in)){
    list_moda_all_in <- lapply(1:c(length(dClassIn)), function(x){
      if(dClassIn[var_in[x]] == "factor"){
        paste0(var_in[x], levels(database[, var_in[x]]))
      }else{
        var_in[x]
      }
    })
  }else{
    list_moda_all_in <- NULL
  }
  list_moda_all_in <- unlist(list_moda_all_in)

  #Specify the way to neutralize outcome vectors
  if (equaGame){
    fct_neut <- function(X){
      rep(mean(X), length(X))
    }
  } else {
    fct_neut <- function(X){
      X * 0
    }
  }
  if(class(model_eco)[1] == "selection"){
    # Extract the relevant estimated parameters
    if (equation == "selection"){
      vIndexBeta <- model_eco$param$index$betaS
    } else {
      vIndexBeta <- model_eco$param$index$betaO
    }
    vBeta <- stats::coef(model_eco)[vIndexBeta]
  }
  if(class(model_eco)[1] == "glm"){
    vBeta <- stats::coef(model_eco)
  }

  #Distribution of covariates (by modalities)
  if(!is.null(list_moda_all_out)){
    mXOutcome_out <- as.data.frame(mXOutcome[, colnames(mXOutcome) %in%
                                               list_moda_all_out])
    colnames(mXOutcome_out) <- colnames(mXOutcome)[colnames(mXOutcome) %in%
                                                     list_moda_all_out]

    mXOutcome_out <- apply(mXOutcome_out, 2, fct_neut)

    mXOutcome_in <- as.data.frame(mXOutcome[, colnames(mXOutcome) %in%
                                              list_moda_all_in])
    colnames(mXOutcome_in) <- colnames(mXOutcome)[colnames(mXOutcome) %in%
                                                    list_moda_all_in]
    mXOutcomeC <- cbind(mXOutcome[, 1], mXOutcome_in, mXOutcome_out)
    colnames(mXOutcomeC)[1] <- "(Intercept)"
  }else{
    mXOutcome_in <- as.data.frame(mXOutcome[, colnames(mXOutcome) %in%
                                              list_moda_all_in])
    colnames(mXOutcome_in) <- colnames(mXOutcome)[colnames(mXOutcome) %in%
                                                    list_moda_all_in]
    mXOutcomeC <- cbind(mXOutcome[,"(Intercept)" ], mXOutcome_in)
    colnames(mXOutcomeC) <- c("(Intercept)", colnames(mXOutcomeC)[-1])
    colnames(mXOutcomeC)[1] <- "(Intercept)"
  }

  # If there are cross terms, if the considered variable interacts and if this
  # variable has not been yet considered in this regard
  if (crossTerms){
    #Dataframe without cross terms
    df2 <- as.data.frame(mXOutcomeC)
    #Drop specific characters, but keep ":" for cross terms
    colnames(df2) <- gsub("[^[:alnum:]\\:\\s]", "", colnames(df2))
    #Create an index to change variable name (change is necessary to avoid
    #problems in creating a new formula after)
    index <- paste0("N", seq( from = 1, to = ncol(df2)))
    map <- list(index, colnames(df2))
    #Two new lists of variables, one without crossterms and one with.
    list_var_a <- gsub("[^[:alnum:]\\:\\s]", "", names(vBeta))
    list_var <- colnames(df2)
    for(i in 1:length(index)){
      list_var <- gsub(paste0("\\", map[[2]][i]), map[[1]][i], list_var)
      list_var_a <- gsub(paste0("\\", map[[2]][i]), map[[1]][i], list_var_a)
    }
    #Change colnames of df2 to apply model matrix after
    colnames(df2) <- list_var
    #Create a new formula, that takes into considerations cross terms
    form2 <- paste(list_var_a, collapse = " + ")
    form2 <- stats::as.formula(paste0("~", form2))
    #New distribution (one per coalition of attributes)
    mXOutcomeC <- stats::model.matrix(form2, df2)[,-1]
    #Give back
    colnames(mXOutcomeC) <- names(vBeta)
  }

  #Distribution of estimated coefficients
  outcome <- sweep(mXOutcomeC, MARGIN = 2, vBeta, `*`)
  outcome <- apply(outcome, 1, sum)

  #Take into consideration the link function:
  if(class(model_eco)[1] == "selection"){
    if (equation != "selection" & model_eco$outcomeVar == "continuous"){
      outcome <- outcome
    } else { #Probit regression
      outcome <- stats::pnorm(outcome)
    }
  }
  if(class(model_eco)[1] == "glm"){
    outcome <- model_eco$family$linkfun(outcome)
  }

  #Add residuals outcome if it is source of inequality, or its mean if it is in
  #var_out
  if(length(errors) == 1 && is.na(errors)){
    outcome <- outcome
  }else{
    if(l[n] == 0){
      outcome <- outcome + fct_neut(errors)
    } else {
      outcome <- outcome + errors
    }
  }

  #Apply correction (to have a constant sum of total outcome, which might not be
  #respected with cross terms, or null-game)
  if(!is.na(correction)){
    outcome_d <- sweep(mXOutcomeSave, MARGIN = 2, vBeta, `*`)
    # delete useless attributes
    outcome_d <- outcome_d[1:nrow(outcome_d), ]
    if(sum(outcome) != sum(outcome_d)){
      diff <- sum(outcome) - sum(outcome_d)
      if(correction == "abs"){
        outcome <- outcome - diff / nrow(database)
      } else if (correction == "rel") {
        outcome <- outcome * (1 - diff / sum(outcome_d))
      }
    }
  }

  #If the predicted outcome for this coalition is egalitarian, then there is no
  #inequality
  if (min(outcome) == max(outcome)) {
    ineq <- 0
  } else {
    if(!is.null(transfo)){
      outcome <- transfo(outcome)
    }
    #If not, we compute the inequality of the estimated outcome
    if(!is.null(theta)){
      ineq <- measure(distri = outcome,
                      weights = database$weights,
                      theta = theta)
    }else{
      ineq <- measure(distri = outcome,
                      weights = database$weights)
    }

  }
  return(ineq)
}
