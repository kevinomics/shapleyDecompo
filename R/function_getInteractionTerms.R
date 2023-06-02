#' Extract the interaction terms from the marginal contributions of attributes in each coalition.
#'
#' @param factors_list A list of factors in the econometric model.
#' @param coalitions A data.frame with all coalitions used for the computation of the inequality.
#' @param marginalContrib The marginal contributions of attributes in each coalition.
#'
#' @return A list with 1) the interactions terms, 2) the relative interaction terms.
#' @export
#'
#' @examples
#' data(Mroz87)
#' exTobitModel <- sampleSelection::selection(lfp ~ age + I(age^2) + faminc + kids5 + educ,
#'     wage ~ exper + I(exper^2) + educ + city,
#'     data = Mroz87)
#' factors <- getFactorList(
#'   equation = "outcome",
#'   model_eco = exTobitModel,
#'   database = Mroz87)
#' coa <- getCoalitions(factors_list = factors)
#' distrib <- getShapleyDistrib(
#'   model_eco = exTobitModel,
#'   equation = "outcome",
#'   database = Mroz87)
#' ineq <- vector()
#' for(i in 1:nrow(coa)){
#'  ineq[i] <- getInequality(
#'   coalition = coa[i, ],
#'   factors_list = factors,
#'   model_eco = exTobitModel,
#'   equation = "outcome",
#'   measure = Gini_w,
#'   database = Mroz87,
#'   transfo = NULL,
#'   mXOutcome = distrib,
#'   correction = NA,
#'   errors = NA,
#'   equaGame = FALSE,
#'   theta = NULL,
#'   weights = rep(1, nrow(Mroz87)))
#' }
#' margContrib <- getMarginalContrib(
#' inequality = ineq,
#'  coalitions = coa,
#'  nVar = length(factors))
#' getInteractionTerms(factors_list = factors,
#'                    coalitions = coa,
#'                    marginalContrib = margContrib)
getInteractionTerms <- function(factors_list, coalitions, marginalContrib){
  coa <- coalitions
  coaux <- marginalContrib
  list_var <- names(factors_list)
  n <- length(factors_list)

  #Compute the interactions terms:
  INT <- matrix(data = NA, nrow = n, ncol = n, dimnames = list(list_var, list_var))
  INT_rel <- matrix(data = NA, nrow = n, ncol = n, dimnames = list(list_var, list_var))
  #Compute the interactions terms:
  for (i in 1:n){
    for (j in 1:n){
      #NB: We could replace 1 by i in the last line, as the interaction matrix is symmetric.
      if (i == j){
        #Si i==j, the we compute the pure marginal contribution of i
        col_j <- coa[2^n-1, ]$name
        substr(col_j, i, i) <- "0"
        INT[i,j] <- coaux[2^n-1, ]$ineq - coaux[which(coa$name == col_j), "ineq"]
        rm(col_j)
      } else {
        #else, we compute the interaction term I(i,j,T)
        #We initiate the temporary table for all the interactions between i and j to get the final interaction term between them
        k <- 1
        # List of the interactions to compute :
        list_coa_it_in <- coa[which(coa[i] == 1 & coa[j] == 1), ]$name
        int_ijt <- matrix(data = NA, nrow = 1, ncol = length(list_coa_it_in))

        for (coa_name_int in list_coa_it_in){
          #Select a coalition where i and j are included
          ineq_ijt <- coaux[which(coa$name == coa_name_int), n+1]

          #Inequality when j left the coalition
          coa_it <- coa_name_int
          substr(coa_it, j, j) <- "0"
          ineq_it  <- coaux[which(coa$name == coa_it), n+1]
          rm(coa_it)

          #Inequality when i left the coalition
          coa_jt <- coa_name_int
          substr(coa_jt, i, i) <- "0"
          ineq_jt  <- coaux[which(coa$name == coa_jt), n+1]
          rm(coa_jt)

          #Inequality when i and j both left the coalition
          coa_t <- coa_name_int
          substr(coa_t, j, j) <- "0"
          substr(coa_t, i, i) <- "0"
          #Define the size of T (i.e. the number of attributes after i and j left the coalition)
          if(as.numeric(coa_t) == 0){
            t = 0
          } else {
            t = table(coa[which(coa$name == coa_t), 1:n] == 1)[[2]]
          }
          ineq_t  <- ifelse(t != 0, coaux[which(coa$name == coa_t), n+1], 0)
          rm(coa_t)

          #The weigthed interaction term between i and j for this coalition,
          int_ijt[k] <- (ineq_ijt - ineq_it - ineq_jt + ineq_t) *
            ((factorial(n - t - 2) * factorial(t+1)) / factorial(n))
          k <- k+1
        }

        #The final interaction term between i and j is the sum of the weighted interaction terms
        INT[i, j] <- apply(int_ijt, 1, sum)
      }
      #Get the relative interaction (compare to total inequality)
      INT_rel[i, j] <- INT[i, j] / coaux[nrow(coaux), n+1]
    }
  }
  return(list("INTERACTIONS" = INT, "INTERACTIONS_r" = INT_rel))
}
