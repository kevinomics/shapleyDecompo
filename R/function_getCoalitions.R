#' Get all possible coalitions for a list of given factors in an econometric
#' model
#'
#' @param factors_list A list of factors in the econometric model.
#'
#' @return A data.frame with factors in columns and all possible coalitions as
#'    rows. The last column corresponds to the name of the coalition which is
#'    defined based on attributes in or out of the coalition.
#'
#' @export
#'
#' @examples
#' factors <- list(A = "A", B = "B", CD = c("C", "D"), E = "E")
#' getCoalitions(factors_list = factors)
getCoalitions <- function(factors_list){
  n <- length(factors_list)
  # Define the set of possible coalitions (null coalition excluded)
  coa <- matrix(data = 0, nrow = 2^n - 1, ncol = n+1)
  for (i in 1:2^n - 1) {
    x <- array(0, dim = n)
    z <- i
    c <- 0
    while (c < n + 1) {
      c <- c + 1
      x[c] <- z - floor(z/2) * 2
      z <- floor(z/2)
    }
    coa[i, ] <- c(x[1:n], NA)
  }

  #A name is given for each coalition
  colnames(coa) <- c(names(factors_list), "name")
  coa <- as.data.frame(coa)
  coa[, n+1] <- apply(coa[, 1:n], 1, paste, collapse = "")
  return(coa)
}
