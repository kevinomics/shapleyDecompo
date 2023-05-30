#' Compute entropy measure of inequality
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#' @param theta Value of sensibility.
#'
#' @return A value of inequality.
#' @export
Entropy <- function(distri, weights, theta){
  if (theta == 1) {
    Th <- (distri) / stats::weighted.mean(distri, weights)
    Th <- sum(distri * log(Th) * weights)
    Th <- Th / (sum(distri * weights))
    return(Th)
  }else if (theta==0) {
    Th <- exp(stats::weighted.mean(log(distri), weights)) /
      stats::weighted.mean(distri, weights)
    Th <- -log(Th)
    return(Th)
  } else {
    e <- (distri / stats::weighted.mean(distri, weights))^theta
    e <- stats::weighted.mean((e - 1),weights) / (theta * (theta - 1))
    return(e)
  }
}
#' Atkinson
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#' @param theta Value of sensibility.
#'
#' @return A value of inequality.
#' @export
Atkinson <- function(distri, weights, theta){
  mean_distri <- stats::weighted.mean(distri, weights)
  if (theta == 1) {
    AT <- 1 - exp(stats::weighted.mean(log(distri), weights)) /
      stats::weighted.mean(distri, weights)
    return(AT)
  } else {
    x <- (distri / stats::weighted.mean(distri, weights))^(1 - theta)
    AT <- 1 - stats::weighted.mean(x, weights)^(1 / (1 - theta))
    return(AT)
  }
}
#' Kolm
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#' @param theta Value of sensibility.
#'
#' @return A value of inequality.
#' @export
Kolm <- function(distri, weights, theta){
  KM <- theta * (stats::weighted.mean(distri, weights) - distri)
  KM <- stats::weighted.mean(exp(KM), weights)
  KM <- (1 / theta) * log(KM)
  return(KM)
}
#' Var
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#'
#' @return A value of inequality.
#' @export
Var <- function(distri, weights){
  sum.w <- sum(weights)
  sum.w2 <- sum(weights^2)
  mean.w <- sum(distri * weights) / sum(weights)
  (sum.w / (sum.w^2 - sum.w2)) * sum(weights * (distri - mean.w)^2)
}
#' Gini_G
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#' @param theta Value of sensibility.
#'
#' @return A value of inequality.
#' @export
Gini_G <- function(distri, theta){
  #Créer un message alerte: "This generalised Gini measure is not suitable with weights."
  x <- as.numeric(na.omit(distri))
  n <- length(x)
  x <- sort(x)
  r <- seq(1,n)
  mu<- mean(x)
  t <- ifelse(is.na(theta),2,theta)
  g=1-sum((((n+1-r)^t-(n-r)^t)/n^t)*(x/mu))
  return(g)
}

#' Gini_w
#'
#' @param distri Distribution of outcomes.
#' @param weights Weights used in the econometric model.
#'
#' @return A value of inequality.
#' @export
Gini_w <- function(distri, weights){
  ox <- order(distri)
  x <- distri[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}
