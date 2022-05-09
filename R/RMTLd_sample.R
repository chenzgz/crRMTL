#' @name RMTLd_sample
#' @title Sample size based on the difference in restricted mean time lost (RMTLd)
#'
#' @description calculating sample size based on RMTLd
#'
#' @param alpha The default is 0.05.
#'
#' @param beta  The default is 0.20.
#'
#' @param r The ratio (group 1 over group 0) of the sample size between groups
#'
#' @param delta A value to specify RMTLd.
#'
#' @param var.0 A value to specify the variance in group 0.
#'
#' @param var.1 A value to specify the variance in group 1.
#'
#' @usage RMTLd_sample(alpha, beta, r, delta, var.0, var.1)
#'
#' @import stats
#'
#' @return an object of class RMTLd_sample
#'
#' @export
#'
#' @examples
#' library(crRMTL)
#' data(Example1)
#' test <- RMTLd_test(Example1$time, Example1$status, Example1$group, alpha = 0.05,
#' digits = 3, tau = NULL)
#' n0 <- table(Example1$group)[1]
#' n1 <- table(Example1$group)[2]
#' r <- n1 / n0
#' delta <- test$Test.event1[3, 1]  ## RMTL of event 1
#' var.0 <- test$RMTL[1, 3] * n0
#' var.1 <- test$RMTL[2, 3] * n1
#' sample <- RMTLd_sample(alpha = 0.05, beta = 0.2, r = r, delta = delta, var.0 = var.0, var.1 = var.1)
#' sample

RMTLd_sample <- function(alpha = 0.05, beta = 0.2, r = 1, delta = 0.9, var.0 = 10, var.1 = 10){

  nrom <- (qnorm(1 - beta) + qnorm(1 - alpha / 2)) ^ 2
  s.rmtl <- round((1 + r) * nrom * (var.0 + var.1 / r) / delta ^ 2)
  c.rmtl <- round(s.rmtl * 1 / (1 + r))
  t.rmtl <- round(s.rmtl * r / (1 + r))
  rmtl <- c.rmtl + t.rmtl

  output <- matrix(0, 1, 3)
  output[1,] <- c(rmtl, c.rmtl, t.rmtl)
  rownames(output) <- c("RMTLd")
  colnames(output) <- c("Total", "Group 0", "Group 1")
  return(output)

}

