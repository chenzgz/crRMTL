#' @name RMTLd_test
#' @title Comparing the difference in restricted mean time lost (RMTLd) under competing risks
#'
#' @description Performs two-sample comparisons using the difference in restricted mean time
#' lost (RMTLd) as a summary measure of between-group difference under
#' competing risks. At the same time, the outcome of hazard-based measures are
#' also reported for assessing between-group difference.
#'
#' @usage  RMTLd_test(time, status, group, alpha = 0.05, digits = 3, tau = NULL)
#'
#' @importFrom survival survfit survdiff Surv coxph cox.zph
#' @importFrom cmprsk cuminc timepoints crr
#' @importFrom stats pchisq pnorm qnorm
#'
#' @param time The follow-up time for right censored data.
#' @param status The status indicator, 1 = event of interest, 2 = competing
#' events and 0 = right censored.
#' @param group The group indicator for comparison. The elements of this vector
#' take either 1 or 0. Normally, 0 = control group, 1 = treatment group.
#' @param alpha The default is 0.05. (1-\code{alpha}) confidence intervals are
#' reported.
#' @param digits The decimal places settings, digits = 3 are set by default.
#' @param tau  The value to specify the truncation time point for the RMTL
#' calculation.
#' The default is the minimum of the largest follow-up time of two groups.
#'
#' @return an object of class RMTLd_test.
#' @return \item{Note}{a note regarding the specification of the truncation
#' time (tau), the default tau will be reported if tau was not specified.}
#' @return \item{RMTL}{RMTL results in two groups, including RMTL of event 1
#' and event 2, and corresponding variance and confidence interval in group 0 and 1.}
#' @return \item{Test.event1}{
#' Three kinds of effect size between groups of event 1 (event of interest), including
#' cause-specific hazard ratio (cHR, group 1 over group 0), subdistributional Hazard Ratio
#' (sHR, group 1 over group 0) and RMTLd (group 1 minus group 0), and corresponding
#' confidence intervals. Meanwhile, the Z-statistics and P-value are estimated via log-rank test,
#' Gray's test and RMTLd test.}
#' @return \item{Test.event2}{
#' Three kinds of effect size between groups of event 2 (competing event), including
#' cause-specific hazard ratio (cHR, group 1 over group 0), subdistributional Hazard Ratio
#' (sHR, group 1 over group 0) and RMTLd (group 1 minus group 0), and corresponding
#' confidence intervals. Meanwhile, the Z-statistics and P-value are estimated via log-rank test,
#' Gray's test and RMTLd test.}
#'
#' @export
#'
#' @examples
#' library(crRMTL)
#' data(Example1)
#' # The time point is set as the minimum of the largest follow-up time of two groups with tau = NULL.
#' RMTLd_test(Example1$time, Example1$status, Example1$group, alpha = 0.05, digits = 3, tau = NULL)
#'
#' # The time point can be specified as fixed time point,like tau = 10.
#' RMTLd_test(Example1$time, Example1$status, Example1$group, alpha = 0.05, digits = 3, tau = 10)
#'
RMTLd_test <- function(time, status, group, alpha = 0.05, digits = 3, tau = NULL){

  ddd <- table(time, status, group)
  judge <- as.data.frame(table(status, group))

  if (dim(ddd)[3] != 2)
    stop("RMTLd test is for two groups.")
  if (dim(ddd)[2] > 3)
    stop("All competing events should be coded as 2.")
  if (dim(ddd)[2] == 1)
    stop("There exists only one type of event or all observations are censored.")
  if (dim(ddd)[2] == 2 & sum(judge$status == 0) != 0)
    stop("There is no competing event or event of interest.")

  if (judge[judge$status == 2, 3][1] == 0 | judge[judge$status == 2, 3][2] == 0)
    stop("There is no competing event in at least one group.")
  if (judge[judge$status == 1, 3][1] == 0 | judge[judge$status == 1, 3][2] == 0)
    stop("There is no event of interest in at least one group.")

  if (sum(group==0) == 0 | sum(group==1) == 0)
    stop("Group variables should be coded as 0 and 1.")



  status1 <- status
  status2 <- ifelse(status == 1, 3, status)
  status2 <- ifelse(status2 == 2, 1, status2)
  status2 <- ifelse(status2 == 3, 2, status2)


  ##### estimate three statistic --- cHR sHR and RMTLd
  inner2<-function(event.type){


    s0 <- 1 * (event.type == 1 | event.type == 2)
    s1 <- 1 * (event.type == 1)
    s2 <- 1 * (event.type == 2)
    d <- data.frame(group, time, status = event.type, s0, s1, s2)
    d0 <- d[d$group == 0,]
    d1 <- d[d$group == 1,]
    n1 <- table(group)[[1]]
    n2 <- table(group)[[2]]

    tau_max <- min(max(d0$time), max(d1$time))
    if(!is.null(tau)){
      if(tau <= tau_max){
        NOTE <- paste("The truncation time: tau =", tau, " was specified.")
      }
      if(tau > tau_max){
        stop(paste("The truncation time, tau, needs to be shorter than or equal to the minimum of the largest observed time on each of the two groups: ",
                   round(tau_max, digits=digits)))
      }
    }
    if(is.null(tau)){
      tau <- tau_max
      NOTE <-(paste("The truncation time, tau, was not specified. Thus, the default tau (the minimum of the largest observed time on each of the two groups)",
                    round(tau_max, digits=digits)," is used."))
    }

    ##### estimate RMTL in each group
    inner1 <- function(data,tau){

      sur_all <- survfit(Surv(time, s0) ~ 1, data = data)
      sur_int <- survfit(Surv(time, s1) ~ 1, data = data)

      index1 <- grep("TRUE", (sur_all[["n.event"]] != 0), value = F)
      index2 <- (sur_all[["time"]][index1] < tau)

      point <- sur_all[["time"]][index1][index2]
      t_fin <- c(point, tau)

      cif <- timepoints(cuminc(d$time, d$status), t_fin)$est[1,]
      cif1 <- timepoints(cuminc(data$time, data$status), t_fin)$est[1,]
      cif2 <- timepoints(cuminc(data$time, data$status), t_fin)$est[2,]

      e <- sur_int[["n.event"]][index1][index2]
      e_all <- sur_all[["n.event"]][index1][index2]

      r <- sur_all[["n.risk"]][index1][index2]
      s <- sur_all[["surv"]][index1][index2]
      N <- cumsum(sur_all[["n.event"]])[index1][index2]
      N1 <- cumsum(e)

      rmtl <- diff(t_fin)*cif1[-length(cif1)]
      R_L <- sum(rmtl)

      var.bk <- diff(c(0, cif1[-length(cif1)])) * ((tau - point) * (1 - cif2[-length(cif2)])- rev(cumsum(rev(rmtl)))) ^ 2 / s / r +
        diff(c(0, cif2[-length(cif2)])) * ((tau - point) * cif1[-length(cif1)] - rev(cumsum(rev(rmtl)))) ^ 2 / s / r
      var.bk[length(var.bk)] <- ifelse(s[length(s)] == 0, 0, var.bk[length(var.bk)])
      var.BK <- sum(var.bk)

      output <- c(tau, R_L, var.BK,
                  R_L - qnorm(1 - alpha / 2) * sqrt(var.BK),
                  R_L + qnorm(1 - alpha / 2) * sqrt(var.BK))
      output
    }

    G1 <- inner1(d0, tau)
    G2 <- inner1(d1, tau)
    CIl <- paste( (1-alpha)*100, "L", sep = "%")
    CIu <- paste( (1-alpha)*100, "U", sep = "%")

    out1 <- matrix(0, 2, 5)
    out1[1, ] <- G1
    out1[2, ] <- G2
    rownames(out1) <- c("group = 0", "group = 1")
    colnames(out1) <- c("tau", "RMTL", "var ", CIl, CIu)

    CHR<-summary(coxph(Surv(time,s1)~group),data=d)$conf.int[c(1,3,4)]
    SHR<-summary(crr(ftime=d$time,fstatus=d$status,cov1=d$group,failcode=1,
                     cencode=0))$conf.int[c(1,3,4)]
    RMTLd <- c((out1[2,2] - out1[1,2]),
               (out1[2,2] - out1[1,2]) - qnorm(1 - alpha / 2) * sqrt(out1[1, 3] + out1[2, 3]),
               (out1[2,2] - out1[1,2]) + qnorm(1 - alpha / 2) * sqrt(out1[1, 3] + out1[2, 3]))
    out2 <- rbind(CHR, SHR, RMTLd)
    rownames(out2) <- c(" CHR  (group 1 / group 0)",
                        " SHR  (group 1 / group 0)",
                        "RMTLd (group 1 - group 0)")
    colnames(out2) <- c("Est", CIl, CIu)

    z1 <- survdiff(Surv(d$time, d$s1) ~ group)$chisq
    z2 <- cuminc(d$time, d$status, d$group, cencode = 0)$Tests[1, ]
    z3 <- (out1[2, 2] - out1[1, 2]) / sqrt(out1[1, 3] + out1[2, 3])

    out3 <- round(cbind(c(z1, z2[1], z3),
                        c(1 - pchisq(z1, 1), z2[2], 2 * (1 - pnorm(abs(z3))) ) ), digits = digits)
    colnames(out3) <- c("Z  ","P-value")

    out4 <- cbind(out2, out3)

    z <- list()
    z$Note <- NOTE
    z$RMTL <- out1
    z$Test <- out4
    z
  }


  out1 <- matrix(0,4,5)
  rmtl1 <- inner2(status1)
  rmtl2 <- inner2(status2)

  out1 <- round(rbind(rmtl1$RMTL, rmtl2$RMTL),digits = digits)
  rownames(out1) <- c("Event 1 RMTL (group 0)",
                      "        RMTL (group 1)",
                      "Event 2 RMTL (group 0)",
                      "        RMTL (group 1)")


  z <- list()
  z$Note <- rmtl1$Note
  z$RMTL <- out1
  z$Test.event1 <- round(rmtl1$Test, digits = digits)
  z$Test.event2 <- round(rmtl2$Test, digits = digits)

  z
}

