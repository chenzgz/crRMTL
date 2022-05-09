#' @title  Data from the European Society for Blood and Marrow Transplantation (EBMT)
#' @description A data frame of 2279 patients transplanted at the EBMT between 1985 and 1998.
#' The data were used in de Wreede (2011) but simplified as the following variables:
#'
#' @description time
#' @description Time in years from transplantation to relapse or death without relapse
#' or last follow-up.
#' @description status
#' @description Event status; 1 = death without relapse, 2 = relapse, 0 = censored
#' @description group
#' @description Donor-recipient gender match; 0 = gender mismatched, 1 = gender matched
#'
#' @usage  data(Example2)
#'
#' @format A data frame with 2279 rows and 3 variables
#'
#' @source The data was simplified for the purpose of illustration of the analysis of competing risks.
#' No clinical conclusions should be drawn from this data.
#'
#' @references de Wreede LC, Fiocco M, Putter H. mstate:
#' An R Package for the Analysis of Competing Risks and Multi-State Models.
#' J STAT SOFTW, 2011,38(7).
"Example2"

