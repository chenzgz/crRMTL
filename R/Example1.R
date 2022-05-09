#' @title  Data from The NEfERT-T Randomized Clinical Trial
#' @description A data frame of 479 patients enrolled from the NEfERT-T randomized clinical trial (Awada, 2016).
#' This trial was used for comparing the trastuzumab-paclitaxel (TP, n = 237)
#' and neratinib-paclitaxel (NP, n = 242) treatment for women with metastatic ERBB2-positive breast cancer
#' on central nervous system (CNS) progression. The included variables are
#'
#' @description time
#' @description Time in months from baseline to CNS lesions or death without CNS
#' lesions or last follow-up.
#' @description status
#' @description Event status; 1 = CNS lesions, 2 = death without CNS lesions, 0 = censored
#' @description group
#' @description Treatment group; 0 = TP group, 1 = NP group
#'
#' @usage  data(Example1)
#'
#' @format A data frame with 479 rows and 3 variables
#'
#' @source The data was reconstructed (Guyot, 2012) using the progression-free
#' survival and overall survival curves from the NEfERT-T trial. And this data
#' was applied for the purpose of illustration of the analysis of competing risks.
#' No clinical conclusions should be drawn from this data.
#'
#' @references Awada A, Colomer R, Inoue K, et.al. Neratinib Plus Paclitaxel
#' vs Trastuzumab Plus Paclitaxel in Previously Untreated Metastatic
#' ERBB2-Positive Breast Cancer: The NEfERT-T Randomized Clinical Trial.
#' JAMA Oncol. 2016;2(12):1557-1564.
#' @references Guyot P, Ades AE, Ouwens MJ, Welton NJ. Enhanced secondary
#' analysis of survival data: reconstructing the data from published Kaplan-Meier
#' survival curves. BMC Med Res Methodol. 2012;12:9.
"Example1"

