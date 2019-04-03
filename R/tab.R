#' Create Summary Tables for Statistical Reports
#'
#' Contains functions for generating tables for statistical reports written in
#' Microsoft Word or LaTeX. There are functions for I-by-J frequency tables,
#' comparison of means or medians across levels of a categorical variable, and
#' summarizing fitted generalized linear models, generalized estimating
#' equations, and Cox proportional hazards regression. Functions are available
#' to handle data simple random samples or survey data. The package is intended
#' to make it easier for researchers to translate results from statistical
#' analyses in R to their reports or manuscripts.
#'
#' \tabular{ll}{
#' Package: \tab tab \cr
#' Type: \tab Package \cr
#' Version: \tab 4.1.1 \cr
#' Date: \tab 2019-04-03 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See \href{https://cran.r-project.org/package=tab}{CRAN documentation} for
#' full list of functions.

#' @author Dane R. Van Domelen \cr \email{vandomed@gmail.com}
#'
#' @references Acknowledgment: This material is based upon work supported by the
#' National Science Foundation Graduate Research Fellowship under Grant No.
#' DGE-0940903.
#'
#' @docType package
#' @import gee
#' @import graphics
#' @importFrom grDevices recordPlot
#' @import MASS
#' @import stats
#' @importFrom survey svymean svyvar
#' @import survival
#' @import xtable
#' @name tab
NULL
