#' Create Summary Tables for Statistical Reports
#'
#' Contains functions for creating various types of summary tables, e.g.
#' comparing characteristics across levels of a categorical variable and
#' summarizing fitted generalized linear models, generalized estimating
#' equations, and Cox proportional hazards models. Functions are available to
#' handle data from simple random samples as well as complex surveys.
#'
#' \tabular{ll}{
#' Package: \tab tab \cr
#' Type: \tab Package \cr
#' Version: \tab 4.1.1 \cr
#' Date: \tab 2019-10-26 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See \href{https://cran.r-project.org/package=tab}{CRAN documentation} for
#' full list of functions.

#' @author Dane R. Van Domelen \cr \email{vandomed@gmail.com}
#'
#' @references
#' Acknowledgment: This material is based upon work supported by the
#' National Science Foundation Graduate Research Fellowship under Grant No.
#' DGE-0940903.
#'
#' @docType package
#' @importFrom dplyr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom knitr kable
#' @import MASS
#' @import stats
#' @importFrom survey svyby svychisq svyglm svymean svyquantile svyranktest svytable svyttest svyvar
#' @importFrom utils capture.output
#' @importFrom xtable xtable
#' @name tab
NULL
