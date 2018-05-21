#' Create Regression Table from Betas and Standard Errors
#'
#' Useful for quickly creating a summary table.
#'
#' @inheritParams tabmeans
#' @param betas Numeric vector.
#' @param ses Numeric vector.
#' @param varcov Numeric matrix.
#' @param columns Character vector specifying what columns to include. Choices
#' are \code{"variable"}, \code{"beta"}, \code{"se"}, \code{"betaci"},
#' \code{"beta.se"}, \code{"beta.ci"}, \code{"or"}, \code{"orci"},
#' \code{"or.ci"}, and \code{"p"}.
#' @param labels Character vector.
#'
#'
#' @return Data frame.
#'
#'
#' @examples
#' # Create summary table for mtcars regression
#' fit <- lm(mpg ~ wt + hp + drat, data = mtcars)
#' regtable(betas = fit$coef, varcov = vcov(fit),
#'          labels = c("Intercept", "Weight", "HP", "Rear axle ratio"))
#'
#'
#' @export
regtable <- function(betas,
                     ses,
                     varcov = NULL,
                     columns = c("variable", "beta.se", "p"),
                     sep.char = ", ",
                     decimals = NULL,
                     p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
                     p.leading0 = TRUE, p.avoid1 = FALSE,
                     labels = NULL) {

  # If decimals is unspecified, set to appropriate value
  if (is.null(decimals)) {
    max.betas <- max(abs(betas))
    if (max.betas >= 1000) {
      decimals <- 0
    } else if (max.betas < 1000 & max.betas >= 10) {
      decimals <- 1
    } else if (max.betas < 10 & max.betas >= 0.1) {
      decimals <- 2
    } else if (max.betas < 0.1 & max.betas >= 0.01) {
      decimals <- 3
    } else if (max.betas < 0.01 & max.betas >= 0.001) {
      decimals <- 4
    } else if (max.betas < 0.001 & max.betas >= 0.0001) {
      decimals <- 5
    } else if (max.betas < 0.0001) {
      decimals <- 6
    }
  }

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # If varcov is specified, calculate SEs as square root of diagonal elements
  if (! is.null(varcov)) {
    ses <- sqrt(diag(varcov))
  }

  # If CI coverage requested, get z value
  if (any(c("betaci", "beta.ci", "orci", "or.ci", "p") %in% columns)) {
    zval <- qnorm(0.975)
  }

  # Initialize table
  tbl <- matrix("", ncol = length(columns), nrow = length(betas))
  tbl.colnames <- c()

  # Loop through and add columns
  for (ii in 1: length(columns)) {

    column.ii <- columns[ii]

    if (column.ii == "variable") {

      if (is.null(labels)) {
        labels <- names(betas)
        if (is.null(labels)) {
          labels <- paste("beta", 0: (length(betas) - 1), sep = "_")
        }
      }
      tbl[, ii] <- labels
      tbl.colnames[ii] <- "Variable"

    } else if (column.ii == "beta") {

      tbl[, ii] <- sprintf(spf, betas)
      tbl.colnames[ii] <- "Beta"

    } else if (column.ii == "se") {

      tbl[, ii] <- sprintf(spf, ses)
      tbl.colnames[ii] <- "SE"

    } else if (column.ii == "betaci") {

      lowers <- betas - zval * ses
      uppers <- betas + zval * ses
      tbl[, ii] <- paste(sprintf(spf, lowers), sep.char,
                         sprintf(spf, uppers), sep = "")
      tbl.colnames[ii] <- "95% CI for Beta"

    } else if (column.ii == "beta.se") {

      tbl[, ii] <- paste(sprintf(spf, betas), " (",
                         sprintf(spf, ses), ")", sep = "")
      tbl.colnames[ii] <- "Beta (SE)"

    } else if (column.ii == "beta.ci") {

      lowers <- betas - zval * ses
      uppers <- betas + zval * ses
      tbl[, ii] <- paste(sprintf(spf, betas), " (",
                         sprintf(spf, lowers), sep.char,
                         sprintf(spf, uppers), ")", sep = "")
      tbl.colnames[ii] <- "Beta (95% CI)"

    } else if (column.ii == "or") {

      tbl[, ii] <- c("-", sprintf(spf, exp(betas[-1])))
      tbl.colnames[ii] <- "OR"

    } else if (column.ii == "orci") {

      lowers <- exp(betas - zval * ses)
      uppers <- exp(betas + zval * ses)
      tbl[, ii] <- c("-", paste(sprintf(spf, lowers[-1]), sep.char,
                          sprintf(spf, uppers[-1]), sep = ""))
      tbl.colnames[ii] <- "95% CI for OR"

    } else if (column.ii == "or.ci") {

      lowers <- exp(betas - zval * ses)
      uppers <- exp(betas + zval * ses)
      tbl[, ii] <- c("-", paste(sprintf(spf, exp(betas[-1])), " (",
                                sprintf(spf, lowers[-1]), sep.char,
                                sprintf(spf, uppers[-1]), ")", sep = ""))
      tbl.colnames[ii] <- "OR (95% CI)"

    } else if (column.ii == "p") {

      p <- pnorm(-abs(betas / ses)) * 2
      tbl[, ii] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                           lowerbound = p.lowerbound, leading0 = p.leading0,
                           avoid1 = p.avoid1)
      tbl.colnames[ii] <- "P"

    }

  }

  # Add column names
  colnames(tbl) <- tbl.colnames

  # Return table
  return(tbl)

}
