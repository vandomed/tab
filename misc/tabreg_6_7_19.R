#' Create Regression Table from Betas and Standard Errors
#'
#' Useful for quickly creating a summary table.
#'
#'
#' @param betas Numeric vector.
#' @param ses Numeric vector.
#' @param varcov Numeric matrix.
#' @param columns Character vector specifying what columns to include. Choices
#' are \code{"variable"}, \code{"beta"}, \code{"se"}, \code{"betaci"},
#' \code{"beta.se"}, \code{"beta.ci"}, \code{"or"}, \code{"orci"},
#' \code{"or.ci"}, and \code{"p"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param latex Logical value for whether to format table so it is
#' ready for printing in LaTeX via \code{\link[xtable]{xtable}} or
#' \code{\link[knitr]{kable}}.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param formatp.list Arguments to pass to \code{\link[tab]{formatp}}.
#' @param labels Character vector.
#' @param print.html Logical value for whether to write a .html file with the
#' table to the current working directory.
#' @param html.filename Character string specifying the name of the .html file
#' that gets written if \code{print.html = TRUE}.
#'
#'
#' @return Data frame.
#'
#'
#' @examples
#' # Create summary table for mtcars regression
#' fit <- lm(mpg ~ wt + hp + drat, data = mtcars)
#' tabreg(betas = fit$coef, varcov = vcov(fit),
#'        labels = c("Intercept", "Weight", "HP", "Rear axle ratio"))
#'
#'
#' @export
tabreg <- function(betas,
                   ses,
                   varcov = NULL,
                   columns = c("variable", "beta.se", "p"),
                   sep.char = ", ",
                   latex = TRUE,
                   decimals = NULL,
                   formatp.list = NULL,
                   labels = NULL,
                   print.html = FALSE,
                   html.filename = "table1.html") {

  # Error checking
  if (! is.numeric(betas)) {
    stop("The input 'betas' must be a numeric vector.")
  }
  if (! is.numeric(ses)) {
    stop("The input 'ses' must be a numeric vector.")
  }
  if (! is.null(varcov) && ! (is.matrix(varcov) && ncol(varcov) == nrow(varcov))) {
    stop("The input 'varcov' must be a square numeric matrix.")
  }
  if (! is.null(columns) &&
      ! all(columns %in% c("variable", "beta", "se", "betaci", "beta.se",
                           "beta.ci", "or", "orci", "or.ci", "hr", "hrci",
                           "hr.ci", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'variable', 'beta', 'se', 'betaci', 'beta.se', 'beta.ci', 'or', 'orci', 'or.ci', 'hr', 'hrci', 'hr.ci', 'test', 'p'.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! is.logical(latex)) {
    stop("The input 'latex' must be a logical.")
  }
  if (! (is.numeric(decimals) && decimals >= 0 & decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in% names(as.list(args(formatp)))))) {
    stop("The input 'format.p' must be a named list of arguments to pass to 'formatp'.")
  }
  if (! is.null(labels) && ! is.character(labels)) {
    stop("The input 'labels' must be a character vector.")
  }
  if (! is.logical(print.html)) {
    stop("The input 'print.html' must be a logical.")
  }
  if (! is.character("html.filename")) {
    stop("The input 'html.filename' must be a character string.")
  }

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

  # Calculate CI if requested
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
      tbl[, ii] <- do.call(formatp, c(list(p = p),
                                      formatp.list))
      tbl.colnames[ii] <- "P"

    }

  }

  # Add column names
  colnames(tbl) <- tbl.colnames

  # Return table
  return(tbl)

}
