#' Create Regression Table from Betas and Standard Errors
#'
#' Useful for quickly creating a summary table.
#'
#'
#' @param betas Numeric vector.
#' @param ses Numeric vector.
#' @param varcov Numeric matrix.
#' @param columns Character vector specifying what columns to include. Choices
#' are \code{"beta"}, \code{"se"}, \code{"betaci"}, \code{"beta.se"},
#' \code{"beta.ci"}, \code{"or"}, \code{"orci"}, \code{"or.ci"}, and \code{"p"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param labels Character vector.
#'
#'
#' @return \code{\link[knitr]{kable}}.
#'
#'
#' @examples
#' # Create summary table for mtcars regression
#' fit <- lm(mpg ~ wt + hp + drat, data = mtcars)
#' tabreg(betas = fit$coef, varcov = vcov(fit),
#'        labels = c("Intercept", "Weight", "HP", "Rear axle ratio")) %>% kable()
#'
#'
#' @export
tabreg <- function(betas,
                   ses = NULL,
                   varcov = NULL,
                   columns = c("beta.se", "p"),
                   sep.char = ", ",
                   decimals = NULL,
                   formatp.list = NULL,
                   labels = NULL) {

  # Error checking
  if (! is.numeric(betas)) {
    stop("The input 'betas' must be a numeric vector.")
  }
  if (! is.null(ses) && ! is.numeric(ses)) {
    stop("The input 'ses' must be a numeric vector.")
  }
  if (! is.null(varcov) && ! (is.matrix(varcov) && ncol(varcov) == nrow(varcov))) {
    stop("The input 'varcov' must be a square numeric matrix.")
  }
  if (! is.null(columns) &&
      ! all(columns %in% c("beta", "se", "betaci", "beta.se", "beta.ci", "or",
                           "orci", "or.ci", "hr", "hrci", "hr.ci", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'beta', 'se', 'betaci', 'beta.se', 'beta.ci', 'or', 'orci', 'or.ci', 'hr', 'hrci', 'hr.ci', 'test', 'p'.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! is.null(decimals) && ! (is.numeric(decimals) && decimals >= 0 &&
                                decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in% names(as.list(args(formatp)))))) {
    stop("The input 'format.p' must be a named list of arguments to pass to 'formatp'.")
  }
  if (! is.null(labels) && ! is.character(labels)) {
    stop("The input 'labels' must be a character vector.")
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

  # If CI coverage requested, get z value
  if (any(c("betaci", "beta.ci", "orci", "or.ci", "p") %in% columns)) {
    zcrit <- qnorm(0.975)
    lower <- betas - zcrit * ses
    upper <- betas + zcrit * ses
  }

  # Create labels if necessary
  if (is.null(labels)) {
    labels <- names(betas)
    if (is.null(labels)) {
      labels <- paste("beta", 0: (length(betas) - 1), sep = "_")
    }
  }

  # Initialize table
  df <- data.frame(Variable = labels, stringsAsFactors = FALSE)

  # Loop through and add columns
  for (column in columns) {


    if (column == "beta") {

      df$`Beta` <- sprintf(spf, betas)

    } else if (column == "se") {

      df$`SE` <- sprintf(spf, ses)

    } else if (column == "betaci") {

      df$`95% CI` <- paste("(", sprintf(spf, lower), sep.char,
                           sprintf(spf, upper), ")", sep = "")

    } else if (column == "beta.se") {

      df$`Beta (SE)` <- paste(sprintf(spf, betas), " (",
                              sprintf(spf, ses), ")", sep = "")

    } else if (column == "beta.ci") {

      df$`Beta (95% CI)` <- paste(sprintf(spf, betas), " (",
                                  sprintf(spf, lower), sep.char,
                                  sprintf(spf, upper), ")", sep = "")

    } else if (column == "or") {

      df$`OR` <- sprintf(spf, exp(betas))
      if (labels[1] %in% c("(Intercept)", "b0")) df$`OR`[1] <- "-"

    } else if (column == "orci") {

      df$`95% CI` <- paste("(", sprintf(spf, exp(lower)), sep.char,
                           sprintf(spf, exp(upper)), ")", sep = "")
      if (labels[1] %in% c("(Intercept)", "b0")) df$`95% CI`[1] <- "-"

    } else if (column == "or.ci") {

      df$`OR (95% CI)` <- paste(sprintf(spf, exp(betas)), " (",
                                sprintf(spf, exp(lower)), sep.char,
                                sprintf(spf, exp(upper)), ")", sep = "")
      if (labels[1] %in% c("(Intercept)", "b0")) df$`OR (95% CI)`[1] <- "-"

    } else if (column == "hr") {

      df$`HR` <- sprintf(spf, exp(betas))
      if (labels[1] %in% c("(Intercept)", "b0")) df$`HR`[1] <- "-"

    } else if (column == "hrci") {

      df$`95% CI` <- paste("(", sprintf(spf, exp(lower)), sep.char,
                           sprintf(spf, exp(upper)), ")", sep = "")
      if (labels[1] %in% c("(Intercept)", "b0")) df$`95% CI`[1] <- "-"

    } else if (column == "hr.ci") {

      df$`HR (95% CI)` <- paste(sprintf(spf, exp(betas)), " (",
                                sprintf(spf, exp(lower)), sep.char,
                                sprintf(spf, exp(upper)), ")", sep = "")
      if (labels[1] %in% c("(Intercept)", "b0")) df$`HR (95% CI)`[1] <- "-"

    } else if (column == "p") {

      df$`P` <- do.call(formatp, c(list(p = pnorm(-abs(betas / ses)) * 2),
                                   formatp.list))

    }

  }

  # Remove parentheses around intercept
  if (df$Variable[1] == "(Intercept)") df$Variable[1] <- "Intercept"

  # Remove row names and return table
  rownames(df) <- NULL
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
