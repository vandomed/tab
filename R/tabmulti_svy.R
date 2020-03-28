#' Create Table Comparing Characteristics Across Levels of a Categorical
#' Variable (for Complex Survey Data)
#'
#' Creates a table comparing multiple characteristics (e.g. median age, mean
#' BMI, and race/ethnicity distribution) across levels of \code{x}.
#'
#' Basically \code{\link{tabmulti}} for complex survey data. Relies heavily on
#' the \pkg{survey} package.
#'
#'
#' @param formula Formula, e.g. \code{Age + Race + BMI ~ Sex}.
#' @param design Survey design object from \code{\link[survey]{svydesign}}.
#' @param xvarname Character string with name of column variable. Should be one
#' of \code{names(design$variables)}.
#' @param yvarnames Character vector with names of row variables. Each element
#' should be one of \code{names(design$variables)}.
#' @param ymeasures Character vector specifying whether each \code{y} variable
#' should be summarized by mean, median, or frequency. For example, if you want
#' to compare frequencies for the first variable, means for the second, and
#' medians for the third, you would set
#' \code{ymeasures = c("freq", "mean", "median")}. If unspecified, function
#' compares means for numeric variables and frequencies for factor and character
#' variables.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for unweighted sample size, \code{"N"} for
#' weighted sample size, \code{"overall"} for overall statistics,
#' \code{"xgroups"} for \code{x} group statistics, and \code{"p"} for p-value.
#' @param listwise.deletion Logical value for whether observations with missing
#' values for any \code{y} variable should be excluded entirely (as opposed to
#' using all available data for each comparison).
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yvarlabels Named list specifying labels for certain \code{y}
#' variables. For example, if you want variables named "race" and "age_yrs" to
#' print as "Race/ethnicity" and "Age (years)", use
#' \\code{yvarlabels = list(race = "Race/ethnicity", age_yrs = "Age (years)")}.
#' @param ylevels Character vector (if only 1 frequency comparison) or list of
#' character vectors with labels for the levels of each categorical \code{y}
#' variable.
#' @param decimals Numeric vector specifying number of decimal places for
#' numbers other than p-values for each \code{y} variable. Can be a single value
#' to use for all \code{y} variables.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display unweighted sample
#' sizes in parentheses in column headings.
#' @param N.headings Logical value for whether to display weighted sample sizes
#' in parentheses in column headings.
#' @param kable Logical value for whether to return a
#' \code{\link[knitr]{kable}}.
#' @param tabmeans.svy.list List of arguments to pass to
#' \code{\link{tabmeans.svy}}.
#' @param tabmedians.svy.list List of arguments to pass to
#' \code{\link{tabmedians.svy}}.
#' @param tabfreq.svy.list List of arguments to pass to
#' \code{\link{tabfreq.svy}}.
#'
#'
#' @return \code{\link[knitr]{kable}} or character matrix.
#'
#'
#' @examples
#' # Create survey design object
#' library("survey")
#' design <- svydesign(
#'   data = tabsvydata,
#'   ids = ~sdmvpsu,
#'   strata = ~sdmvstra,
#'   weights = ~wtmec2yr,
#'   nest = TRUE
#' )
#'
#' # Compare age, race, and BMI by sex
#' tabmulti.svy(Age + Race + BMI ~ Sex, design = design) %>% kable()
#'
#'
#' @export
tabmulti.svy <- function(formula = NULL,
                         design,
                         xvarname = NULL,
                         yvarnames = NULL,
                         ymeasures = NULL,
                         columns = c("xgroups", "p"),
                         listwise.deletion = FALSE,
                         sep.char = ", ",
                         xlevels = NULL,
                         yvarlabels = NULL,
                         ylevels = NULL,
                         decimals = NULL,
                         formatp.list = NULL,
                         n.headings = FALSE,
                         N.headings = FALSE,
                         kable = TRUE,
                         tabmeans.svy.list = NULL,
                         tabmedians.svy.list = NULL,
                         tabfreq.svy.list = NULL) {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! "survey.design" %in% class(design)) {
    stop("The input 'design' must be a survey design object.")
  }
  if (! is.null(xvarname) && ! xvarname %in% names(design$variables)) {
    stop("The input 'xvarname' must be a character string matching one of the variables in 'design'.")
  }
  if (! is.null(yvarnames) && ! all(yvarnames %in% names(design$variables))) {
    stop("Each element of 'yvarnames' must be a character string matching one of the variables in 'design'.")
  }
  if (! is.null(ymeasures) && ! all(ymeasures %in% c("freq", "mean", "median"))) {
    stop("Each element of 'ymeasures' must be one of the following: 'freq', 'mean', 'median'.")
  }
  if (! all(columns %in% c("n", "N", "overall", "xgroups", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'N', 'overall', 'xgroups', 'p'.")
  }
  if (! is.logical(listwise.deletion)) {
    stop("The input 'listwise.deletion' must be a logical.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! is.null(xlevels) && ! is.character(xlevels)) {
    stop("The input 'xlevels' must be a character vector.")
  }
  if (! is.null(ylevels) && ! is.character(ylevels)) {
    stop("The input 'ylevels' must be a character vector.")
  }
  if (! is.null(decimals) && ! (is.numeric(decimals) && decimals >= 0 &&
                                decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in% names(as.list(args(formatp)))))) {
    stop("The input 'formatp.list' must be a named list of arguments to pass to 'formatp'.")
  }
  if (! is.logical(n.headings)) {
    stop("The input 'n.headings' must be a logical.")
  }
  if (! is.logical(N.headings)) {
    stop("The input 'N.headings' must be a logical.")
  }
  if (! is.logical(kable)) {
    stop("The input 'kable' must be a logical.")
  }
  if (! is.null(tabmeans.svy.list) &&
      ! (is.list(tabmeans.svy.list) && all(names(tabmeans.svy.list) %in%
                                       names(as.list(args(tabmeans.svy)))))) {
    stop("The input 'tabmeans.svy.list' must be a named list of arguments to pass to 'tabmeans.svy'.")
  }
  if (! is.null(tabmedians.svy.list) &&
      ! (is.list(tabmedians.svy.list) && all(names(tabmedians.svy.list) %in%
                                         names(as.list(args(tabmedians.svy)))))) {
    stop("The input 'tabmedians.svy.list' must be a named list of arguments to pass to 'tabmedians.svy'.")
  }
  if (! is.null(tabfreq.svy.list) &&
      ! (is.list(tabfreq.svy.list) && all(names(tabfreq.svy.list) %in%
                                      names(as.list(args(tabfreq.svy)))))) {
    stop("The input 'tabfreq.svy.list' must be a named list of arguments to pass to 'tabfreq.svy'.")
  }

  # Figure out x and y
  if (! is.null(formula)) {
    varnames <- all.vars(formula)
    xvarname <- varnames[length(varnames)]
    yvarnames <- varnames[-length(varnames)]
  }
  ynames <- unlist(sapply(yvarnames, function(x) ifelse(x %in% names(yvarlabels), yvarlabels[x], x)))

  # If listwise.deletion is TRUE, drop observations with missing values for
  # column variable or any row variables
  if (listwise.deletion){
    design <- subset(design, complete.cases(design$variables[, c(xvarname, yvarnames)]))
  }

  # Create x vector
  x <- design$variables[, xvarname]

  # Number of y variables
  num.yvars <- length(yvarnames)

  # If ymeasures is NULL, compare frequencies for factor/character variables and
  # means for numeric variables
  if (is.null(ymeasures)) {
    ymeasures <- ifelse(sapply(design$variables[, yvarnames], class) == "numeric", "mean", "freq")
  } else if (length(ymeasures) == 1) {
    ymeasures <- rep(ymeasures, num.yvars)
  }

  # If decimals is a single value, recycle as needed
  if (length(decimals) == 1) {
    decimals <- rep(decimals, num.yvars)
  }

  # If ylevels is a vector, convert to a list
  if (! is.null(ylevels) && ! is.list(ylevels)) {
    ylevels <- list(ylevels)
  }

  # Call tabmeans.svy, tabmedians.svy, or tabfreq.svy repeatedly
  mediansindex <- 0
  meansindex <- 0
  freqindex <- 0
  for (ii in 1: num.yvars) {
    ymeasures.ii <- ymeasures[ii]

    if (ymeasures.ii == "mean") {

      # Means
      meansindex <- meansindex + 1
      args1 <- list(formula = as.formula(paste(yvarnames[ii], " ~ ", xvarname, sep = "")),
                    design = design,
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    decimals = decimals[ii],
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    N.headings = N.headings,
                    kable = FALSE)
      current <- do.call(tabmeans.svy, c(args1, tabmeans.svy.list))

    } else if (ymeasures.ii == "median") {

      # Medians
      mediansindex <- mediansindex + 1
      args1 <- list(formula = as.formula(paste(yvarnames[ii], " ~ ", xvarname, sep = "")),
                    design = design,
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    decimals = decimals[ii],
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    N.headings = N.headings,
                    kable = FALSE)
      current <- do.call(tabmedians.svy, c(args1, tabmedians.svy.list))

    } else if (ymeasures.ii == "freq") {

      # Frequencies
      freqindex <- freqindex + 1
      args1 <- list(formula = as.formula(paste(yvarnames[ii], " ~ ", xvarname, sep = "")),
                    design = design,
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    ylevels = ylevels[[freqindex]],
                    decimals = ifelse(is.null(decimals[ii]), 1, decimals[ii]),
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    N.headings = N.headings,
                    kable = FALSE)
      current <- do.call(tabfreq.svy, c(args1, tabfreq.svy.list))

    }

    # Add to growing table
    if (ii == 1) {
      df <- current
    } else {
      df <- rbind(df, current)
    }
  }

  # Return table
  if (! kable) return(df)
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
