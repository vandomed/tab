#' Create Table Comparing Characteristics Across Levels of a Categorical
#' Variable
#'
#' Creates a table comparing multiple characteristics (e.g. median age, mean
#' BMI, and race/ethnicity distribution) across levels of \code{x}.
#'
#'
#' @param formula Formula, e.g. \code{Age + Sex + Race + BMI ~ Group}.
#' @param data Data frame containing variables named in \code{formula}.
#' @param xvarname Character string with name of column variable. Should be one
#' of \code{names(data)}.
#' @param yvarnames Character vector with names of row variables. Each element
#' should be one of \code{names(data)}.
#' @param ymeasures Character vector specifying whether each \code{y} variable
#' should be summarized by mean, median, or frequency. For example, if you want
#' to compare frequencies for the first variable, means for the second, and
#' medians for the third, you would set
#' \code{ymeasures = c("freq", "mean", "median")}. If unspecified, function
#' compares means for numeric variables and frequencies for factor and character
#' variables.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall statistics, \code{"xgroups"} for \code{x} group statistics,
#' \code{"test"} for test statistic, and \code{"p"} for p-value.
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
#' @param quantiles Numeric value. If specified, function compares \code{y}
#' variables across quantiles of \code{x}. For example, if \code{x} contains BMI
#' values and \code{yvarnames} includes HDL and race, setting
#' \code{quantiles = 3} compares mean BMI and distribution of race across BMI
#' tertiles.
#' @param quantile.vals Logical value for whether labels for \code{x} quantiles
#' should show quantile number and corresponding range, e.g. Q1 [0.00, 0.25),
#' rather than just the quantile number.
#' @param decimals Numeric vector specifying number of decimal places for
#' numbers other than p-values for each \code{y} variable. Can be a single value
#' to use for all \code{y} variables.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display group sample sizes in
#' parentheses in column headings.
#' @param kable Logical value for whether to return a
#' \code{\link[knitr]{kable}}.
#' @param tabmeans.list List of arguments to pass to \code{\link{tabmeans}}.
#' @param tabmedians.list List of arguments to pass to \code{\link{tabmedians}}.
#' @param tabfreq.list List of arguments to pass to \code{\link{tabfreq}}.
#'
#'
#' @return \code{\link[knitr]{kable}} or character matrix.
#'
#'
#' @examples
#' # Compare age, sex, race, and BMI in control vs. treatment group
#' tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata)
#'
#' # Same as previous, but compare medians rather than means for BMI
#' tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata,
#'          ymeasures = c("mean", "freq", "freq", "median"))
#'
#'
#' @export
tabmulti <- function(formula = NULL,
                     data,
                     xvarname = NULL,
                     yvarnames = NULL,
                     ymeasures = NULL,
                     columns = c("xgroups", "p"),
                     listwise.deletion = FALSE,
                     sep.char = ", ",
                     xlevels = NULL,
                     yvarlabels = NULL,
                     ylevels = NULL,
                     quantiles = NULL,
                     quantile.vals = FALSE,
                     decimals = NULL,
                     formatp.list = NULL,
                     n.headings = FALSE,
                     tabmeans.list = NULL,
                     tabmedians.list = NULL,
                     tabfreq.list = NULL,
                     kable = TRUE) {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! is.data.frame(data)) {
    stop("The input 'data' must be a data frame.")
  }
  if (! is.null(xvarname) && ! xvarname %in% names(data)) {
    stop("The input 'xvarname' must be a character string matching one of the variables in 'data'.")
  }
  if (! is.null(yvarnames) && ! all(yvarnames %in% names(data))) {
    stop("Each element of 'yvarnames' must be a character string matching one of the variables in 'data'.")
  }
  if (! is.null(ymeasures) && ! all(ymeasures %in% c("freq", "mean", "median"))) {
    stop("Each element of 'ymeasures' must be one of the following: 'freq', 'mean', 'median'.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'test', 'p'.")
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
  if (! is.null(ylevels) && ! is.character(ylevels) &&
      (is.list(ylevels) &&
       (!all(vapply(ylevels, is.character, logical(1))) ||
        length(ylevels) != sum(ymeasures == "freq")))) {
    stop("The input 'ylevels' must be a character vector or ",
         "list of character vectors with as much elements as ",
         "'freq' in 'ymeasures'.")
  }
  if (! is.null(quantiles) && ! (is.numeric(quantiles) && quantiles > 1 &&
                                 quantiles == as.integer(quantiles))) {
    stop("The input 'quantiles' must be an integer greater than 1.")
  }
  if ( ! is.logical(quantile.vals)) {
    stop("The input 'quantile.vals' must be a logical.")
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
  if (! is.null(tabmeans.list) &&
      ! (is.list(tabmeans.list) && all(names(tabmeans.list) %in%
                                       names(as.list(args(tabmeans)))))) {
    stop("The input 'tabmeans.list' must be a named list of arguments to pass to 'tabmeans'.")
  }
  if (! is.null(tabmedians.list) &&
      ! (is.list(tabmedians.list) && all(names(tabmedians.list) %in%
                                         names(as.list(args(tabmedians)))))) {
    stop("The input 'tabmedians.list' must be a named list of arguments to pass to 'tabmedians'.")
  }
  if (! is.null(tabfreq.list) &&
      ! (is.list(tabfreq.list) && all(names(tabfreq.list) %in%
                                      names(as.list(args(tabfreq)))))) {
    stop("The input 'tabfreq.list' must be a named list of arguments to pass to 'tabfreq'.")
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
    data <- subset(data, complete.cases(data[, c(xvarname, yvarnames)]))
  }

  # Create x vector
  x <- data[[xvarname]]

  # Number of y variables
  num.yvars <- length(yvarnames)

  # If ymeasures is NULL, compare frequencies for factor/character variables and
  # means for numeric variables
  if (is.null(ymeasures)) {
    ymeasures <- ifelse(sapply(data[, yvarnames, drop = FALSE], class) == "numeric", "mean", "freq")
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

  # Call tabmeans, tabmedians, or tabfreq repeatedly
  mediansindex <- 0
  meansindex <- 0
  freqindex <- 0
  for (ii in 1: num.yvars) {
    ymeasures.ii <- ymeasures[ii]

    if (ymeasures.ii == "mean") {

      # Means
      meansindex <- meansindex + 1
      args1 <- list(x = x,
                    y = data[[yvarnames[ii]]],
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    quantiles = quantiles,
                    quantile.vals = quantile.vals,
                    decimals = decimals[ii],
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    kable = FALSE)
      current <- do.call(tabmeans, c(args1, tabmeans.list))

    } else if (ymeasures.ii == "median") {

      # Medians
      mediansindex <- mediansindex + 1
      args1 <- list(x = x,
                    y = data[[yvarnames[ii]]],
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    quantiles = quantiles,
                    quantile.vals = quantile.vals,
                    decimals = decimals[ii],
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    kable = FALSE)
      current <- do.call(tabmedians, c(args1, tabmedians.list))

    } else if (ymeasures.ii == "freq") {

      # Frequencies
      freqindex <- freqindex + 1
      args1 <- list(x = x,
                    y = data[[yvarnames[ii]]],
                    columns = columns,
                    sep.char = sep.char,
                    xlevels = xlevels,
                    yname = ynames[ii],
                    ylevels = ylevels[[freqindex]],
                    quantiles = quantiles,
                    quantile.vals = quantile.vals,
                    decimals = ifelse(is.null(decimals[ii]), 1, decimals[ii]),
                    formatp.list = formatp.list,
                    n.headings = n.headings,
                    kable = FALSE)
      current <- do.call(tabfreq, c(args1, tabfreq.list))

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
