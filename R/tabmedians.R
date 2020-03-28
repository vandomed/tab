#' Create Table Comparing Group Medians
#'
#' Creates a table comparing the median of \code{y} across levels of \code{x}.
#'
#' If \code{x} has 2 levels, a Mann-Whitney U (also known as Wilcoxon
#' rank-sum) test is used to test whether the distribution of \code{y} differs
#' in the two groups; if \code{x} has more than 2 levels, a Kruskal-Wallis test
#' is used to test whether the distribution of \code{y} differs across at
#' least two of the groups. Observations with missing values for \code{x} and/or
#' \code{y} are dropped.
#'
#'
#' @param formula Formula, e.g. \code{BMI ~ Group}.
#' @param data Data frame containing variables named in \code{formula}.
#' @param x Vector of values for the categorical \code{x} variable.
#' @param y Vector of values for the continuous \code{y} variable.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall median, \code{"xgroups"} for \code{x} group medians, \code{"diff"}
#' for difference in \code{x} group medians (only available for binary
#' \code{x}), \code{"test"} for test statistic, and \code{"p"} for p-value.
#' @param parenth Character string specifying what values are shown in
#' parentheses after the medians in each cell. Choices are \code{"none"},
#' \code{"iqr"}, \code{"q1q3"} for first and third quartiles, \code{"range"},
#' \code{"minmax"}, and \code{"ci"} for 95\% confidence interval for the medians
#' based on normal approximation to binomial.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yname Character string with a label for the \code{y} variable.
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent.
#' @param quantiles Numeric value. If specified, table compares \code{y} across
#' quantiles of \code{x} created on the fly.
#' @param quantile.vals Logical value for whether labels for \code{x} quantiles
#' should show quantile number and corresponding range, e.g. Q1 [0.00, 0.25),
#' rather than just the quantile number.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display group sample sizes in
#' parentheses in column headings.
#' @param kable Logical value for whether to return a
#' \code{\link[knitr]{kable}}.
#'
#'
#' @return \code{\link[knitr]{kable}}.
#'
#'
#' @examples
#' # Compare median BMI in control group vs. treatment group in sample dataset
#' (medtable1 <- tabmedians(BMI ~ Group, data = tabdata))
#'
#' # Compare median baseline systolic BP across tertiles of BMI
#' (medtable2 <- tabmedians(bp.1 ~ BMI, data = tabdata,
#'                          quantiles = 3, yname = "Systolic BP"))
#'
#'
#' @export
tabmedians <- function(formula = NULL,
                       data = NULL,
                       x = NULL,
                       y = NULL,
                       columns = c("xgroups", "p"),
                       parenth = "iqr",
                       sep.char = ", ",
                       xlevels = NULL,
                       yname = NULL,
                       text.label = NULL,
                       quantiles = NULL,
                       quantile.vals = FALSE,
                       decimals = NULL,
                       formatp.list = NULL,
                       n.headings = TRUE,
                       kable = TRUE) {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! is.null(data) && ! is.data.frame(data)) {
    stop("The input 'data' must be a data frame.")
  }
  if (! is.null(y) && ! (class(y) %in% c("numeric", "difftime"))) {
    stop("The input 'y' must be a numeric vector.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "diff", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'diff', 'test', 'p'.")
  }
  if (! parenth %in% c("none", "iqr", "q1q3", "range", "minmax", "ci")) {
    stop("The input 'parenth' must be one of the following: 'none', 'iqr', 'q1q3', 'range', 'minmax', 'ci'.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! is.null(xlevels) && ! is.character(xlevels)) {
    stop("The input 'xlevels' must be a character vector.")
  }
  if (! is.null(yname) && ! is.character(yname)) {
    stop("The input 'yname' must be a character string.")
  }
  if (! is.null(text.label) && ! is.character(text.label)) {
    stop("The input 'text.label' must be a character string.")
  }
  if (! is.null(quantiles) && ! (is.numeric(quantiles) && quantiles > 1 &&
                                 quantiles == as.integer(quantiles))) {
    stop("The input 'quantiles' must be an integer greater than 1.")
  }
  if (! is.logical(quantile.vals)) {
    stop("The input 'quantile.vals' must be a logical.")
  }
  if (! is.null(decimals) && ! (is.numeric(decimals) && decimals >= 0 &&
                                decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in%
                                      names(as.list(args(formatp)))))) {
    stop("The input 'formatp.list' must be a named list of arguments to pass to 'formatp'.")
  }
  if (! is.logical(n.headings)) {
    stop("The input 'n.headings' must be a logical.")
  }
  if (! is.logical(kable)) {
    stop("The input 'kable' must be a logical.")
  }

  # If formula specified, figure out x and y
  if (! is.null(formula)) {
    varnames <- all.vars(formula)
    xvarname <- varnames[2]
    yvarname <- varnames[1]
    x <- data[[xvarname]]
    y <- data[[yvarname]]
    if (is.null(yname)) {
      yname <- yvarname
    }
  } else {
    if (is.null(yname)) {
      yname <- deparse(substitute(y))
      if (grepl("\\$", yname)) {
        yname <- strsplit(yname, "\\$")[[1]][2]
      }
    }
  }

  # If yname unspecified, use variable name
  if (is.null(yname)) {
    yname <- deparse(substitute(y))
    if (grepl("\\$", yname)) {
      yname <- strsplit(yname, "\\$")[[1]][2]
    }
  }

  # Drop missing values
  locs.complete <- which(! is.na(x) & ! is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]

  # Create quantiles if necessary
  if (! is.null(quantiles)) {
    x <- cut(x = x, breaks = quantile(x, probs = seq(0, 1, 1 / quantiles)),
             include.lowest = TRUE, right = TRUE, dig.lab = 3)
  }

  # Calculate various statistics
  medians <- tapply(X = y, INDEX = x, FUN = median)
  ns <- tapply(X = y, INDEX = x, FUN = length)

  median.y <- median(y)
  n <- sum(ns)

  xvals <- names(medians)
  num.groups <- length(medians)

  # If decimals is unspecified, set to a reasonable value
  if (is.null(decimals)) {
    max.median <- max(abs(medians))
    if (max.median >= 1000 | max.median == 0) {
      decimals <- 0
    } else if (max.median < 1000 & max.median >= 10) {
      decimals <- 1
    } else if (max.median < 10 & max.median >= 0.1) {
      decimals <- 2
    } else if (max.median < 0.1 & max.median >= 0.01) {
      decimals <- 3
    } else if (max.median < 0.01 & max.median >= 0.001) {
      decimals <- 4
    } else if (max.median < 0.001 & max.median >= 0.0001) {
      decimals <- 5
    } else if (max.median < 0.0001) {
      decimals <- 6
    }
  }
  spf <- paste("%0.", decimals, "f", sep = "")

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    if (! is.null(quantiles)) {
      if (quantile.vals) {
        xlevels <- paste("Q", 1: num.groups, " ", xvals, sep = "")
      } else {
        xlevels <- paste("Q", 1: num.groups, sep = "")
      }
    } else {
      xlevels <- xvals
    }
  }

  # Hypothesis test
  if (length(xlevels) == 2) {

    # Mann-Whitney U test a.k.a. Wilcoxon rank-sum test
    fit <- wilcox.test(y ~ x)
    message(paste("Mann-Whitney U was used to test whether the distribution of ",
                  yname, " differs in the two groups.", sep = ""))
    test.stat <- fit$statistic
    test.label <- "W"
    p <- fit$p.value

  } else {

    # Kruskal-Wallis rank-sum test
    fit <- kruskal.test(y ~ as.factor(x))
    message(paste("Kruskal-Wallis was used to test whether the distribution of ",
                  yname, " differs across at least two of the groups.",
                  sep = ""))
    test.stat <- fit$statistic
    test.label <- "Chi-sq"
    p <- fit$p.value

  }

  # Figure out text.label for first column of table
  if (is.null(text.label)) {
    if (parenth == "none") {
      text.label <- ", Median"
    } else if (parenth == "iqr") {
      text.label <- ", Median (IQR)"
    } else if (parenth == "range") {
      text.label <- ", Median (range)"
    } else if (parenth == "minmax") {
      text.label <- paste(", Median (min", sep.char, "max)", sep = "")
    } else if (parenth == "q1q3") {
      text.label <- paste(", Median (Q1", sep.char, "Q3)", sep = "")
    } else if (parenth == "ci") {
      text.label <- ", Median (95% CI)"
    }
  } else {
    text.label <- paste(",", text.label)
  }

  # Initialize table
  df <- data.frame(Variable = paste(yname, text.label, sep = ""))

  # Loop through and add columns requested
  for (column in columns) {

    if (column == "n") {

      df$N <- n

    } else if (column == "overall") {

      if (parenth == "none") {
        df$Overall <- sprintf(spf, median.y)
      } else if (parenth == "iqr") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, IQR(y)), ")", sep = "")
      } else if (parenth == "q1q3") {
        df$Overall <-
          paste(sprintf(spf, median.y), " (",
                sprintf(spf, quantile(y, probs = 0.25)), sep.char,
                sprintf(spf, quantile(y, probs = 0.75)), ")", sep = "")
      } else if (parenth == "range") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, diff(range(y))), ")", sep = "")
      } else if (parenth == "minmax") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, min(y)), sep.char,
                            sprintf(spf, max(y)), ")", sep = "")
      } else if (parenth == "ci") {

        zcrit <- qnorm(p = 0.975)
        sort.y <- sort(y)
        lower <- sort.y[n / 2 - zcrit * sqrt(n) / 2]
        upper <- sort.y[1 + n / 2 + zcrit * sqrt(n) / 2]
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, lower), sep.char,
                            sprintf(spf, upper), ")", sep = "")

      }

    } else if (column == "xgroups") {

      if (parenth == "none") {
        newcols <- paste(sprintf(spf, medians))
      } else if (parenth == "iqr") {
        iqrs <- tapply(X = y, INDEX = x, FUN = IQR)
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, iqrs), ")", sep = "")
      } else if (parenth == "q1q3") {
        q1s <- tapply(X = y, INDEX = x, FUN = function(x) {
          quantile(x, probs = 0.25)
        })
        q3s <- tapply(X = y, INDEX = x, FUN = function(x) {
          quantile(x, probs = 0.75)
        })
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, q1s), sep.char,
                         sprintf(spf, q3s), ")", sep = "")
      } else if (parenth == "range") {
        ranges <- tapply(X = y, INDEX = x, FUN = function(x) diff(range(x)))
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, ranges), ")", sep = "")
      } else if (parenth == "minmax") {
        mins <- tapply(X = y, INDEX = x, FUN = min)
        maxes <- tapply(X = y, INDEX = x, FUN = max)
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, mins), sep.char,
                         sprintf(spf, maxes), ")", sep = "")
      } else if (parenth == "ci") {
        zcrit <- qnorm(p = 0.975)
        lower <- tapply(X = y, INDEX = x, FUN = function(x) {
          sort(x)[length(x) / 2 - zcrit * sqrt(length(x)) / 2]
        })
        upper <- tapply(X = y, INDEX = x, FUN = function(x) {
          sort(x)[1 + length(x) / 2 + zcrit * sqrt(length(x)) / 2]
        })
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, lower), sep.char,
                         sprintf(spf, upper), ")", sep = "")
      }
      names(newcols) <- xlevels
      df <- cbind(df, do.call(rbind, list(newcols)))

    } else if (column == "diff") {

      df$`Diff.` <- sprintf(spf, -diff(medians))

    } else if (column == "test") {

      newcol <- data.frame(sprintf(spf, test.stat))
      names(newcol) <- test.label
      df <- cbind(df, newcol)

    } else if (column == "p") {

      df$P <- do.call(formatp, c(list(p = p), formatp.list))

    }

  }

  # Add sample sizes to column headings if requested
  if (n.headings) {

    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", n, ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (n = ", ns, ")", sep = "")

  }

  # Return table
  if (! kable) return(df)
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
