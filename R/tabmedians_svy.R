#' Create Table Comparing Group Medians (for Complex Survey Data)
#'
#' Creates a table comparing the median of \code{y} across levels of \code{x}.
#'
#' Basically \code{\link{tabmedians}} for complex survey data. Relies heavily on
#' the \pkg{survey} package.
#'
#'
#' @param formula Formula, e.g. \code{BMI ~ Sex}.
#' @param design Survey design object from \code{\link[survey]{svydesign}}.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall median, \code{"xgroups"} for \code{x} group medians, \code{"diff"}
#' for difference in \code{x} group medians (only available for binary
#' \code{x}), and \code{"p"} for p-value.
#' @param parenth Character string specifying what values are shown in
#' parentheses after the medians in each cell. Choices are \code{"none"},
#' \code{"iqr"}, \code{"q1q3"} for first and third quartiles, \code{"range"},
#' \code{"minmax"}, and \code{"ci"} for 95\% confidence interval for the median.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yname Character string with a label for the \code{y} variable.
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param svyranktest.list List of arguments to pass to
#' \code{\link[survey]{svyranktest}}.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display group sample sizes in
#' parentheses in column headings.
#' @param print.html Logical value for whether to write a .html file with the
#' table to the current working directory.
#' @param html.filename Character string specifying the name of the .html file
#' that gets written if \code{print.html = TRUE}.
#'
#'
#' @return Data frame which you can print in R (e.g. with \strong{xtable}'s
#' \code{\link[xtable]{xtable}} or \strong{knitr}'s \code{\link[knitr]{kable}})
#' or export to Word, Excel, or some other program. To export the table, set
#' \code{print.html = TRUE}. This will result in a .html file being written to
#' your current working directory, which you can open and copy/paste into your
#' document.
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
#' # Compare median BMI by sex
#' (medtable1 <- tabmedians.svy(BMI ~ Sex, design = design))
#'
#' # Create single table comparing median BMI and median age by sex. Drop missing
#' # observations first.
#' design2 <- subset(design, ! is.na(BMI) & ! is.na(Age) & ! is.na(Sex))
#' (medtable2 <- rbind(tabmedians.svy(BMI ~ Sex, design = design2),
#'                     tabmedians.svy(Age ~ Sex, design = design2)))
#'
#' # Same as previous, but using tabmulti for convenience
#' # (medtable3 <- tabmulti.svy(BMI + Age ~ Sex, data = tabsvydata))
#'
#' @export
tabmedians.svy <- function(formula, design,
                           columns = c("xgroups", "p"),
                           parenth = "iqr",
                           sep.char = ", ",
                           xlevels = NULL,
                           yname = NULL,
                           text.label = NULL,
                           decimals = NULL,
                           svyranktest.list = NULL,
                           formatp.list = NULL,
                           n.headings = TRUE,
                           print.html = FALSE,
                           html.filename = "table1.html") {

  # Error checking
  if (class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! "survey.design" %in% class(design)) {
    stop("The input 'design' must be a survey design object.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "diff", "diffci",
                           "diff.ci", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'diff', 'diffci', 'diff.ci', 'p'.")
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
  if (! is.logical(print.html)) {
    stop("The input 'print.html' must be a logical.")
  }
  if (! is.character("html.filename")) {
    stop("The input 'html.filename' must be a character string.")
  }

  # Get variable names etc.
  varnames <- all.vars(formula)
  xvarname <- varnames[2]
  yvarname <- varnames[1]
  if (is.null(yname)) yname <- yvarname

  # Drop missing values
  design <- eval(str2expression(paste("subset(design, ! is.na(", xvarname, ") & ! is.na(", yvarname, "))", sep = "")))

  # Extract x and y values
  x <- design$variables[, xvarname]
  y <- design$variables[, yvarname]

  # Calculate various statistics
  svyby.svyquantile <- svyby(as.formula(paste("~", yvarname, sep = "")),
                             by = as.formula(paste("~", xvarname, sep = "")),
                             design = design, FUN = svyquantile, quantiles = 0.5,
                             keep.var = FALSE, ci = TRUE)
  medians <- unlist(svyby.svyquantile$statistic.quantiles)
  lower <- sapply(svyby.svyquantile$statistic.CIs, function(x) x[1])
  upper <- sapply(svyby.svyquantile$statistic.CIs, function(x) x[2])
  ns <- table(x)

  xvals <- names(ns)
  num.groups <- length(xvals)

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) xlevels <- xvals

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

  # Hypothesis test
  fit <- do.call(svyranktest, c(list(formula = formula, design = design),
                                svyranktest.list))
  p <- fit$p.value

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

      df$N <- sum(ns)

    } else if (column == "overall") {

      svyquantile.q2 <- svyquantile(
        as.formula(paste("~", yvarname, sep = "")),
        design = design, quantiles = 0.5, keep.var = FALSE, ci = TRUE
      )
      median.y <- as.numeric(svyquantile.q2$quantiles)

      if (parenth == "none") {
        df$Overall <- sprintf(spf, median.y)
      } else if (parenth == "iqr") {
        svyquantile.q1q3 <- svyquantile(
          as.formula(paste("~", yvarname, sep = "")),
          design = design, quantiles = c(0.25, 0.75), keep.var = FALSE, ci = FALSE
        )
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, diff(as.numeric(svyquantile.q1q3))),
                            ")", sep = "")
      } else if (parenth == "q1q3") {
        svyquantile.q1q3 <- svyquantile(
          as.formula(paste("~", yvarname, sep = "")),
          design = design, quantiles = c(0.25, 0.75), keep.var = FALSE, ci = FALSE
        )
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, svyquantile.q1q3[1]), sep.char,
                            sprintf(spf, svyquantile.q1q3[2]), ")", sep = "")
      } else if (parenth == "range") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, diff(range(y))), ")", sep = "")
      } else if (parenth == "minmax") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, min(y)), sep.char,
                            sprintf(spf, max(y)), ")", sep = "")
      } else if (parenth == "ci") {
        df$Overall <- paste(sprintf(spf, median.y), " (",
                            sprintf(spf, svyquantile.q2$CIs[1]), sep.char,
                            sprintf(spf, svyquantile.q2$CIs[2]), ")", sep = "")
      }

    } else if (column == "xgroups") {

      if (parenth == "none") {
        newcols <- paste(sprintf(spf, medians))
      } else if (parenth == "iqr") {
        svyby.svyquantile.q1q3 <- svyby(
          as.formula(paste("~", yvarname, sep = "")),
          by = as.formula(paste("~", xvarname, sep = "")),
          design = design, FUN = svyquantile, quantiles = c(0.25, 0.75),
          keep.var = FALSE
        )
        iqrs <- svyby.svyquantile.q1q3$statistic2 - svyby.svyquantile.q1q3$statistic1
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, iqrs), ")", sep = "")
      } else if (parenth == "q1q3") {
        svyby.svyquantile.q1q3 <- svyby(
          as.formula(paste("~", yvarname, sep = "")),
          by = as.formula(paste("~", xvarname, sep = "")),
          design = design, FUN = svyquantile, quantiles = c(0.25, 0.75),
          keep.var = FALSE
        )
        q1s <- svyby.svyquantile.q1q3$statistic1
        q3s <- svyby.svyquantile.q1q3$statistic2
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
        lower <- sapply(svyby.svyquantile$statistic.CIs, function(x) x[1])
        upper <- sapply(svyby.svyquantile$statistic.CIs, function(x) x[2])
        newcols <- paste(sprintf(spf, medians), " (",
                         sprintf(spf, lower), sep.char,
                         sprintf(spf, upper), ")", sep = "")
      }
      names(newcols) <- xlevels
      df <- cbind(df, do.call(rbind, list(newcols)))

    } else if (column == "diff") {

      df$`Diff.` <- sprintf(spf, -diff(medians))

    } else if (column == "p") {

      df$P <- do.call(formatp, c(list(p = p), formatp.list))

    }

  }

  # Add sample sizes to column headings if requested
  if (n.headings) {

    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", sum(ns), ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (n = ", ns, ")", sep = "")

  }

  # Print html version of table if requested
  if (print.html) {

    df.xtable <- xtable(
      df,
      align = paste("ll", paste(rep("r", ncol(df) - 1), collapse = ""), sep = "", collapse = "")
    )
    print(df.xtable, include.rownames = FALSE, type = "html", file = html.filename)

  }

  # Return table
  return(df)

}
