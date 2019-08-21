#' Create Table Comparing Group Means
#'
#' Creates a table comparing the mean of \code{y} across levels of \code{x}.
#'
#' A t-test is used to compare means if \code{x} has two levels, and a one-way
#' analysis of variance is used if \code{x} has more than two levels.
#' Observations with missing values for \code{x} and/or \code{y} are dropped.
#'
#'
#' @param formula Formula, e.g. \code{BMI ~ Group}.
#' @param data Data frame containing variables named in \code{formula}.
#' @param x Vector of values for the categorical \code{x} variable.
#' @param y Vector of values for the continuous \code{y} variable.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall mean, \code{"xgroups"} for \code{x} group means, \code{"diff"} for
#' difference in \code{x} group means (this one and the next two are only
#' available for binary \code{x}), \code{"diffci"} for 95% CI for difference in
#' \code{x} group means, \code{"diff.ci"} for difference in group means and 95%
#' confidence interval, \code{"test"} for test statistic, and \code{"p"} for
#' p-value.
#' @param parenth Character string specifying what statistic to display in
#' parentheses after the means. Choices are \code{"none"}, \code{"sd"},
#' \code{"se"},  \code{"t.ci"}, \code{"z.ci"}, \code{"range"}, and
#' \code{"minmax"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param variance Character string specifying which version of the two-sample
#' t-test to use if \code{x} has 2 levels. Choices are \code{"equal"} for equal
#' variance t-test, \code{"unequal"} for unequal variance t-test, and \code{"f"}
#' for F test to determine which to use.
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
#' # Compare mean BMI in control vs. treatment group in sample dataset
#' (meanstable1 <- tabmeans(BMI ~ Group, data = tabdata))
#'
#' # Same as previous, but specifying input vectors rather than formula
#' (meanstable2 <- tabmeans(x = tabdata$Group, y = tabdata$BMI))
#'
#' # Compare mean baseline systolic BP across tertiles of BMI
#' (meanstable3 <- tabmeans(bp.1 ~ BMI, data = tabdata,
#'                          quantiles = 3, yname = "Systolic BP"))
#'
#' # Create single table comparing mean BMI and mean age in control vs.
#' # treatment group. Drop missing observations first.
#' tabdata2 <- subset(tabdata, ! is.na(BMI) & ! is.na(Age))
#' (meanstable4 <- rbind(tabmeans(BMI ~ Group, data = tabdata2),
#'                       tabmeans(Age ~ Group, data = tabdata2)))
#'
#' # Same as previous, but using tabmulti for convenience
#' (meanstable5 <- tabmulti(BMI + Age ~ Group, data = tabdata))
#'
#'
#' @export
tabmeans <- function(formula = NULL,
                     data = NULL,
                     x = NULL,
                     y = NULL,
                     columns = c("xgroups", "p"),
                     parenth = "sd",
                     sep.char = ", ",
                     variance = "unequal",
                     xlevels = NULL,
                     yname = NULL,
                     text.label = NULL,
                     quantiles = NULL,
                     quantile.vals = FALSE,
                     decimals = NULL,
                     formatp.list = NULL,
                     n.headings = TRUE,
                     print.html = FALSE,
                     html.filename = "table1.html") {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! is.null(data) && ! is.data.frame(data)) {
    stop("The input 'data' must be a data frame.")
  }
  if (! is.null(y) && ! is.numeric(y)) {
    stop("The input 'y' must be a numeric vector.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "diff", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'diff', 'test', 'p'.")
  }
  if (! parenth %in% c("none", "sd", "se", "t.ci", "z.ci", "range", "minmax")) {
    stop("The input 'parenth' must be one of the following: 'none', 'sd', 'se', 't.ci', 'z.ci', 'range', 'minmax'.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! variance %in% c("equal", "unequal", "f")) {
    stop("The input 'variance' must be one of the following: 'equal', 'unequal', 'f'.")
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
  if (! is.logical(print.html)) {
    stop("The input 'print.html' must be a logical.")
  }
  if (! is.character("html.filename")) {
    stop("The input 'html.filename' must be a character string.")
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
  means <- tapply(X = y, INDEX = x, FUN = mean)
  sds <- tapply(X = y, INDEX = x, FUN = sd)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  n <- sum(ns)
  ses <- sds / sqrt(ns)

  xvals <- names(means)
  num.groups <- length(means)

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

  # If decimals is unspecified, set to a reasonable value
  if (is.null(decimals)) {
    max.mean <- max(abs(means))
    if (max.mean >= 1000) {
      decimals <- 0
    } else if (max.mean < 1000 & max.mean >= 10) {
      decimals <- 1
    } else if (max.mean < 10 & max.mean >= 0.1) {
      decimals <- 2
    } else if (max.mean < 0.1 & max.mean >= 0.01) {
      decimals <- 3
    } else if (max.mean < 0.01 & max.mean >= 0.001) {
      decimals <- 4
    } else if (max.mean < 0.001 & max.mean >= 0.0001) {
      decimals <- 5
    } else if (max.mean < 0.0001) {
      decimals <- 6
    }
  }
  spf <- paste("%0.", decimals, "f", sep = "")

  # Hypothesis test
  if (length(xlevels) == 2) {

    if (variance == "equal") {
      fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                    var.equal = TRUE)
    } else if (variance == "unequal") {
      fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                    var.equal = FALSE)
    } else if (variance == "f") {
      f <- var.test(x = y[x == xvals[1]], y = y[x == xvals[2]])
      if (f$p.value < 0.05) {
        fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = FALSE)
        message(paste("Unequal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
      } else {
        fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = TRUE)
        message(paste("Equal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
      }
    }
    diffmeans <- -diff(fit$estimate)
    diffmeans.ci <- fit$conf.int
    test.stat <- fit$statistic
    test.label <- "t"
    p <- fit$p.value

  } else {

    # ANOVA
    fit <- anova(lm(y ~ as.factor(x)))
    test.stat <- fit[["F value"]][1]
    test.label <- "F"
    p <- fit[["Pr(>F)"]][1]

  }

  # Figure out text.label for first column of table
  if (is.null(text.label)) {
    if (parenth == "none") {
      text.label <- "M"
    } else if (parenth == "sd") {
      text.label <- ", M (SD)"
    } else if (parenth == "se") {
      text.label <- ", M (SE)"
    } else if (parenth %in% c("t.ci", "z.ci")) {
      text.label <- ", M (95% CI)"
    } else if (parenth == "range") {
      text.label <- ", M (range)"
    } else if (parenth == "minmax") {
      text.label <- paste(", M (min", sep.char, "max)", sep = "")
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

      mean.y <- mean(y)
      sd.y <- sd(y)
      se.y <- sd.y / n

      if (parenth == "none") {
        df$Overall <- sprintf(spf, mean.y)
      } else if (parenth == "sd") {
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, sd.y), ")", sep = "")
      } else if (parenth == "se") {
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, se.y), ")", sep = "")
      } else if (parenth == "t.ci") {
        tcrit <- qt(p = 0.975, df = n - 1)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, mean.y - tcrit * se.y), sep.char,
                            sprintf(spf, mean.y + tcrit * se.y), ")", sep = "")
      } else if (parenth == "z.ci") {
        zcrit <- qnorm(p = 0.975)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, mean.y - zcrit * se.y), sep.char,
                            sprintf(spf, mean.y + zcrit * se.y), ")", sep = "")
      } else if (parenth == "range") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf.r, diff(range(y))), ")", sep = "")
      } else if (parenth == "minmax") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf.r, min(y)), sep.char,
                            sprintf(spf.r, max(y)), ")", sep = "")
      }

    } else if (column == "xgroups") {

      if (parenth == "none") {
        newcols <- paste(sprintf(spf, means))
      } else if (parenth == "sd") {
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, sds), ")", sep = "")
      } else if (parenth == "se") {
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, ses), ")", sep = "")
      } else if (parenth == "t.ci") {
        tcrits <- qt(p = 0.975, df = ns - 1)
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, means - tcrits * ses), sep.char,
                         sprintf(spf, means + tcrits * ses), ")", sep = "")
      } else if (parenth == "z.ci") {
        zcrit <- qnorm(p = 0.975)
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, means - zcrit * ses), sep.char,
                         sprintf(spf, means + zcrit * ses), ")", sep = "")
      } else if (parenth == "range") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        ranges <- tapply(X = y, INDEX = x, FUN = function(x) diff(range(x)))
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf.r, ranges), ")", sep = "")
      } else if (parenth == "minmax") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        mins <- tapply(X = y, INDEX = x, FUN = min)
        maxes <- tapply(X = y, INDEX = x, FUN = max)
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf.r, mins), sep.char,
                         sprintf(spf.r, maxes), ")", sep = "")
      }
      names(newcols) <- xlevels
      df <- cbind(df, do.call(rbind, list(newcols)))

    } else if (column == "diff") {

      df$`Diff.` <- sprintf(spf, diffmeans)

    } else if (column == "diffci") {

      df$`95% CI for Diff.` <- paste(sprintf(spf, diffmeans.ci[1]), sep.char,
                                     sprintf(spf, diffmeans.ci[2]), sep = "")

    } else if (column == "diff.ci") {

      df$`Diff. (95% CI)` <- paste(sprintf(spf, diffmeans), " (",
                                   sprintf(spf, diffmeans.ci[1]), sep.char,
                                   sprintf(spf, diffmeans.ci[2]), ")", sep = "")

    } else if (column == "test") {

      newcol <- data.frame(sprintf(spf, test.stat))
      names(newcol) <- test.label
      df <- cbind(df, newcol)
      #df <- mutate(df, !!as_name(test.label) := test.stat)

    } else if (column == "p") {

      df$P <- do.call(formatp, c(list(p = p), formatp.list))

    }

  }

  # Add sample sizes to column headings if requested
  if (n.headings) {

    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", n, ")", sep = "")
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

