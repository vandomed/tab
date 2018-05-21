#' Generate Summary Table Comparing Group Means
#'
#' Creates table (or figure) comparing the mean of \code{y} across levels of
#' \code{x}.
#'
#' A t-test is used to compare means if \code{x} has two levels, and a one-way
#' analysis of variance is used if \code{x} has more than two levels.
#' Observations with missing values for \code{x} and/or \code{y} are dropped.
#'
#' @section Note:
#' If you want to paste your tables into Microsoft Word, you can use either of
#' these approaches:
#'
#' \enumerate{
#'
#' \item Use the \code{\link[Kmisc]{write.cb}} function in \pkg{Kmisc} [2].
#' If your table is stored in a character matrix named \code{table1}, use
#' \code{write.cb(table1)} to copy the table to your clipboard. Paste the result
#' into your document, then highlight the text and go to
#' \code{Insert -> Table -> Convert Text to Table... OK}.
#'
#' \item Set \code{print.html = TRUE}. This will result in a .html file
#' being written to your current working directory. When you open this file, you
#' will see a (hopefully) nice-looking table that you can copy and paste into
#' your document. You can control the name of this file with
#' \code{html.filename}.
#' }
#'
#' If you want to use LaTeX, R Markdown, knitr, Sweave, etc., set
#' \code{latex = TRUE}. Then, you can pass the returned object to
#' \code{\link[xtable]{xtable}} or \code{\link[knitr]{kable}} for printing. For
#' \code{\link[xtable]{xtable}}, you may have to set
#' \code{sanitize.text.function = identity} when printing.
#'
#'
#' @param x Vector of values for the categorical \code{x} variable.
#'
#' @param y Vector of values for the continuous \code{y} variable.
#'
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall mean, \code{"xgroups"} for \code{x} group means, \code{"diff"} for
#' difference in \code{x} group means (only available for binary \code{x}),
#' \code{"test"} for test statistic, and \code{"p"} for p-value.
#'
#' @param parenth Character string specifying what statistic to display in
#' parentheses after the means. Choices are \code{"none"}, \code{"sd"},
#' \code{"se"},  \code{"t.ci"}, \code{"z.ci"}, \code{"range"}, and
#' \code{"minmax"}.
#'
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#'
#' @param variance Character string specifying which version of the two-sample
#' t-test to use if \code{x} has 2 levels. Choices are \code{"equal"} for equal
#' variance t-test, \code{"unequal"} for unequal variance t-test, and \code{"f"}
#' for F test to determine which to use.
#'
#' @param xname Character string with a label for the \code{x} variable.
#'
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#'
#' @param yname Character string with a label for the \code{y} variable.
#'
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent.
#'
#' @param quantiles Numeric value. If specified, table compares \code{y} across
#' quantiles of \code{x} created on the fly.
#'
#' @param quantile.vals Logical value for whether labels for \code{x} quantiles
#' should show quantile number and corresponding range, e.g. Q1 [0.00, 0.25),
#' rather than just the quantile number.
#'
#' @param latex Logical value for whether to format table so it is
#' ready for printing in LaTeX via \code{\link[xtable]{xtable}} or
#' \code{\link[knitr]{kable}}.
#'
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#'
#' @param p.decimals Numeric value specifying number of decimal places for
#' p-values. Can be a vector if you want the number of decimals to depend on
#' what range the p-value is in. See \code{p.cuts}.
#'
#' @param p.cuts Numeric value or vector of cutpoints to control number of
#' decimal places for p-values. For example, by default \code{p.cuts = 0.1} and
#' \code{p.decimals = c(2, 3)}, meaning p-values in the range [0.1, 1] are
#' printed to 2 decimal places while p-values in the range [0, 0.1) are printed
#' to 3.
#'
#' @param p.lowerbound Numeric value specifying cutpoint beneath which p-values
#' appear as <p.lowerbound.
#'
#' @param p.leading0 Logical value for whether p-values should appear with
#' leading 0's before the decimal point.
#'
#' @param p.avoid1 Logical value for whether p-values that round to 1 should
#' appear as \code{>0.99} (or similarly depending on \code{p.decimals} and
#' \code{p.cuts}) rather than 1.
#'
#' @param n.headings Logical value for whether to display group sample sizes in
#' parentheses in column headings.
#'
#' @param variable.colname Character string with desired heading for first
#' column of table, in case you prefer something other than \code{"Variable"}.
#'
#' @param fig Logical value for whether to generate a figure rather than a
#' table.
#'
#' @param print.html Logical value for whether to write a .html file with the
#' table to the current working directory.
#'
#' @param html.filename Character string specifying the name of the .html file
#' that gets written if \code{print.html = TRUE}.
#'
#' @param ... Additional arguments to pass to \code{\link[graphics]{plot}}. Only
#' used if \code{fig = TRUE}.
#'
#'
#' @return Character matrix comparing mean \code{y} across levels of \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset and drop rows with missing values
#' data(tabdata)
#' tabdata <- tabdata[complete.cases(tabdata), ]
#'
#' # Compare mean BMI in control group vs. treatment group - table and figure
#' (meanstable1 <- tabmeans(x = tabdata$Group, y = tabdata$BMI))
#' meansfig1 <- tabmeans(x = tabdata$Group, y = tabdata$BMI, fig = TRUE)
#'
#' # Compare mean baseline systolic BP across tertiles of BMI - table and figure
#' (meanstable2 <- tabmeans(x = tabdata$BMI, y = tabdata$bp.1,
#'                          yname = "Systolic BP", quantiles = 3))
#' meansfig2 <- tabmeans(x = tabdata$BMI, y = tabdata$bp.1, quantiles = 3,
#'                       fig = TRUE, yname = "Systolic BP",
#'                       xname = "BMI Tertile")
#'
#' # Create single table comparing mean BMI and mean age in control vs.
#' # treatment group
#' (meanstable3 <- rbind(tabmeans(x = tabdata$Group, y = tabdata$BMI),
#'                       tabmeans(x = tabdata$Group, y = tabdata$Age)))
#'
#' # An easier way to make this table is to use tabmulti
#' (meanstable4 <- tabmulti(data = tabdata, xvarname = "Group",
#'                          yvarnames = c("BMI", "Age")))
#'
#'
#' @references
#'1. Dahl, D.B. (2016). xtable: Export Tables to LaTeX or HTML. R package
#'version 1.8-2, \url{https://cran.r-project.org/package=xtable}.
#'
#'2. Ushley, K. (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
#'\url{https://CRAN.R-project.org/package=Kmisc}.
#'
#'
#' @export
tabmeans <- function(x, y,
                     columns = c("xgroups", "p"),
                     parenth = "sd",
                     sep.char = ", ",
                     variance = "unequal",
                     xname = NULL,
                     xlevels = NULL,
                     yname = NULL,
                     text.label = NULL,
                     quantiles = NULL,
                     quantile.vals = FALSE,
                     latex = FALSE,
                     decimals = NULL,
                     p.decimals = c(2, 3),
                     p.cuts = 0.01,
                     p.lowerbound = 0.001,
                     p.leading0 = TRUE,
                     p.avoid1 = FALSE,
                     n.headings = TRUE,
                     variable.colname = "Variable",
                     fig = FALSE,
                     print.html = FALSE,
                     html.filename = "table1.html", ...) {

  # If xname or yname unspecified, use variable names
  if (is.null(xname) & fig) {
    xname <- deparse(substitute(x))
    if (grepl("\\$", xname, fixed = TRUE)) {
      xname <- strsplit(xname, "\\$")[[1]][2]
    }
  }
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
  means <- tapply(X = y, INDEX = x, FUN = mean)
  sds <- tapply(X = y, INDEX = x, FUN = sd)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  n <- sum(ns)
  ses <- sds / sqrt(ns)

  mean.y <- mean(y)
  sd.y <- sd(y)
  se.y <- sd.y / n

  xvals <- names(means)
  num.groups <- length(means)

  # If decimals is unspecified, set to appropriate value
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

  if (! fig) {

    # Hypothesis test
    if (length(xlevels) == 2) {

      if (variance == "equal") {
        fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = TRUE)
        message(paste("Equal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
      } else if (variance == "unequal") {
        fit <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = FALSE)
        message(paste("Unequal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
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
      message(paste("ANOVA was used to compare means for ", yname, sep = ""))
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
    tbl <- matrix(paste(yname, text.label, sep = ""), ncol = 1,
                  dimnames = list(NULL, variable.colname))

    # Convert decimals to variable for sprintf
    spf <- paste("%0.", decimals, "f", sep = "")

    # Loop through column input and add each
    for (ii in 1: length(columns)) {

      column.ii <- columns[ii]

      if (column.ii == "n") {

        # N
        newcol <- matrix(n, dimnames = list(NULL, "N"))

      } else if (column.ii == "overall") {

        # Overall
        if (parenth == "none") {
          overall.cell <- paste(sprintf(spf, mean.y))
        } else if (parenth == "sd") {
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf, sd.y), ")", sep = "")
        } else if (parenth == "se") {
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf, se.y), ")", sep = "")
        } else if (parenth == "t.ci") {
          tcrit <- qt(p = 0.975, df = n - 1)
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf, mean.y - tcrit * se.y), sep.char,
                                sprintf(spf, mean.y + tcrit * se.y), ")",
                                sep = "")
        } else if (parenth == "z.ci") {
          zcrit <- qnorm(p = 0.975)
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf, mean.y - zcrit * se.y), sep.char,
                                sprintf(spf, mean.y + zcrit * se.y), ")",
                                sep = "")
        } else if (parenth == "range") {
          spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf.r, diff(range(y))), ")", sep = "")
        } else if (parenth == "minmax") {
          spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
          overall.cell <- paste(sprintf(spf, mean.y), " (",
                                sprintf(spf.r, min(y)), sep.char,
                                sprintf(spf.r, max(y)), ")", sep = "")
        }
        newcol <- matrix(overall.cell, dimnames = list(NULL, "Overall"))

      } else if (column.ii == "xgroups") {

        # M (parenth)
        if (parenth == "none") {
          groups.cells <- paste(sprintf(spf, means))
        } else if (parenth == "sd") {
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf, sds), ")", sep = "")
        } else if (parenth == "se") {
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf, ses), ")", sep = "")
        } else if (parenth == "t.ci") {
          tcrits <- qt(p = 0.975, df = ns - 1)
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf, means - tcrits * ses), sep.char,
                               sprintf(spf, means + tcrits * ses), ")",
                               sep = "")
        } else if (parenth == "z.ci") {
          zcrit <- qnorm(p = 0.975)
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf, means - zcrit * ses), sep.char,
                               sprintf(spf, means + zcrit * ses), ")",
                               sep = "")
        } else if (parenth == "range") {
          spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
          ranges <- tapply(X = y, INDEX = x, FUN = function(x) diff(range(x)))
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf.r, ranges), ")", sep = "")
        } else if (parenth == "minmax") {
          spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
          mins <- tapply(X = y, INDEX = x, FUN = min)
          maxes <- tapply(X = y, INDEX = x, FUN = max)
          group.cells <- paste(sprintf(spf, means), " (",
                               sprintf(spf.r, mins), sep.char,
                               sprintf(spf.r, maxes), ")", sep = "")
        }
        newcol <- matrix(group.cells, nrow = 1, dimnames = list(NULL, xlevels))

      } else if (column.ii == "diff") {

        # Diff.
        newcol <- matrix(sprintf(spf, diffmeans),
                         dimnames = list(NULL, "Diff."))

      } else if (column.ii == "diff.diffci") {

        # Diff. (95% CI)
        newcol <- matrix(paste(sprintf(spf, diffmeans), " (",
                               sprintf(spf, diffmeans.ci[1]), sep.char,
                               sprintf(spf, diffmeans.ci[2]), ")", sep = ""),
                         dimnames = list(NULL, "Diff. (95% CI)"))

      } else if (column.ii == "diffci") {

        # 95% CI for Diff.
        newcol <- matrix(paste(sprintf(spf, diffmeans.ci[1]), sep.char,
                               sprintf(spf, diffmeans.ci[2]), sep = ""),
                         dimnames = list(NULL, "95% CI for Diff.)"))

      } else if (column.ii == "test") {

        # t/z
        newcol <- matrix(sprintf(spf, test.stat),
                         dimnames = list(NULL, test.label))

      } else if (column.ii == "p") {

        # P
        p.formatted <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                               lowerbound = p.lowerbound, leading0 = p.leading0,
                               avoid1 = p.avoid1)
        newcol <- matrix(p.formatted, dimnames = list(NULL, "P"))

      }

      # Add column to table
      tbl <- cbind(tbl, newcol)

    }

    # Add sample sizes to column headings if requested
    if (n.headings) {

      colnames(tbl)[colnames(tbl) == "Overall"] <-
        paste("Overall (n = ", n, ")", sep = "")
      colnames(tbl)[colnames(tbl) %in% xlevels] <-
        paste(xlevels, " (n = ", ns, ")", sep = "")

    }

    # Print html version of table if requested
    if (print.html) {

      tbl.xtable <-
        xtable(tbl, align = paste("ll",
                                  paste(rep("r", ncol(tbl) - 1), collapse = ""),
                                  sep = "", collapse = ""))
      print(tbl.xtable, include.rownames = FALSE, type = "html",
            file = html.filename, sanitize.text.function = function(x) {
              ifelse(substr(x, 1, 1) == " ", paste("&nbsp &nbsp", x), x)
            })

    }

  } else {

    if (parenth == "sd") {

      lowerbars <- means - sds
      upperbars <- means + sds
      ylabel <- paste(yname, " (Mean +/- 1 SD)", sep = "")

    } else if (parenth == "se") {

      lowerbars <- means - ses
      upperbars <- means + ses
      ylabel <- paste(yname, " (Mean +/- 1 SE)", sep = "")

    } else if (parenth == "t.ci") {

      tcrit <- qt(p = 0.975, df = ns - 1)
      lowerbars <- means - tcrit * ses
      upperbars <- means + tcrit * ses
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

    } else if (parenth == "z.ci") {

      zcrit <- qnorm(p = 0.975)
      lowerbars <- means - zcrit * ses
      upperbars <- means + zcrit * ses
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

    } else if (parenth %in% c("range", "minmax")) {

      lowerbars <- tapply(X = y, INDEX = x, FUN = min)
      upperbars <- tapply(X = y, INDEX = x, FUN = max)
      ylabel <- paste(yname, " (Mean, range)", sep = "")

    } else if (parenth == "minmax") {

      mins <- tapply(X = y, INDEX = x, FUN = min)
      maxes <- tapply(X = y, INDEX = x, FUN = max)
      group.cells <- paste(sprintf(spf, means), " (",
                           sprintf(spf, mins), sep.char,
                           sprintf(spf, maxes), ")", sep = "")

    }

    # Save ... into extra.args list
    extra.args <- list(...)

    if (parenth == "none") {

      # Figure out plot inputs if not specified
      if (is.null(extra.args$main)) {
        extra.args$main <- paste("Mean ", yname, " by ", xname, sep = "")
      }
      if (is.null(extra.args$xlim)) {
        extra.args$xlim <- c(0.5, (length(xlevels) + 0.5))
      }
      if (is.null(extra.args$ylim)) {
        bar.range <- max(means) - min(means)
        extra.args$ylim <- c(min(means) - 0.2 * bar.range,
                             max(means) + 0.2 * bar.range)
      }
      if (is.null(extra.args$xlab)) {
        extra.args$xlab <- xname
      }
      if (is.null(extra.args$ylab)) {
        extra.args$ylab <- paste(yname, " (Mean)", sep = "")
      }
      if (is.null(extra.args$cex.lab)) {
        extra.args$cex.lab <- 1.1
      }
      if (is.null(extra.args$pch)) {
        extra.args$pch <- 19
      }

      # Create figure
      tbl <- do.call(plot, c(list(x = 1: length(means), y = means,
                                  xaxt = "n"), extra.args))
      axis(side = 1, at = 1: length(xlevels), labels = xlevels)

    } else {

      # Figure out plot inputs if not specified
      if (is.null(extra.args$main)) {
        extra.args$main <- paste("Mean ", yname, " by ", xname, sep = "")
      }
      if (is.null(extra.args$xlim)) {
        extra.args$xlim <- c(0.5, length(xlevels) + 0.5)
      }
      if (is.null(extra.args$ylim)) {
        bar.range <- max(c(lowerbars, upperbars)) - min(c(lowerbars, upperbars))
        ylim1 <- min(c(lowerbars, upperbars) - 0.1 * bar.range)
        ylim2 <- max(c(lowerbars, upperbars) + 0.1 * bar.range)
        extra.args$ylim <- c(ylim1, ylim2)
      }
      if (is.null(extra.args$xlab)) {
        extra.args$xlab <- xname
      }
      if (is.null(extra.args$cex.lab)) {
        extra.args$cex.lab <- 1.1
      }
      if (is.null(extra.args$ylab)) {
        extra.args$ylab <- ylabel
      }

      # Create figure
      tbl <- do.call(plot, c(list(x = NULL, y = NULL, xaxt = "n"), extra.args))
      for (ii in 1:length(lowerbars)) {

        endpoints <- c(lowerbars[ii], upperbars[ii])
        points(x = ii, y = means[ii], pch = 19)
        lines(x = rep(ii, 2), y = endpoints)
        lines(x = c((ii - 0.03), (ii + 0.03)), y = rep(endpoints[1], 2))
        lines(x = c((ii - 0.03), (ii + 0.03)), y = rep(endpoints[2], 2))

      }
      axis(side = 1, at = 1:length(xlevels), labels = xlevels)

    }

    tbl <- recordPlot()

  }

  # Return table
  return(tbl)

}
