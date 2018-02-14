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
#' \code{format.xtable = TRUE}. You may have to set
#' \code{sanitize.text.function = identity} when calling
#' \code{\link[xtable]{print.xtable}}.
#'
#'
#' @param x Vector of values for the categorical \code{x} variable.
#'
#' @param y Vector of values for the continuous \code{y} variable.
#'
#' @param format.xtable Logical value for whether to format table for
#' converting to \code{\link[xtable]{xtable}} object and printing in LaTeX.
#'
#' @param variance Character string specifying which version of the two-sample
#' t-test to use, supposing \code{x} has two levels. Choices are \code{"equal"}
#' for equal variance t-test, \code{"unequal"} for unequal variance t-test, and
#' \code{"ftest"} for F test to determine which to use.
#'
#' @param xname Character string with a label for the \code{x} variable.
#'
#' @param xlevels Character vector with labels for each level of \code{x}, for
#' column headings.
#'
#' @param yname Character string with a label for the \code{y} variable.
#'
#' @param quantiles Numeric value. If specified, table compares mean \code{y}
#' across quantiles of \code{x}. For example, if \code{x} contains BMI values
#' and \code{y} HDL levels, setting \code{quantiles = 3} compares mean HDL
#' across BMI tertiles.
#'
#' @param quantile.vals Logical value for whether labels for \code{x} should
#' show quantile number and corresponding range, e.g. Q1 [0.00, 0.25), rather
#' than just the quantile number.
#'
#' @param parenth Character string specifying what statistic should appear in
#' parentheses after the means in each cell. Choices are \code{"none"},
#' \code{"sd"} for standard deviation, \code{"se"} for standard error,
#' \code{"t.ci"} for 95\% confidence interval based on t distribution, and
#' \code{"z.ci"} for 95\% confidence interval based on z distribution.
#'
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent.
#'
#' @param parenth.sep Character string with separator to place between first and
#' second numbers in parentheses, if applicable. Typically \code{"-"} or
#' \code{", "}.
#'
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#'
#' @param p.include Logical value for whether to include a p-value column.
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
#' @param overall.column Logical value for whether to include a column with the
#' overall mean of \code{y}.
#'
#' @param n.column Logical value for whether to include a column with the
#' overall sample size.
#'
#' @param n.headings Logical value for whether to include sample sizes in
#' parentheses in column headings.
#'
#' @param bold.colnames Logical value for whether to use bold font for column
#' headings. Only used if \code{xtable = TRUE}.
#'
#' @param bold.varnames Logic value for whether to use bold font for the
#' \code{y} variable name in the first column. Only used if \code{xtable = TRUE}.
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
#' @return Character matrix with table comparing mean \code{y} across levels of
#' \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset d and drop rows with missing values
#' data(d)
#' d <- d[complete.cases(d), ]
#'
#' # Compare mean BMI in control group vs. treatment group - table and figure
#' meanstable1 <- tabmeans(x = d$Group, y = d$BMI)
#' meansfig1 <- tabmeans(x = d$Group, y = d$BMI, fig = TRUE)
#'
#' # Compare mean BMI by race - table and figure
#' meanstable2 <- tabmeans(x = d$Race, y = d$BMI)
#' meansfig2 <- tabmeans(x = d$Race, y = d$BMI, fig = TRUE)
#'
#' # Compare mean baseline systolic BP across tertiles of BMI - table and figure
#' meanstable3 <- tabmeans(x = d$BMI, y = d$bp.1, yname = "Systolic BP",
#'                         quantiles = 3)
#' meansfig3 <- tabmeans(x = d$BMI, y = d$bp.1, quantiles = 3, fig = TRUE,
#'                       yname = "Systolic BP", xname = "BMI Tertile")
#'
#' # Create single table comparing mean BMI and mean age in control vs.
#' # treatment group
#' meanstable4 <- rbind(tabmeans(x = d$Group, y = d$BMI),
#'                      tabmeans(x = d$Group, y = d$Age))
#'
#' # An easier way to make the above table is to call the tabmulti function
#' meanstable5 <- tabmulti(dataset = d, xvarname = "Group",
#'                         yvarnames = c("BMI", "Age"))
#'
#' # meanstable4 and meanstable5 are equivalent
#' all(meanstable4 == meanstable5)
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
tabmeans <- function(x, y, format.xtable = FALSE, variance = "unequal",
                     xname = NULL, xlevels = NULL, yname = NULL, quantiles = NULL,
                     quantile.vals = FALSE, parenth = "sd", text.label = NULL,
                     parenth.sep = "-", decimals = NULL, p.include = TRUE,
                     p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
                     p.leading0 = TRUE, p.avoid1 = FALSE, overall.column = TRUE,
                     n.column = FALSE, n.headings = TRUE, bold.colnames = TRUE,
                     bold.varnames = FALSE, variable.colname = "Variable",
                     fig = FALSE, print.html = FALSE,
                     html.filename = "table1.html", ...) {

  # If yname or xname unspecified, use variable names
  if (is.null(yname)) {
    yname <- deparse(substitute(y))
    if (grepl("\\$", yname)) {
      yname <- strsplit(yname, "\\$")[[1]][2]
    }
  }
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
    if (grepl("\\$", xname)) {
      xname <- strsplit(xname, "\\$")[[1]][2]
    }
  }

  # If any inputs are not correct class, return error
  if (!is.logical(format.xtable)) {
    stop("For format.xtable input, please enter TRUE or FALSE")
  }
  if (! variance %in% c("equal", "unequal", "ftest")) {
    stop("For variance input, please enter 'equal', 'unequal', or 'ftest'")
  }
  if (!is.character(xname)) {
    stop("For xname input, please enter character string")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (!is.character(yname)) {
    stop("For yname input, please enter character string")
  }
  if (!is.null(quantiles) &&
      ! (is.numeric(quantiles) & length(quantiles) == 1 && quantiles > 1 &
         quantiles == round(quantiles))) {
    stop("For quantiles input, please enter a whole number greater than 1")
  }
  if (!is.logical(quantile.vals)) {
    stop("For quantile.vals input, please enter TRUE or FALSE")
  }
  if (! parenth %in% c("none", "sd", "se", "t.ci", "z.ci", "none")) {
    stop("For parenth input, please enter 'none', 'sd', 'se', 't.ci', or 'z.ci'")
  }
  if (!is.null(text.label) && !is.character(text.label)) {
    stop("For text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (!is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string (only used if parenth is set to 't.ci' or 'z.ci'; usually '-' or ', ')")
  }
  if (!is.null(decimals) && !is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
  }
  if (!is.logical(p.include)) {
    stop("For p.include input, please enter TRUE or FALSE")
  }
  if (!is.numeric(p.decimals)) {
    stop("For p.decimals input, please enter numeric value or vector")
  }
  if (!is.numeric(p.cuts)) {
    stop("For p.cuts input, please enter numeric value or vector")
  }
  if (!is.numeric(p.lowerbound)) {
    stop("For p.lowerbound input, please enter numeric value")
  }
  if (!is.logical(p.leading0)) {
    stop("For p.leading0 input, please enter TRUE or FALSE")
  }
  if (!is.logical(p.avoid1)) {
    stop("For p.avoid1 input, please enter TRUE or FALSE")
  }
  if (!is.logical(overall.column)) {
    stop("For overall.column input, please enter TRUE or FALSE")
  }
  if (!is.logical(n.column)) {
    stop("For n.column input, please enter TRUE or FALSE")
  }
  if (!is.logical(n.headings)) {
    stop("For n.headings input, please enter TRUE or FALSE")
  }
  if (!is.logical(bold.colnames)) {
    stop("For bold.colnames input, please enter TRUE or FALSE")
  }
  if (!is.logical(bold.varnames)) {
    stop("For bold.varnames input, please enter TRUE or FALSE")
  }
  if (!is.character(variable.colname)) {
    stop("For variable.colname input, please enter a character string")
  }
  if (!is.logical(fig)) {
    stop("For fig input, please enter TRUE or FALSE")
  }

  # Drop missing values
  locs.complete <- which(! is.na(x) & ! is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]

  # Create quantiles if necessary
  if (!is.null(quantiles)) {
    x <- cut(x = x, breaks = quantile(x, probs = seq(0, 1, 1 / quantiles)),
             include.lowest = TRUE, right = TRUE, dig.lab = 3)
  }

  # Get means and sample sizes
  means <- tapply(X = y, INDEX = x, FUN = mean)
  ns <- tapply(X = y, INDEX = x, FUN = length)

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

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Get unique values of x
  xvals <- sort(unique(x))

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    if (!is.null(quantiles)) {
      if (quantile.vals) {
        xlevels <- paste("Q", 1:length(xvals), " ", as.character(xvals),
                         sep = "")
      } else {
        xlevels <- paste("Q", 1:length(xvals), sep = "")
      }
    } else {
      xlevels <- as.character(xvals)
    }
  }

  # Calculate p-value based on ANOVA or t-test depending on number of levels of
  # x
  if (p.include) {

    if (length(xlevels) == 2) {

      if (variance == "equal") {
        p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                    var.equal = TRUE)$p.value
        message(paste("Equal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
      } else if (variance == "unequal") {
        p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                    var.equal = FALSE)$p.value
        message(paste("Unequal variance t-test was used to compare mean ",
                      yname, " in the two groups.", sep = ""))
      } else if (variance == "ftest") {
        f <- var.test(x = y[x == xvals[1]], y = y[x == xvals[2]])
        if (f$p.value < 0.05) {
          p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = FALSE)$p.value
          message(paste("Unequal variance t-test was used to compare mean ",
                        yname, " in the two groups.", sep = ""))
        } else {
          p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]],
                      var.equal = TRUE)$p.value
          message(paste("Equal variance t-test was used to compare mean ",
                        yname, " in the two groups.", sep = ""))
        }
      }

    } else {

      # ANOVA
      p <- anova(lm(y ~ as.factor(x)))$"Pr(>F)"[1]
      message(paste("ANOVA was used to compare means for ", yname, sep = ""))

    }

  } else {
    p <- NA
  }

  if (! fig) {

    # Initialize table
    tbl <- matrix("", nrow = 1, ncol = length(xlevels) + 4)

    # Figure out text.label
    if (is.null(text.label)) {
      if (parenth == "sd") {
        text.label <- ", M (SD)"
      } else if (parenth == "se") {
        text.label <- ", M (SE)"
      } else if (parenth %in% c("t.ci", "z.ci")) {
        if (format.xtable) {
          text.label <- ", M (95\\% CI)"
        } else {
          text.label <- ", M (95% CI)"
        }
      } else if (parenth == "none") {
        text.label <- ", M"
      }
    } else if (text.label == "none") {
      text.label <- NULL
    }

    # Add variable name and n column to table
    tbl[1, 1] <- paste(yname, text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(y))

    # Add M (parenth) overall and in each x group
    if (parenth == "sd") {
      sds <- tapply(X = y, INDEX = x, FUN = sd)
      tbl[1, 3] <- paste(sprintf(spf, mean(y)), " (", sprintf(spf, sd(y)), ")",
                         sep = "")
      tbl[1, 4:(ncol(tbl) - 1)] <- paste(sprintf(spf, means), " (",
                                         sprintf(spf, sds), ")", sep = "")
    } else if (parenth == "se") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      tbl[1, 3] <- paste(sprintf(spf, mean(y)), " (",
                         sprintf(spf, sd(y) / sqrt(length(y))), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <- paste(sprintf(spf, means), " (",
                                          sprintf(spf, ses), ")", sep = "")
    } else if (parenth == "t.ci") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      ci.lower <- means - qt(p = 0.975, df = ns - 1) * ses
      ci.upper <- means + qt(p = 0.975, df = ns - 1) * ses
      tbl[1, 3] <-
        paste(sprintf(spf, mean(y)), " (",
              sprintf(spf, mean(y) -
                        qt(p = 0.975, df = length(y) - 1) * sd(y) /
                        sqrt(length(y))),
              parenth.sep,
              sprintf(spf, mean(y) +
                        qt(p = 0.975, df = length(y) - 1) * sd(y) /
                        sqrt(length(y))), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <- paste(sprintf(spf, means), " (",
                                          sprintf(spf, ci.lower), parenth.sep,
                                          sprintf(spf, ci.upper), ")", sep = "")
    } else if (parenth == "z.ci") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      ci.lower <- means - qnorm(0.975) * ses
      ci.upper <- means + qnorm(0.975) * ses
      tbl[1, 3] <-
        paste(sprintf(spf, mean(y)), " (",
              sprintf(spf, mean(y) - qnorm(0.975) * sd(y) / sqrt(length(y))),
              parenth.sep,
              sprintf(spf, mean(y) + qnorm(0.975) * sd(y) / sqrt(length(y))),
              ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <- paste(sprintf(spf, means), " (",
                                          sprintf(spf, ci.lower), parenth.sep,
                                          sprintf(spf, ci.upper), ")", sep = "")
    } else if (parenth == "none") {
      tbl[1, 3] <- sprintf(spf, mean(y))
      tbl[1, 4: (ncol(tbl) - 1)] <- sprintf(spf, means)
    }

    # Add p-value from t-test
    if (p.include) {
      tbl[1, ncol(tbl)] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                                   lowerbound = p.lowerbound,
                                   leading0 = p.leading0, avoid1 = p.avoid1)
    }

    # Add column names, with sample sizes for each group if requested
    if (! n.headings) {
      colnames(tbl) <- c(variable.colname, "N", "Overall", xlevels, "P")
    } else {
      colnames(tbl) <- c(variable.colname, "N",
                         paste(c("Overall", xlevels), " (n = ", c(sum(ns), ns),
                               ")", sep = ""), "P")
    }

    # Drop N column if requested
    if (! n.column) {
      tbl <- tbl[, -which(colnames(tbl) == "N"), drop = FALSE]
    }

    # Drop overall column if requested
    if (! overall.column) {
      tbl <- tbl[, -grep("^Overall", colnames(tbl)), drop = FALSE]
    }

    # Drop p column if requested
    if (! p.include) {
      tbl <- tbl[, -which(colnames(tbl) == "P"), drop = FALSE]
    }

    # If format.xtable is TRUE, do some re-formatting
    if (format.xtable) {
      if (p.include) {
        plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
        if (length(plocs) > 0) {
          tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2),
                                   sep = "")
        }
      }
      if (bold.colnames) {
        colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
      }
      if (bold.varnames) {
        tbl[1, 1] <- paste("$\\textbf{", tbl[1, 1], "}$")
      }
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

      lowerbars <- means - tapply(X = y, INDEX = x, FUN = sd)
      upperbars <- means + tapply(X = y, INDEX = x, FUN = sd)
      ylabel <- paste(yname, " (Mean +/- 1 SD)", sep = "")

    } else if (parenth == "se") {

      lowerbars <- means - tapply(X = y, INDEX = x, FUN = function(x)
        sd(x) / sqrt(length(x)))
      upperbars <- means + tapply(X = y, INDEX = x, FUN = function(x)
        sd(x) / sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 1 SE)", sep = "")

    } else if (parenth == "t.ci") {

      lowerbars <- means - qt(p = 0.975, df = ns - 1) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      upperbars <- means + qt(p = 0.975, df = ns - 1) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

    } else if (parenth == "z.ci") {

      lowerbars <- means - qnorm(p = 0.975) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      upperbars <- means + qnorm(p = 0.975) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x) / sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

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
