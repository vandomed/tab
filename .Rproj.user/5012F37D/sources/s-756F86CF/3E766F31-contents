#' Generate Summary Tables of Mean Comparisons for Statistical Reports
#'
#' Compares the mean of a continuous variable across levels of a categorical
#' variable and summarizes the results in a clean table (or figure) for a
#' statistical report.
#'
#' If \code{x} has two levels, a t-test is used to test for a difference in
#' means. If \code{x} has more than two levels, a one-way analysis of variance
#' is used to test for a difference in means across the groups.
#'
#' Both \code{x} and \code{y} can have missing values. The function drops
#' observations with missing \code{x} or \code{y}.
#'
#' If you wish to paste your tables into Word, you can use either of these
#' approaches:
#'
#'   1. Use the \code{\link{write.cb}} function in the \pkg{Kmisc} package [2].
#'   If your table is stored in a character matrix named \code{table1}, use
#'   \code{write.cb(table1)} to copy the table to your clipboard. Paste the
#'   result into Word, then highlight the text and go to
#'   \code{Insert - Table - Convert Text to Table... OK}.
#'
#'   2. Set \code{print.html = TRUE}. This will result in a .html file writing
#'   to your current working directory. When you open this file, you will see a
#'   nice looking table that you can copy and paste into Word. You can control
#'   the name of this file with \code{html.filename}.
#'
#' If you wish to use LaTeX, R Markdown, knitr, Sweave, etc., set
#' \code{latex = TRUE} and then use \code{\link[xtable]{xtable}} [1]. You may
#' have to set \code{sanitize.text.function = identity} when calling
#' \code{\link{print.xtable}}.
#'
#' @param x Vector of values for the categorical \code{x} variable.
#'
#' @param y Vector of values for the continuous \code{y} variable.
#'
#' @param latex If \code{TRUE}, object returned is formatted for printing in
#' LaTeX using \code{\link[xtable]{xtable}} [1]; if \code{FALSE}, formatted for
#' copy-and-pasting from RStudio into a word processor.
#'
#' @param variance Controls whether equal variance t-test or unequal variance
#' t-test is used when \code{x} has two levels. Possible values are
#' \code{"equal"} for equal variance, \code{"unequal"} for unequal variance, and
#' \code{"ftest"} for F test to determine which version of the t-test to use.
#' Note that unequal variance t-test is less restrictive than equal variance
#' t-test, and the F test is only valid when \code{y} is normally distributed in
#' both \code{x} groups.
#'
#' @param xname Label for the categorical variable. Only used if
#' \code{fig = TRUE}.
#'
#' @param xlevels Optional character vector to label the levels of \code{x},
#' used in the column headings. If unspecified, the function uses the values
#' that \code{x} takes on.
#'
#' @param yname Optional label for the continuous \code{y} variable. If
#' unspecified, variable name of \code{y} is used.
#'
#' @param quantiles If specified, function compares means of the \code{y}
#' variable across quantiles of the \code{x} variable. For example, if \code{x}
#' contains continuous BMI values and \code{y} contains continuous HDL
#' cholesterol levels, setting \code{quantiles = 3} would result in mean HDL
#' being compared across tertiles of BMI.
#'
#' @param quantile.vals If \code{TRUE}, labels for \code{x} show quantile number
#' and corresponding range of the \code{x} variable, e.g. Q1 [0.00, 0.25). If
#' \code{FALSE}, labels for quantiles just show quantile number, e.g. Q1. Only
#' used if \code{xlevels} is not specified.
#'
#' @param parenth Controls what values (if any) are placed in parentheses after
#' the means in each cell. Possible values are \code{"none"}, \code{"sd"} for
#' standard deviation, \code{"se"} for standard error, \code{"t.ci"} for 95\%
#' confidence interval for population mean based on t distribution, and
#' \code{"z.ci"} for 95\% confidence interval for population mean based on z
#' distribution.
#'
#' @param text.label Optional text to put after the \code{y} variable name,
#' identifying what cell values and parentheses indicate in the table. If
#' unspecified, function uses default labels based on \code{parenth}, e.g. M
#' (SD) if \code{parenth = "sd"}. Set to \code{"none"} for no text labels.
#'
#' @param parenth.sep Optional character specifying the separator between lower
#' and upper bound of confidence interval (when requested). Usually either
#' \code{"-"} or \code{", "} depending on user preference.
#'
#' @param decimals Number of decimal places for numeric values in the table
#' (except p-values). If unspecified, function uses 0 decimal places if the
#' largest mean (in magnitude) is in [1,000, Inf), 1 decimal place if
#' [10, 1,000), 2 decimal places if [0.1, 10), 3 decimal places if [0.01, 0.1),
#' 4 decimal places if [0.001, 0.01), 5 decimal places if [0.0001, 0.001), and 6
#' decimal places if [0, 0.0001).
#'
#' @param p.include If \code{FALSE}, t-test is not performed and p-value is not
#' returned.
#'
#' @param p.decimals Number of decimal places for p-values. If a vector is
#' provided rather than a single value, number of decimal places will depend on
#' what range the p-value lies in. See \code{p.cuts}.
#'
#' @param p.cuts Cut-point(s) to control number of decimal places used for
#' p-values. For example, by default \code{p.cuts = 0.1} and
#' \code{p.decimals = c(2, 3)}. This means that p-values in the range [0.1, 1]
#' will be printed to two decimal places, while p-values in the range [0, 0.1)
#' will be printed to three decimal places.
#'
#' @param p.lowerbound Controls cut-point at which p-values are no longer
#' printed as their value, but rather <lowerbound. For example, by default
#' \code{p.lowerbound = 0.001}. Under this setting, p-values less than 0.001 are
#' printed as \code{<0.001}.
#'
#' @param p.leading0 If \code{TRUE}, p-values are printed with 0 before decimal
#' place; if \code{FALSE}, the leading 0 is omitted.
#'
#' @param p.avoid1 If \code{TRUE}, p-values rounded to 1 are not printed as 1,
#' but as \code{>0.99} (or similarly depending on \code{p.decimals} and
#' \code{p.cuts}).
#'
#' @param overall.column If \code{FALSE}, column showing mean of \code{y} in
#' full sample is suppressed.
#'
#' @param n.column If \code{TRUE}, the table will have a column for sample size.
#'
#' @param n.headings If \code{TRUE}, the table will indicate the sample size
#' overall and in each group in parentheses after the column headings.
#'
#' @param bold.colnames If \code{TRUE}, column headings are printed in bold
#' font. Only applies if \code{latex = TRUE}.
#'
#' @param bold.varnames If \code{TRUE}, variable name in the first column of the
#' table is printed in bold font. Only applies if \code{latex = TRUE}.
#'
#' @param variable.colname Character string with desired heading for first
#' column of table, which shows the \code{y} variable name.
#'
#' @param fig If \code{TRUE}, a figure is returned rather than a table. The
#' figure shows mean (95\% confidence interval) for each level of \code{x}.
#'
#' @param fig.errorbars Controls error bars around mean when \code{fig = TRUE}.
#' Possible values are \code{"sd"} for +/- 1 standard deviation, \code{"se"} for
#' +/- 1 standard error, \code{"t.ci"} for 95\% confidence interval based on t
#' distribution, \code{"z.ci"} for 95\% confidence interval based on z
#' distribution, and \code{"none"} for no error bars.
#'
#' @param fig.title Title of figure. If unspecified, title is set to
#' \code{"Mean yname by xname"}.
#'
#' @param print.html If \code{TRUE}, function prints a .html file to the current
#' working directory.
#'
#' @param html.filename Character string indicating the name of the .html file
#' that gets printed if \code{print.html = TRUE}.
#'
#' @return Character matrix with the requested table comparing mean \code{y}
#' across levels of \code{x}. If \code{latex = TRUE}, matrix is formatted for
#' inserting into a Markdown/Sweave/knitr report using the
#' \code{\link[xtable]{xtable}} package [1].
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
#' @references
#'1. Dahl, D.B. (2016). xtable: Export Tables to LaTeX or HTML. R package
#'version 1.8-2, \url{https://cran.r-project.org/package=xtable}.
#'
#'2. Ushley, K. (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
#'\url{https://CRAN.R-project.org/package=Kmisc}.
#'
#' @export
tabmeans <- function(x, y, latex = FALSE, variance = "unequal", xname = NULL,
                     xlevels = NULL, yname = NULL, quantiles = NULL,
                     quantile.vals = FALSE, parenth = "sd", text.label = NULL,
                     parenth.sep = "-", decimals = NULL, p.include = TRUE,
                     p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
                     p.leading0 = TRUE, p.avoid1 = FALSE, overall.column = TRUE,
                     n.column = FALSE, n.headings = TRUE, bold.colnames = TRUE,
                     bold.varnames = FALSE, variable.colname = "Variable",
                     fig = FALSE, fig.errorbars = "z.ci", fig.title = NULL,
                     print.html = FALSE,
                     html.filename = "table1.html") {

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
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
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
  locs.complete <- which(!is.na(x) & !is.na(y))
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
      if (quantile.vals == TRUE) {
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
  if (p.include == TRUE) {

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

  if (fig == FALSE) {

    # Initialize table
    tbl <- matrix("", nrow = 1, ncol = length(xlevels)+4)

    # Figure out text.label
    if (is.null(text.label)) {
      if (parenth == "sd") {
        text.label <- ", M (SD)"
      } else if (parenth == "se") {
        text.label <- ", M (SE)"
      } else if (parenth %in% c("t.ci", "z.ci")) {
        if (latex == TRUE) {
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
      tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (",
                                       sprintf(spf, sds), ")", sep = "")
    } else if (parenth == "se") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      tbl[1, 3] <- paste(sprintf(spf, mean(y)), " (",
                         sprintf(spf, sd(y)/sqrt(length(y))), ")", sep = "")
      tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (",
                                       sprintf(spf, ses), ")", sep = "")
    } else if (parenth == "t.ci") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
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
      tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (",
                                       sprintf(spf, ci.lower), parenth.sep,
                                       sprintf(spf, ci.upper), ")", sep = "")
    } else if (parenth == "z.ci") {
      ses <- tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      ci.lower <- means - qnorm(0.975) * ses
      ci.upper <- means + qnorm(0.975) * ses
      tbl[1, 3] <-
        paste(sprintf(spf, mean(y)), " (",
              sprintf(spf, mean(y) - qnorm(0.975) * sd(y)/sqrt(length(y))),
              parenth.sep,
              sprintf(spf, mean(y) + qnorm(0.975) * sd(y)/sqrt(length(y))),
              ")", sep = "")
      tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (",
                                       sprintf(spf, ci.lower), parenth.sep,
                                       sprintf(spf, ci.upper), ")", sep = "")
    } else if (parenth == "none") {
      tbl[1, 3] <- sprintf(spf, mean(y))
      tbl[1, 4:(ncol(tbl)-1)] <- sprintf(spf, means)
    }

    # Add p-value from t-test
    if (p.include == TRUE) {
      tbl[1, ncol(tbl)] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                                   lowerbound = p.lowerbound,
                                   leading0 = p.leading0, avoid1 = p.avoid1)
    }

    # Add column names, with sample sizes for each group if requested
    if (n.headings == FALSE) {
      colnames(tbl) <- c(variable.colname, "N", "Overall", xlevels, "P")
    } else {
      colnames(tbl) <- c(variable.colname, "N",
                         paste(c("Overall", xlevels), " (n = ", c(sum(ns), ns),
                               ")", sep = ""), "P")
    }

    # Drop N column if requested
    if (n.column == FALSE) {
      tbl <- tbl[, -which(colnames(tbl) == "N"), drop = FALSE]
    }

    # Drop overall column if requested
    if (overall.column == FALSE) {
      tbl <- tbl[, -grep("^Overall", colnames(tbl)), drop = FALSE]
    }

    # Drop p column if requested
    if (p.include == FALSE) {
      tbl <- tbl[, -which(colnames(tbl) == "P"), drop = FALSE]
    }

    # If latex is TRUE, do some re-formatting
    if (latex == TRUE) {
      if (p.include == TRUE) {
        plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
        if (length(plocs) > 0) {
          tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2),
                                   sep = "")
        }
      }
      if (bold.colnames == TRUE) {
        colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
      }
      if (bold.varnames == TRUE) {
        tbl[1, 1] <- paste("$\\textbf{", tbl[1, 1], "}$")
      }
    }

  } else {

    if (fig.errorbars == "sd") {

      lowerbars <- means - tapply(X = y, INDEX = x, FUN = sd)
      upperbars <- means + tapply(X = y, INDEX = x, FUN = sd)
      ylabel <- paste(yname, " (Mean +/- 1 SD)", sep = "")

    } else if (fig.errorbars == "se") {

      lowerbars <- means - tapply(X = y, INDEX = x, FUN = function(x)
        sd(x)/sqrt(length(x)))
      upperbars <- means + tapply(X = y, INDEX = x, FUN = function(x)
        sd(x)/sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 1 SE)", sep = "")

    } else if (fig.errorbars == "t.ci") {

      lowerbars <- means - qt(p = 0.975, df = ns - 1) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      upperbars <- means + qt(p = 0.975, df = ns - 1) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

    } else if (fig.errorbars == "z.ci") {

      lowerbars <- means - qnorm(p = 0.975) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      upperbars <- means + qnorm(p = 0.975) *
        tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      ylabel <- paste(yname, " (Mean +/- 95% CI)", sep = "")

    }

    if (fig.errorbars == "none") {

      bar.range <- max(means) - min(means)
      ylim1 <- min(means) - 0.2*bar.range
      ylim2 <- max(means) + 0.2*bar.range

      tbl <- plot(x = 1:length(means), y = means,
                  main = paste("Mean ", yname, " by ", xname, sep = ""),
                  xlim = c(0.5, (length(xlevels)+0.5)), ylim = c(ylim1, ylim2),
                  ylab = paste(yname, " (Mean)", sep = ""), xlab = xname,
                  xaxt = "n", cex.lab = 1.1, pch = 19)
      axis(side = 1, at = 1:length(xlevels), labels = xlevels)


    } else {

      bar.range <- max(c(lowerbars, upperbars)) - min(c(lowerbars, upperbars))
      ylim1 <- min(c(lowerbars, upperbars) - 0.1*bar.range)
      ylim2 <- max(c(lowerbars, upperbars) + 0.1*bar.range)

      if (is.null(fig.title)) {
        fig.title <- paste("Mean ", yname, " by ", xname, sep = "")
      }

      tbl <- plot(x = NULL, y = NULL, main = fig.title,
                  xlim = c(0.5, (length(xlevels)+0.5)), ylim = c(ylim1, ylim2),
                  ylab = ylabel, xlab = xname, xaxt = "n", cex.lab = 1.1)

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

  # Return table
  return(tbl)

}
