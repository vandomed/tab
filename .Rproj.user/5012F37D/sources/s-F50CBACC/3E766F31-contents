#' Generate Summary Table Comparing Group Medians
#'
#' Creates table (or figure) comparing the median of \code{y} across levels of
#' \code{x}.
#'
#' If \code{x} has 2 levels, a Mann-Whitney U (also known as Wilcoxon
#' rank-sum) test is used to test whether the distribution of \code{y} differs
#' in the two groups; if \code{x} has more than 2 levels, a Kruskal-Wallis test
#' is used to test whether the distribution of \code{y} differs across at
#' least two of the groups. Observations with missing values for \code{x} and/or
#' \code{y} are dropped.
#'
#'
#' @inherit tabmeans references
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#'
#'
#' @param quantiles Numeric value. If specified, table compares median \code{y}
#' across quantiles of \code{x}. For example, if \code{x} contains BMI values
#' and \code{y} HDL levels, setting \code{quantiles = 3} compares median HDL
#' across BMI tertiles.
#'
#' @param parenth Character string specifying what values are shown in
#' parentheses after the medians in each cell. Choices are \code{"none"},
#' \code{"iqr"}, \code{"range"}, \code{"minmax"}, \code{"q1q3"} for first and
#' third quartiles, and \code{"ci"} for 95\% confidence interval for the medians
#' (based on binomial probabilities if one or more groups have n less than 10,
#' otherwise based on normal approximation to binomial).
#'
#' @param overall.column Logical value for whether to include a column with the
#' overall median of \code{y}.
#'
#'
#' @return Character matrix with table comparing median \code{y} across levels
#' of \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset d and drop rows with missing values
#' data(d)
#' d <- d[complete.cases(d), ]
#'
#' # Create labels for group and race
#' groups <- c("Control", "Treatment")
#' races <- c("White", "Black", "Mexican American", "Other")
#'
#' # Compare median BMI in control group vs. treatment group
#' medtable1 <- tabmedians(x = d$Group, y = d$BMI)
#'
#' # Repeat, but show first and third quartile rather than IQR in parentheses
#' medtable2 <- tabmedians(x = d$Group, y = d$BMI, parenth = "q1q3")
#'
#' # Compare median BMI by race, suppressing overall column and (n = ) part of
#' # headings
#' medtable3 <- tabmedians(x = d$Race, y = d$BMI, overall.column = FALSE,
#'                         n.headings = FALSE)
#'
#' # Compare median BMI by quartile of age
#' medtable4 <- tabmedians(x = d$Age, y = d$BMI, quantiles = 4)
#'
#' # Create single table comparing median BMI and median age in control vs.
#' # treatment group
#' medtable5 <- rbind(tabmedians(x = d$Group, y = d$BMI),
#'                    tabmedians(x = d$Group, y = d$Age))
#'
#' # A (usually) faster way to make the above table is to call the tabmulti
#' # function
#' medtable6 <- tabmulti(dataset = d, xvarname = "Group",
#'                       yvarnames = c("BMI", "Age"), ymeasures = "median")
#'
#' # medtable5 and medtable6 are equivalent
#' all(medtable5 == medtable6)
#'
#'
#' @export
tabmedians <- function(x, y, latex = FALSE, xname = NULL, xlevels = NULL,
                       yname = NULL, quantiles = NULL, quantile.vals = FALSE,
                       parenth = "iqr", text.label = NULL, parenth.sep = "-",
                       decimals = NULL, p.include = TRUE, p.decimals = c(2, 3),
                       p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
                       p.avoid1 = FALSE, overall.column = TRUE,
                       n.column = FALSE, n.headings = TRUE,
                       bold.colnames = TRUE, bold.varnames = FALSE,
                       variable.colname = "Variable", fig = FALSE,
                       print.html = FALSE, html.filename = "table1.html",
                       ...) {

  # If yname or xname unspecified, use variable name
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
  if (! parenth %in% c("none", "iqr", "range", "minmax", "q1q3", "ci")) {
    stop("For parenth input, please enter 'none', 'iqr', 'range', 'minmax', 'q1q3', or 'ci'")
  }
  if (!is.null(text.label) && !is.character(text.label)) {
    stop("For text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (!is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string (only used if parenth is set to 'minmax' or 'q1q3'; usually '-' or ', ')")
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

  # Get medians and sample sizes
  medians <- tapply(X = y, INDEX = x, FUN = median)
  ns <- tapply(X = y, INDEX = x, FUN = length)

  # If decimals is unspecified, set to appropriate value
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

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Get unique values of x
  xvals <- sort(unique(x))

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    if (!is.null(quantiles)) {
      if (quantile.vals) {
        xlevels <- paste("Q", 1: length(xvals), " ", as.character(xvals),
                         sep = "")
      } else {
        xlevels <- paste("Q", 1: length(xvals), sep = "")
      }
    } else {
      xlevels <- as.character(xvals)
    }
  }

  if (! fig) {

    # Initialize table
    tbl <- matrix("", nrow = 1, ncol = length(xlevels) + 4)

    # Figure out text.label
    if (is.null(text.label)) {
      if (parenth == "iqr") {
        text.label <- ", Median (IQR)"
      } else if (parenth == "Range") {
        text.label <- ", Median (range)"
      } else if (parenth == "minmax") {
        text.label <- ", Median (Min-Max)"
      } else if (parenth == "q1q3") {
        text.label <- ", Median (Q1-Q3)"
      } else if (parenth == "ci") {
        if (latex == TRUE) {
          text.label <- ", Median (95\\% CI)"
        } else {
          text.label <- ", Median (95% CI)"
        }
      } else if (parenth == "none") {
        text.label <- ", Median"
      }
    } else if (text.label == "none") {
      text.label <- NULL
    }

    # Add variable name and n column to table
    tbl[1, 1] <- paste(yname, text.label, sep = "")
    tbl[1, 2] <- sprintf("%.0f", length(y))

    # Add Median (parenth) overall and in each x group
    if (parenth == "minmax") {
      parent1 <- tapply(X = y, INDEX = x, FUN = min)
      parent2 <- tapply(X = y, INDEX = x, FUN = max)
      parent <- paste(sprintf(spf, parent1), parenth.sep,
                      sprintf(spf, parent2), sep = "")
      tbl[1, 3] <- paste(sprintf(spf, median(y)), " (", sprintf(spf, min(y)),
                         parenth.sep, sprintf(spf, max(y)), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <- paste(sprintf(spf, medians), " (", parent, ")",
                                          sep = "")
    } else if (parenth == "range") {
      parent1 <- tapply(X = y, INDEX = x, FUN = min)
      parent2 <- tapply(X = y, INDEX = x, FUN = max)
      parent <- paste(sprintf(spf, parent2 - parent1))
      tbl[1, 3] <- paste(sprintf(spf, median(y)), " (",
                         sprintf(spf, max(y) - min(y)), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <-
        paste(sprintf(spf, medians), " (", parent, ")", sep = "")
    } else if (parenth == "q1q3") {
      parent1 <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.25))
      parent2 <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.75))
      parent <- paste(sprintf(spf, parent1), parenth.sep, sprintf(spf, parent2),
                      sep = "")
      tbl[1, 3] <- paste(sprintf(spf, median(y)), " (",
                         sprintf(spf, quantile(y, probs = 0.25)), parenth.sep,
                         sprintf(spf, quantile(y, 0.75)), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <-
        paste(sprintf(spf, medians), " (", parent, ")", sep = "")
    } else if (parenth == "iqr") {
      parent1 <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.25))
      parent2 <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.75))
      parent <- paste(sprintf(spf, parent2 - parent1))
      tbl[1, 3] <- paste(sprintf(spf, median(y)), " (",
                         sprintf(spf, quantile(y, probs = 0.75) -
                                   quantile(y, probs = 0.25)), ")", sep = "")
      tbl[1, 4: (ncol(tbl) - 1)] <-
        paste(sprintf(spf, medians), " (", parent, ")", sep = "")
    } else if (parenth == "ci") {
      if (all(ns >= 10)) {
        parent1 <- tapply(X = y, INDEX = x, FUN = function(x)
          sort(x)[length(x) / 2 - qnorm(p = 0.975) * sqrt(length(x)) / 2])
        parent2 <- tapply(X = y, INDEX = x, FUN = function(x)
          sort(x)[1 + length(x) / 2 + qnorm(p = 0.975) * sqrt(length(x)) / 2])
        parent <- paste(sprintf(spf, parent1), parenth.sep,
                        sprintf(spf, parent2), sep = "")
        tbl[1, 3] <-
          paste(sprintf(spf, median(y)), " (",
                sprintf(spf, sort(y)[length(y) / 2 - qnorm(p = 0.975) *
                                       sqrt(length(y)) / 2]),
                parenth.sep,
                sprintf(spf, sort(y)[1 + length(y) / 2 + qnorm(p = 0.975) *
                                       sqrt(length(y)) / 2]), ")", sep = "")
        tbl[1, 4: (ncol(tbl) - 1)] <-
          paste(sprintf(spf, medians), " (", parent, ")", sep = "")
      } else {
        func <- function(x, lower.or.upper) {
          sorted.vals <- sort(x)
          n <- length(sorted.vals)
          probs <- dbinom(x = 0:n, prob = 0.5, size = n)
          sorted.probs <- sort(probs, decreasing = TRUE)
          min.prob <- sorted.probs[which(cumsum(sorted.probs) >= 0.95)[1]]
          ranks <- which(probs >= min.prob)
          if (lower.or.upper == "lower") {
            ret <- sorted.vals[ranks[1]]
          } else if (lower.or.upper == "upper") {
            ret <- sorted.vals[rev(ranks)[1]]
          }
          return(ret)
        }
        parent1 <- tapply(X = y, INDEX = x, FUN = function(x)
          func(x = x, lower.or.upper = "lower"))
        parent2 <- tapply(X = y, INDEX = x, FUN = function(x)
          func(x = x, lower.or.upper = "upper"))
        parent <- paste(sprintf(spf, parent1), parenth.sep,
                        sprintf(spf, parent2), sep = "")
        tbl[1, 3] <- paste(sprintf(spf, median(y)), " (",
                           sprintf(spf, func(y, "lower")), parenth.sep,
                           sprintf(spf, func(y, "upper")), ")", sep = "")
        tbl[1, 4: (ncol(tbl) - 1)] <-
          paste(sprintf(spf, medians), " (", parent, ")", sep = "")
      }
    } else if (parenth == "none") {
      tbl[1, 3] <- sprintf(spf, median(y))
      tbl[1, 4: (ncol(tbl) - 1)] <- sprintf(spf, medians)
    }

    # Add p-value from statistical test depending on number of levels of x
    if (p.include) {

      if (length(xlevels) == 2) {

        # Mann-Whitney U test a.k.a. Wilcoxon rank-sum test
        p <- wilcox.test(y ~ x)$p.value

      } else {

        # Kruskal-Wallis rank-sum test
        p <- kruskal.test(y ~ as.factor(x))$p.value

      }

    } else {
      p <- NA
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
      colnames(tbl) <-
        c(variable.colname, "N",
          paste(c("Overall", xlevels), " (n = ", c(sum(ns), ns), ")", sep = ""),
          "P")
    }

    # Drop N column if requested
    if (! n.column) {
      tbl <- tbl[, -which(colnames(tbl) == "N"), drop = FALSE]
    }

    # Drop overall column if requested
    if (overall.column == FALSE) {
      tbl <- tbl[, -grep("^Overall", colnames(tbl)), drop = FALSE]
    }

    # Drop p column if requested
    if (! p.include) {
      tbl <- tbl[, -which(colnames(tbl) == "P"), drop = FALSE]
    }

    # If latex is TRUE, do some re-formatting
    if (latex) {
      if (p.include) {
        plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
        if (length(plocs) > 0) {
          tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
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

    if (parenth %in% c("iqr", "q1q3")) {

      lowerbars <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.25))
      upperbars <- tapply(X = y, INDEX = x, FUN = function(x)
        quantile(x, probs = 0.75))
      ylabel <- paste(yname, " (Median, IQR)", sep = "")

    } else if (parenth %in% c("range", "minmax")) {

      lowerbars <- tapply(X = y, INDEX = x, FUN = min)
      upperbars <- tapply(X = y, INDEX = x, FUN = max)
      ylabel <- paste(yname, " (Median, range)", sep = "")

    } else if (parenth == "q1q3") {

      lowerbars <- tapply(X = y, INDEX = x, FUN = min)
      upperbars <- tapply(X = y, INDEX = x, FUN = max)
      ylabel <- paste(yname, " (Median, range)", sep = "")

    } else if (parenth == "ci") {

      if (all(ns >= 10)) {

        lowerbars <- tapply(X = y, INDEX = x, FUN = function(x)
          sort(x)[length(x) / 2 - qnorm(p = 0.975) * sqrt(length(x)) / 2])
        upperbars <- tapply(X = y, INDEX = x, FUN = function(x)
          sort(x)[length(x) / 2 + qnorm(p = 0.975) * sqrt(length(x)) / 2])

      } else {

        func <- function(x, lower.or.upper) {
          sorted.vals <- sort(x)
          n <- length(sorted.vals)
          probs <- dbinom(x = 0: n, prob = 0.5, size = n)
          sorted.probs <- sort(probs, decreasing = TRUE)
          min.prob <- sorted.probs[which(cumsum(sorted.probs) >= 0.95)[1]]
          ranks <- which(probs >= min.prob)
          if (lower.or.upper == "lower") {
            ret <- sorted.vals[ranks[1]]
          } else if (lower.or.upper == "upper") {
            ret <- sorted.vals[rev(ranks)[1]]
          }
          return(ret)
        }
        lowerbars <- tapply(X = y, INDEX = x, FUN = function(x)
          func(x = x, lower.or.upper = "lower"))
        upperbars <- tapply(X = y, INDEX = x, FUN = function(x)
          func(x = x, lower.or.upper = "upper"))

      }

      ylabel <- paste(yname, " (Median +/- 95% CI")

    }

    # Save ... into extra.args list
    extra.args <- list(...)

    if (parenth == "none") {

      # Figure out plot inputs if not specified
      if (is.null(extra.args$main)) {
        extra.args$main <- paste("Median ", yname, " by ", xname, sep = "")
      }
      if (is.null(extra.args$xlim)) {
        extra.args$xlim <- c(0.5, (length(xlevels) + 0.5))
      }
      if (is.null(extra.args$ylim)) {
        bar.range <- max(medians) - min(medians)
        extra.args$ylim <- c(min(medians) - 0.2 * bar.range,
                             max(medians) + 0.2 * bar.range)
      }
      if (is.null(extra.args$xlab)) {
        extra.args$xlab <- xname
      }
      if (is.null(extra.args$ylab)) {
        extra.args$ylab <- paste(yname, " (Median)", sep = "")
      }
      if (is.null(extra.args$cex.lab)) {
        extra.args$cex.lab <- 1.1
      }
      if (is.null(extra.args$pch)) {
        extra.args$pch <- 19
      }

      # Create figure
      tbl <- do.call(plot, c(list(x = 1: length(medians), y = medians,
                                  xaxt = "n"), extra.args))
      axis(side = 1, at = 1: length(xlevels), labels = xlevels)

    } else {

      # Figure out plot inputs if not specified
      if (is.null(extra.args$main)) {
        extra.args$main <- paste("Median ", yname, " by ", xname, sep = "")
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
      tbl <- do.call(plot, c(list(x = NULL, y = NULL), extra.args))
      for (ii in 1:length(lowerbars)) {

        endpoints <- c(lowerbars[ii], upperbars[ii])
        points(x = ii, y = medians[ii], pch = 19)
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
