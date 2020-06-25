#' Create Frequency Table
#'
#' Creates an I-by-J frequency table comparing the distribution of \code{y}
#' across levels of \code{x}.
#'
#'
#' @param formula Formula, e.g. \code{Sex ~ Group}.
#' @param data Data frame containing variables named in \code{formula}.
#' @param x Vector indicating group membership for columns of I-by-J table.
#' @param y Vector indicating group membership for rows of I-by-J table.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall distribution of \code{y}, \code{"xgroups"} for distributions of
#' \code{y} for each \code{x} group, \code{"test"} for test statistic, and
#' \code{"p"} for p-value.
#' @param cell Character string specifying what statistic to display in cells.
#' Choices are \code{"counts"}, \code{"tot.percent"}, \code{"col.percent"},
#' and \code{"row.percent"}.
#' @param parenth Character string specifying what statistic to display in
#' parentheses. Choices are \code{"none"}, \code{"se"}, \code{"ci"},
#' \code{"counts"}, \code{"tot.percent"}, \code{"col.percent"}, and
#' \code{"row.percent"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param test Character string specifying which test for association between
#' \code{x} and \code{y} should be used. Choices are \code{"chi.fisher"} for
#' Pearson's chi-squared test if its assumptions are met, otherwise Fisher's
#' exact test; \code{"chi"}; \code{"fisher"}; \code{"z"} for z test without
#' continuity correction; and \code{"z.continuity"} for z test with continuity
#' correction. The last two only work if both \code{x} and \code{y} are binary.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yname Character string with a label for the \code{y} variable.
#' @param ylevels Character vector with labels for the levels of \code{y}. Note
#' that levels of \code{y} are listed in the order that they appear when you run
#' \code{table(y, x)}.
#' @param compress.binary Logical value for whether to compress binary \code{y}
#' variable to a single row, excluding the first level rather than showing both.
#' @param yname.row Logical value for whether to include a row displaying the
#' name of the \code{y} variable and indent the factor levels.
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
#' # Compare sex distribution by group
#' (freqtable1 <- tabfreq(Sex ~ Group, data = tabdata))
#'
#' # Same as previous, but showing male row only and % (SE) rather than n (%)
#' (freqtable2 <- tabfreq(Sex ~ Group, data = tabdata,
#'                        cell = "col.percent", parenth = "se",
#'                        compress.binary = TRUE))
#'
#'
#' @export
tabfreq <- function(formula = NULL,
                    data = NULL,
                    x = NULL,
                    y = NULL,
                    columns = c("xgroups", "p"),
                    cell = "counts",
                    parenth = "col.percent",
                    sep.char = ", ",
                    test = "chi.fisher",
                    xlevels = NULL,
                    yname = NULL,
                    ylevels = NULL,
                    compress.binary = FALSE,
                    yname.row = TRUE,
                    text.label = NULL,
                    quantiles = NULL,
                    quantile.vals = FALSE,
                    decimals = 1,
                    formatp.list = NULL,
                    n.headings = FALSE,
                    kable = TRUE) {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! is.null(data) && ! is.data.frame(data)) {
    stop("The input 'data' must be a data frame.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "test", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'test', 'p'.")
  }
  if (! cell %in% c("counts", "tot.percent", "col.percent", "row.percent")) {
    stop("The input 'cell' must be one of the following: 'counts', 'tot.percent', 'col.percent', 'row.percent'.")
  }
  if (! parenth %in% c("none", "se", "ci", "counts", "tot.percent",
                       "col.percent", "row.percent")) {
    stop("The input 'parenth' must be one of the following: 'none', 'se', 'ci', 'counts', 'tot.percent', 'col.percent', 'row.percent'.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! test %in% c("chi.fisher", "chi", "fisher", "z", "z.continuity")) {
    stop("The input 'test' must be one of the following: 'chi.fisher', 'chi', 'fisher', 'z', 'z.continuity'.")
  }
  if (! is.null(xlevels) && ! is.character(xlevels)) {
    stop("The input 'xlevels' must be a character vector.")
  }
  if (! is.null(yname) && ! is.character(yname)) {
    stop("The input 'yname' must be a character string.")
  }
  if (! is.null(ylevels) && ! is.character(ylevels)) {
    stop("The input 'ylevels' must be a character vector.")
  }
  if (! is.logical(compress.binary)) {
    stop("The input 'compress.binary' must be a logical.")
  }
  if (! is.logical(yname.row)) {
    stop("The input 'yname.row' must be a logical.")
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
  if (! (is.numeric(decimals) && decimals >= 0 &&
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

  # If x is NULL, set to a vector of 1's
  if (is.null(x)) {
    x <- rep(1, length(y))
    columns <- columns[! columns %in% c("test", "p")]
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

  # Get cell counts and percents
  counts <- table(y, x)
  summary.counts <- summary(counts)
  rowsums.counts <- rowSums(counts)
  colsums.counts <- colSums(counts)
  n <- summary.counts$n.cases
  num.xlevels <- length(colsums.counts)
  num.ylevels <- length(rowsums.counts)
  tot.percents <- 100 * prop.table(counts)
  col.percents <- 100 * prop.table(counts, margin = 2)
  row.percents <- 100 * prop.table(counts, margin = 1)

  # If xlevels or ylevels unspecified, set to actual values
  if (is.null(xlevels)) {
    if (! is.null(quantiles)) {
      if (quantile.vals) {
        xlevels <- paste("Q", 1: num.xlevels, " ", colnames(counts), sep = "")
      } else {
        xlevels <- paste("Q", 1: num.xlevels, sep = "")
      }
    } else {
      xlevels <- colnames(counts)
    }
  }
  if (is.null(ylevels)) ylevels <- rownames(counts)

  # Hypothesis test
  if (test == "chi.fisher") {
    if (summary.counts$approx.ok) {
      test <- "chi"
    } else {
      test <- "fisher"
    }
  }
  if (test == "chi") {
    if (summary.counts$approx.ok) {
    } else {
      message(paste("Pearson's chi-square test was used to test whether the distribution of ",
                    yname, " differed across groups. Assumptions were violated, so you may want to switch to Fisher's exact test.", sep = ""))
    }
    test.stat <- summary.counts$statistic
    test.label <- "Chi-sq"
    p <- summary.counts$p.value
  } else if (test == "fisher") {
    fit <- fisher.test(x = x, y = y)
    test.stat <- "-"
    p <- fit$p.value
  } else if (test == "z") {
    fit <- prop.test(x = counts, correct = FALSE)
    test.stat <- fit$statistic
    test.label <- "Chi-sq"
    p <- fit$p.value
  } else if (test == "z.continuity") {
    fit <- prop.test(x = counts)
    test.stat <- fit$statistic
    test.label <- "Chi-sq"
    p <- fit$p.value
  }

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Initialize table
  df <- data.frame(Variable = ylevels, stringsAsFactors = FALSE)

  # Loop through and add columns requested
  for (column in columns) {

    if (column == "n") {

      df$N <- ""
      df$N[1] <- n

    } else if (column == "overall") {

      if (cell == "counts") {
        part1 <- rowsums.counts
      } else if (cell %in% c("tot.percent", "col.percent")) {
        part1 <- sprintf(spf, rowsums.counts / n * 100)
      }
      if (parenth == "none") {
        part2 <- NULL
      } else if (parenth == "counts") {
        part2 <- paste(" (", rowsums.counts, ")", sep = "")
      } else if (parenth == "se") {
        y.percents <- rowsums.counts / n * 100
        part2 <- paste(" (",
                       sprintf(spf, sqrt(y.percents * (100 - y.percents) / n)),
                       ")", sep = "")
      } else if (parenth == "ci") {
        y.percents <- rowsums.counts / n * 100
        zcrit <- qnorm(p = 0.975)
        ses <- sqrt(y.percents * (100 - y.percents) / n)
        lower <- y.percents - zcrit * ses
        upper <- y.percents + zcrit * ses
        part2 <- paste(" (", sprintf(spf, lower), sep.char,
                       sprintf(spf, upper), ")", sep = "")
      } else if (parenth %in% c("tot.percent", "col.percent", "row.percent")) {
        y.percents <- rowsums.counts / n * 100
        part2 <- paste(" (", sprintf(spf, y.percents), ")", sep = "")
      }

      df$Overall <- paste(part1, part2, sep = "")

    } else if (column == "xgroups") {

      # Cell (parenth)
      if (cell == "counts") {
        part1 <- sprintf("%.0f", counts)
      } else if (cell == "tot.percent") {
        part1 <- sprintf(spf, tot.percents)
      } else if (cell == "col.percent") {
        part1 <- sprintf(spf, col.percents)
      } else if (cell == "row.percent") {
        part1 <- sprintf(spf, row.percents)
      }
      if (parenth == "none") {
        part2 <- NULL
      } else if (parenth == "counts") {
        part2 <- paste(" (", counts, ")", sep = "")
      } else if (parenth == "se") {
        if (cell == "tot.percent") {
          ses <- sqrt(tot.percents * (100 - tot.percents) / n)
          part2 <- paste(" (", sprintf(spf, ses), ")", sep = "")
        } else if (cell %in% c("counts", "col.percent")) {
          ses <- sqrt(col.percents * (100 - col.percents) /
                        matrix(rep(colsums.counts, each = num.ylevels),
                               ncol = num.xlevels))
          part2 <- paste(" (", sprintf(spf, ses), ")", sep = "")
        } else if (cell == "row.percent") {
          ses <- sqrt(row.percents * (100 - row.percents) /
                        matrix(rep(rowsums.counts, each = num.xlevels),
                               nrow = num.ylevels, byrow = TRUE))
          part2 <- paste(" (", sprintf(spf, ses), ")", sep = "")
        }
      } else if (parenth == "ci") {
        zcrit <- qnorm(p = 0.975)
        if (cell == "tot.percent") {
          ses <- sqrt(tot.percents * (100 - tot.percents) / n)
        } else if (cell %in% c("counts", "col.percent")) {
          ses <- sqrt(col.percents * (100 - col.percents) /
                        matrix(rep(colsums.counts, each = num.ylevels),
                               ncol = num.xlevels))
        } else if (cell == "row.percent") {
          ses <- sqrt(row.percents * (100 - row.percents) /
                        matrix(rep(rowsums.counts, each = num.xlevels),
                               nrow = num.ylevels, byrow = TRUE))
        }
        lower <- tot.percents - zcrit * ses
        upper <- tot.percents + zcrit * ses
        part2 <- paste(" (", sprintf(spf, lower), sep.char,
                       sprintf(spf, upper), ")", sep = "")
      } else if (parenth == "counts") {
        part2 <- paste(" (", counts, ")", sep = "")
      } else if (parenth == "tot.percent") {
        part2 <- paste(" (", sprintf(spf, tot.percents), ")", sep = "")
      } else if (parenth == "col.percent") {
        part2 <- paste(" (", sprintf(spf, col.percents), ")", sep = "")
      } else if (parenth == "row.percent") {
        part2 <- paste(" (", sprintf(spf, row.percents), ")", sep = "")
      }

      newcols <- matrix(paste(part1, part2, sep = ""), ncol = num.xlevels,
                        dimnames = list(NULL, xlevels))
      df <- cbind(df, newcols, stringsAsFactors = FALSE)

    } else if (column == "test") {

      newcol <- c(sprintf(spf, test.stat), rep("", num.ylevels - 1))
      names(newcol) <- test.label
      df <- cbind(df, newcol)

    } else if (column == "p") {

      df$P <- ""
      df$P[1] <- do.call(formatp, c(list(p = p), formatp.list))

    }

  }

  # Remove first row if requested
  if (compress.binary & num.ylevels == 2) {
    row1 <- df[1, , drop = FALSE]
    df <- df[-1, , drop = FALSE]
    summary.cols <- which(names(df) %in% c("N", "Chi-sq", "P"))
    df[1, summary.cols] <- row1[1, summary.cols]
  }

  # Add yname row and indent ylevels if requested
  if (yname.row) {
    spaces <- "&nbsp; &nbsp; &nbsp;"
    row1 <- df[1, , drop = FALSE]
    df[, 1] <- paste(spaces, df[, 1], sep = "")
    df <- rbind(c(yname, rep("", ncol(df) - 1)), df)
    summary.cols <- which(colnames(df) %in% c("N", "Chi-sq", "P"))
    df[1, summary.cols] <- row1[1, summary.cols]
    df[2, summary.cols] <- ""
  }

  # Add text.label to first entry of first column, whether it happens to be
  # yname or ylevels[1]
  if (is.null(text.label)) {
    if (cell == "counts") {
      part1 <- "n"
    } else if (cell %in% c("tot.percent", "col.percent", "row.percent")) {
      part1 <- "%"
    }
    if (parenth == "none") {
      text.label <- paste(", ", part1, sep = "")
    } else if (parenth == "se") {
      text.label <- paste(", ", part1, " (SE)", sep = "")
    } else if (parenth == "ci") {
      text.label <- paste(", ", part1, " (95% CI)", sep = "")
    } else if (parenth %in% c("tot.percent", "col.percent", "row.percent")) {
      text.label <- paste(", ", part1, " (%)", sep = "")
    }
  } else {
    text.label <- paste(",", text.label)
  }
  df[1, 1] <- paste(df[1, 1], text.label, sep = "")

  # Add sample sizes to column headings if requested
  if (n.headings) {

    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", n, ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (n = ", colsums.counts, ")", sep = "")

  }

  # Return table
  if (! kable) return(df)
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
