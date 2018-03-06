#' Generate Frequency Table
#'
#' Creates I-by-J frequency table comparing the distribution of \code{y} across
#' levels of \code{x}.
#'
#'
#' @inherit tabmeans references
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#'
#'
#' @param x Vector indicating group membership for columns of I-by-J table.
#'
#' @param y Vector indicating group membership for rows of I-by-J table.
#'
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall distribution of \code{y}, \code{"xgroups"} for distributions of
#' \code{y} for each \code{x} group, \code{"test"} for test statistic, and
#' \code{"p"} for p-value.
#'
#' @param cell Character string specifying what statistic to display in cells.
#' Choices are \code{"counts"}, \code{"tot.percent"}, \code{"col.percent"},
#' and \code{"row.percent"}.
#'
#' @param parenth Character string specifying what statistic to display in
#' parentheses. Choices are \code{"none"}, \code{"se"}, \code{"ci"},
#' \code{"counts"}, \code{"tot.percent"}, \code{"col.percent"}, and
#' \code{"row.percent"}.
#'
#' @param test Character string specifying which test for association between
#' \code{x} and \code{y} should be used. Choices are \code{"chi.fisher"} for
#' Pearson's chi-squared test if its assumptions are met, otherwise Fisher's
#' exact test; \code{"chi"}; \code{"fisher"}; \code{"z"} for z test without
#' continuity correction; and \code{"z.continuity"} for z test with continuity
#' correction. The last two only work if both \code{x} and \code{y} are binary.
#'
#' @param ylevels Character vector with labels for the levels of \code{y}. Note
#' that levels of \code{y} are listed in the order that they appear when you run
#' \code{table(y, x)}.
#'
#' @param compress.binary Logical value for whether to compress binary \code{y}
#' variable to a single row, excluding the first level rather than showing both.
#'
#' @param yname.row Logical value for whether to include a row displaying the
#' name of the \code{y} variable.
#'
#'
#' @return Character matrix comparing the distribution of \code{y} across levels
#' of \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset and drop rows with missing values
#' data(tabdata)
#' tabdata <- tabdata[complete.cases(tabdata), ]
#'
#' # Compare sex distribution by group, with group as column variable
#' (freqtable1 <- tabfreq(x = tabdata$Group, y = tabdata$Sex))
#'
#' # Same comparison, but compress table to show male row only and show percent
#' # (SE) rather than n (percent)
#' (freqtable2 <- tabfreq(x = tabdata$Group, y = tabdata$Sex,
#'                        cell = "col.percent", parenth = "se",
#'                        compress.binary = TRUE))
#'
#' # Use rbind to create single table comparing sex and race in control vs.
#' # treatment group
#' (freqtable3 <- rbind(tabfreq(x = tabdata$Group, y = tabdata$Sex),
#'                      tabfreq(x = tabdata$Group, y = tabdata$Race)))
#'
#' # An easier way to make this table is to use tabmulti
#' (freqtable4 <- tabmulti(data = d, xvarname = "Group",
#'                         yvarnames = c("Sex", "Race")))
#'
#'
#' @export
tabfreq <- function(x = NULL, y, columns = c("xgroups", "p"),
                    cell = "counts", parenth = "col.percent", sep.char = ", ",
                    test = "chi.fisher", xlevels = NULL, yname = NULL,
                    ylevels = NULL, compress.binary = FALSE,
                    yname.row = ! compress.binary, text.label = NULL,
                    quantiles = NULL, quantile.vals = FALSE,
                    latex = FALSE, decimals = 1, p.decimals = c(2, 3),
                    p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
                    p.avoid1 = FALSE, n.headings = FALSE,
                    variable.colname = "Variable", print.html = FALSE,
                    html.filename = "table1.html") {

  # If yname unspecified, use variable name
  if (is.null(yname)) {
    yname <- deparse(substitute(y))
    if (grepl("\\$", yname)) {
      yname <- strsplit(yname, "\\$")[[1]][2]
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

  # If xlevels unspecified, set to actual values
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

  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }

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
      message(paste("Pearson's chi-square test was used to test whether the distribution of ",
                    yname, " differed across groups.", sep = ""))
    } else {
      message(paste("Pearson's chi-square test was used to test whether the distribution of ",
                    yname, " differed across groups. Assumptions were violated, so you may want to switch to Fisher's exact test.", sep = ""))
    }
    test.stat <- summary.counts$statistic
    test.label <- "Chi-sq"
    p <- summary.counts$p.value
  } else if (test == "fisher") {
    fit <- fisher.test(x = x, y = y)
    message(paste("Fisher's exact test was used to test whether the distribution of ",
                  yname, " differed across groups.", sep = ""))
    test.stat <- "-"
    p <- fit$p.value
  } else if (test == "z") {
    fit <- prop.test(x = counts, correct = FALSE)
    message(paste("A z-test (without continuity correction) was used to test whether proportions of ",
                  yname, " differed in the two groups.", sep = ""))
    test.stat <- fit$statistic
    test.label <- "Chi-sq"
    p <- fit$p.value
  } else if (test == "z.continuity") {
    fit <- prop.test(x = counts)
    message(paste("A z-test (with continuity correction) was used to test whether proportions of ",
                  yname, " differed in the two groups.", sep = ""))
    test.stat <- fit$statistic
    test.label <- "Chi-sq"
    p <- fit$p.value
  }

  # Initialize table
  tbl <- matrix(ylevels, ncol = 1, dimnames = list(NULL, variable.colname))

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Loop through column input and add each
  for (ii in 1: length(columns)) {

    column.ii <- columns[ii]

    if (column.ii == "n") {

      # N
      newcol <- matrix(c(n, rep("", num.ylevels - 1)),
                       dimnames = list(NULL, "N"))

    } else if (column.ii == "overall") {

      # Overall
      if (cell == "counts") {
        part1 <- rowsums.counts
      } else if (cell %in% c("tot.percent", "col.percent")) {
        part1 <- sprintf(spf, rowsums.counts / n)
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

      newcol <- matrix(paste(part1, part2, sep = ""), ncol = 1,
                       dimnames = list(NULL, "Overall"))

    } else if (column.ii == "xgroups") {

      # Cell (parenth)
      if (cell == "counts") {
        part1 <- as.vector(counts)
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
      newcol <- matrix(paste(part1, part2, sep = ""), ncol = num.xlevels,
                       dimnames = list(NULL, xlevels))

    } else if (column.ii == "test") {

      # Chi-sq.
      newcol <- matrix(c(sprintf(spf, test.stat), rep("", num.ylevels - 1)),
                       dimnames = list(NULL, test.label))

    } else if (column.ii == "p") {

      # P
      p.formatted <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                             lowerbound = p.lowerbound, leading0 = p.leading0,
                             avoid1 = p.avoid1)
      newcol <- matrix(c(p.formatted, rep("", num.ylevels - 1)),
                       dimnames = list(NULL, "P"))

    }

    # Add column to table
    tbl <- cbind(tbl, newcol)

  }

  # Remove first row if requested
  if (compress.binary & num.ylevels == 2) {
    row1 <- tbl[1, , drop = FALSE]
    tbl <- tbl[-1, , drop = FALSE]
    summary.cols <- which(colnames(tbl) %in% c("N", "Chi-sq", "P"))
    tbl[1, summary.cols] <- row1[1, summary.cols]
  }

  # Add yname row and indent ylevels if requested
  if (yname.row) {
    row1 <- tbl[1, , drop = FALSE]
    tbl[, 1] <- paste(ifelse(latex, "\\ \\ \\ \\ ", "  "), tbl[, 1], sep = "")
    tbl <- rbind(c(yname, rep("", ncol(tbl) - 1)), tbl)
    summary.cols <- which(colnames(tbl) %in% c("N", "Chi-sq", "P"))
    tbl[1, summary.cols] <- row1[1, summary.cols]
    tbl[2, summary.cols] <- ""
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
  tbl[1, 1] <- paste(tbl[1, 1], text.label, sep = "")

  # Add sample sizes to column headings if requested
  if (n.headings) {

    colnames(tbl)[colnames(tbl) == "Overall"] <-
      paste("Overall (n = ", n, ")", sep = "")
    colnames(tbl)[colnames(tbl) %in% xlevels] <-
      paste(xlevels, " (n = ", colsums.counts, ")", sep = "")

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
