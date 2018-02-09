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
#' @param x Vector indicating group membership for columns of IxJ table.
#'
#' @param y Vector indicating group membership for rows of IxJ table.
#'
#' @param ylevels Character vector with labels for the levels of \code{y}. If
#' unspecified, the values that \code{y} takes on are used. Note that levels of
#' \code{y} are listed in the order that they appear when you run
#' \code{table(y, x)}.
#'
#' @param quantiles Numeric value. If specified, function compares distribution
#' of \code{y} across quantiles of \code{x}. For example, if \code{x} contains
#' BMI values and \code{y} is race setting \code{quantiles = 3} compares the
#' distribution of race across BMI tertiles.
#'
#' @param cell Character string specifying what statistic should appear in cells
#' of the table. Choices are \code{"n"} for counts, \code{"tot.percent"} for
#' table percentage, \code{"col.percent"} for column percentage,
#' \code{"row.percent"} for row percentage, \code{"tot.prop"} for table
#' proportion, \code{"col.prop"} for column proportion, \code{"row.prop"} for
#' row proportion, \code{"n/totn"} for count/total counts, \code{"n/coln"} for
#' count/column count, and \code{"n/rown"} for count/row count.
#'
#' @param parenth Character string specifying what statistic should appear in
#' parentheses after cell values. Choices are \code{"none"}, \code{"se"} for
#' standard error of requested percentage or proportion, \code{"ci"} for 95\%
#' confidence interval for requested percentage or proportion, and
#' \code{"tot.percent"}, \code{"col.percent"}, \code{"row.percent"},
#' \code{"tot.prop"}, \code{"col.prop"}, and \code{"row.prop"} for various
#' percentages and proportions.
#'
#' @param test Character string specifying which test for association between
#' \code{x} and \code{y} should be used. Choices are \code{"chi"} for Pearson's
#' chi-squared test, \code{"fisher"} for Fisher's exact test, \code{"z"} for z
#' test without continuity correction; and \code{"z.continuity"} for z test with
#' continuity correction. \code{"z"} and \code{"z.continuity"} can only be used
#' if \code{x} and \code{y} are binary.
#'
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values. If unspecified, function uses 1 decimal for percentages
#' and 3 decimals for proportions. No decimals are ever used for counts.
#'
#' @param overall.column Logical value for whether to include a column showing
#' the distribution of \code{y} in the full sample.
#'
#' @param compress Logical value for whether binary \code{y} variables should
#' be displayed as a single row rather than two. Which level gets displayed is
#' controlled by \code{compress.val}.
#'
#' @param compress.val Value specifying which level of \code{y} gets displayed
#' when \code{compress = TRUE}.
#'
#' @param bold.varlevels Logical value for whether levels of \code{y} should
#' appear in bold font in the first column. Only used if \code{latex = TRUE}.
#'
#'
#' @return Character matrix with table comparing the distribution of \code{y}
#' across levels of \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset d and drop rows with missing values
#' data(d)
#' d <- d[complete.cases(d), ]
#'
#' # Compare sex distribution by group, with group as column variable
#' freqtable1 <- tabfreq(x = d$Group, y = d$Sex)
#'
#' # Same comparison, but compress table to show Female row only, show percent
#' # (SE) rather than n (percent), and suppress (n = ) from column headings
#' freqtable2 <- tabfreq(x = d$Group, y = d$Sex, compress = TRUE,
#'                       compress.val = "Female", cell = "col.percent",
#'                       parenth = "se", n.headings = FALSE)
#'
#' # Compare sex distribution by race, suppressing (n = ) from column headings
#' # and showing percent (95\% CI) rather than n (percent)
#' freqtable3 <- tabfreq(x = d$Race, y = d$Sex, n.headings = FALSE,
#'                       cell = "col.percent")
#'
#' # Use rbind to create single table comparing sex and race in control vs.
#' # treatment group
#' freqtable4 <- rbind(tabfreq(x = d$Group, y = d$Sex),
#'                     tabfreq(x = d$Group, y = d$Race))
#'
#' # A (usually) faster way to make the above table is to call the the tabmulti
#' # function
#' freqtable5 <- tabmulti(dataset = d, xvarname = "Group",
#'                        yvarnames = c("Sex", "Race"))
#'
#' # freqtable4 and freqtable5 are equivalent
#' all(freqtable4 == freqtable5)
#'
#'
#' @export
tabfreq <- function(x = NULL, y, latex = FALSE, xlevels = NULL, yname = NULL,
                    ylevels = NULL, quantiles = NULL, quantile.vals = FALSE,
                    cell = "n", parenth = NULL, text.label = NULL,
                    parenth.sep = "-", test = "chi", decimals = NULL,
                    p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
                    p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
                    overall.column = TRUE, n.column = FALSE, n.headings = TRUE,
                    compress = FALSE, compress.val = NULL, bold.colnames = TRUE,
                    bold.varnames = FALSE, bold.varlevels = FALSE,
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
    p.include <- FALSE
  }

  # Drop missing values
  locs.complete <- which(!is.na(x) & ! is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]

  # Create quantiles if necessary
  if (! is.null(quantiles)) {
    x <- cut(x = x, breaks = quantile(x, probs = seq(0, 1, 1 / quantiles)),
             include.lowest = TRUE, right = TRUE, dig.lab = 3)
  }

  # Get cell counts and proportions
  counts <- table(y, x)
  props <- 100*prop.table(counts)
  colprops <- 100 * prop.table(counts, margin = 2)

  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! is.null(xlevels) && ! is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (! is.character(yname)) {
    stop("For yname input, please enter character string")
  }
  if (! is.null(ylevels) && ! is.character(ylevels)) {
    stop("For ylevels input, please enter vector of character strings")
  }
  if (! is.null(quantiles) &&
      ! ( is.numeric(quantiles) & length(quantiles) == 1 && quantiles > 1 &
          quantiles == round(quantiles))) {
    stop("For quantiles input, please enter a whole number greater than 1")
  }
  if (! is.logical(quantile.vals)) {
    stop("For quantile.vals input, please enter TRUE or FALSE")
  }
  if (! cell %in% c("n", "percent", "tot.percent", "col.percent", "row.percent",
                    "tot.prop", "col.prop", "row.prop", "n/totn", "n/coln",
                    "n/rown")) {
    stop("For cell input, please enter 'n', 'tot.percent', 'col.percent', 'row.percent', 'tot.prop', 'col.prop', 'row.prop', 'n/totn', 'n/coln', or 'n/rown'")
  }
  if (! is.null(parenth) &&
      ! parenth %in% c("none", "se", "ci", "tot.percent", "col.percent",
                       "row.percent", "tot.prop", "col.prop", "row.prop")) {
    stop("For parenth input, please enter 'none', 'se', 'ci', 'tot.percent', 'col.percent', 'row.percent', 'tot.prop', 'col.prop', or 'row.prop'")
  }
  if (! is.null(text.label) && ! is.character(text.label)) {
    stop("For text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (! is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string (only used if parenth = 'ci'; usually '-' or ', ')")
  }
  if (! test %in% c("chi", "fisher", "z", "z.continuity")) {
    stop("For test input, please enter 'chi', 'fisher', 'z', or 'z.continuity'")
  }
  if (test %in% c("z", "z.continuity") & ! all(dim(counts) == 2)) {
    stop("For test input, 'z' and 'z.continuity' can only be used if both x and y are binary")
  }
  if (! is.null(decimals) && ! is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
  }
  if (! is.logical(p.include)) {
    stop("For p.include input, please enter TRUE or FALSE")
  }
  if (! is.numeric(p.decimals)) {
    stop("For p.decimals input, please enter numeric value or vector")
  }
  if (! is.numeric(p.cuts)) {
    stop("For p.cuts input, please enter numeric value or vector")
  }
  if (! is.numeric(p.lowerbound)) {
    stop("For p.lowerbound input, please enter numeric value")
  }
  if (! is.logical(p.leading0)) {
    stop("For p.leading0 input, please enter TRUE or FALSE")
  }
  if (! is.logical(p.avoid1)) {
    stop("For p.avoid1 input, please enter TRUE or FALSE")
  }
  if (! is.logical(overall.column)) {
    stop("For overall.column input, please enter TRUE or FALSE")
  }
  if (! is.logical(n.column)) {
    stop("For n.column input, please enter TRUE or FALSE")
  }
  if (!is.logical(n.headings)) {
    stop("For n.headings input, please enter TRUE or FALSE")
  }
  if (! is.logical(compress)) {
    stop("For compress input, please enter TRUE or FALSE")
  }
  if (! is.null(compress) && ! is.null(compress.val) &&
      ! compress.val %in% unique(y)) {
    stop("For compress.val input, please ensure that you enter one of the values that y takes on")
  }
  if (!is.logical(bold.colnames)) {
    stop("For bold.colnames input, please enter TRUE or FALSE")
  }
  if (! is.logical(bold.varnames)) {
    stop("For bold.varnames input, please enter TRUE or FALSE")
  }
  if (! is.logical(bold.varlevels)) {
    stop("For bold.varlevels input, please enter TRUE or FALSE")
  }
  if (! is.character(variable.colname)) {
    stop("For variable.colname input, please enter a character string")
  }

  # If cell is "percent" change to "col.percent"
  if (cell == "percent") {
    cell <- "col.percent"
  }

  # If parenth is NULL, set default value based on cell
  if (is.null(parenth)) {
    if (cell %in% c("n", "n/coln")) {
      parenth <- "col.percent"
    } else if (cell == "n/totn") {
      parenth <- "tot.percent"
    } else if (cell == "n/rown") {
      parenth <- "row.percent"
    } else if (cell %in% c("tot.percent", "col.percent", "row.percent",
                           "tot.prop", "col.prop", "row.prop")) {
      parenth <- "ci"
    }
  }

  # If decimals is unspecified, set to appropriate value
  if (is.null(decimals)) {
    if (cell %in% c("tot.percent", "col.percent", "row.percent") |
        parenth %in% c("tot.percent", "col.percent", "row.percent")) {
      decimals <- 1
    } else {
      decimals <- 3
    }
  }

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }

  # Create table for univariate case
  if (ncol(counts) == 1) {

    # Initialize table
    tbl <- matrix("", nrow = nrow(counts) + 1, ncol = 3)

    # Figure out text.label
    if (is.null(text.label)) {
      if (cell == "n") {
        part1 <- "n"
      } else if (cell %in% c("tot.percent", "col.percent")) {
        part1 <- "%"
      } else if (cell %in% c("tot.prop", "col.prop")) {
        part1 <- "prop."
      } else if (cell %in% c("n/totn", "n/coln")) {
        part1 <- "n/total"
      }
      if (parenth == "none") {
        text.label <- part1
      } else if (parenth == "se") {
        text.label <- paste(part1, " (SE)", sep = "")
      } else if (parenth == "ci") {
        text.label <- paste(part1, " (95% CI)", sep = "")
      } else if (parenth %in% c("tot.percent", "col.percent")) {
        text.label <- paste(part1, " (%)", sep = "")
      } else if (parenth %in% c("tot.prop", "col.prop")) {
        text.label <- paste(part1, " (prop.)", sep = "")
      }
    } else if (text.label == "none") {
      text.label <- NULL
    }

    # Add variable name and levels of Y to first row
    tbl[, 1] <- c(yname, paste("  ", ylevels, sep = ""))

    # Add N column
    tbl[1, 2] <- sprintf("%.0f", sum(counts))

    # Cell values and parentheses for each cell
    if (cell == "n") {
      cells <- sprintf("%.0f", counts[, 1])
    } else if (cell %in% c("tot.percent", "col.percent")) {
      cells <- sprintf(spf, counts[, 1] / sum(counts) * 100)
    } else if (cell %in% c("tot.prop", "col.prop")) {
      cells <- sprintf(spf, counts[, 1] / sum(counts))
    } else if (cell %in% c("n/totn", "n/coln")) {
      cells <- paste(counts[, 1], "/", sum(counts), sep = "")
    }
    if (parenth == "none") {
      tbl[2: nrow(tbl), 3] <- cells
    } else if (parenth == "se") {
      if (cell %in% c("tot.percent", "col.percent")) {
        parentheses <-
          sqrt(counts[, 1] / sum(counts) *
                 (1 - counts[, 1] / sum(counts)) / sum(counts)) * 100
      } else if (cell %in% c("tot.prop", "col.prop")) {
        parentheses <- sqrt(counts[, 1] / sum(counts) *
                              (1 - counts[, 1] / sum(counts)) / sum(counts))
      }
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    } else if (parenth == "ci") {
      if (cell %in% c("tot.percent", "col.percent")) {
        conf95 <- matrix(NA, ncol = 2, nrow = nrow(counts))
        for (jj in 1:nrow(counts)) {
          conf95[jj, ] <- binom.test(x = counts[jj, 1],
                                     n = sum(counts))$conf.int * 100
        }
      } else if (cell %in% c("tot.prop", "col.prop")) {
        conf95 <- matrix(NA, ncol = 2, nrow = nrow(counts))
        for (jj in 1: nrow(counts)) {
          conf95[jj, ] <- binom.test(x = counts[jj, 1],
                                     n = sum(counts))$conf.int
        }
      }
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep,
              sprintf(spf, conf95[, 2]), ")", sep = "")
    } else if (parenth %in% c("tot.percent", "col.percent")) {
      parentheses <- counts[, 1] / sum(counts) * 100
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    } else if (parenth %in% c("tot.prop", "col.prop")) {
      parentheses <- counts[, 1] / sum(counts)
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    }

    # If y binary and compress is TRUE, compress table to a single row
    if (nrow(counts) <= 2 & compress) {
      if (is.null(compress.val)) {
        tbl <- matrix(c(ylevels[2], tbl[1, 2], tbl[3, 3]), nrow = 1)
      } else {
        whichrow <- which(rownames(counts) == as.character(compress.val)) + 1
        tbl <- matrix(c(ylevels[whichrow - 1], tbl[1, 2], tbl[whichrow, 3]),
                      nrow = 1)
      }
    }

    # Add column names
    colnames(tbl) <- c(variable.colname, "N", text.label)

    # Drop N column if requested
    if (! n.column) {
      tbl <- tbl[, -2, drop = FALSE]
    }

    # If latex is TRUE, do some re-formatting
    if (latex) {
      spacelocs <- which(substr(tbl[, variable.colname], 1, 2) == "  ")
      if (length(spacelocs) > 0) {
        tbl[spacelocs, variable.colname] <-
          paste("$\\hskip .4cm$",
                substring(tbl[spacelocs, variable.colname], 3), sep = "")
      }
      chars <- strsplit(colnames(tbl), "")
      for (ii in 1:length(chars)) {
        percentlocs <- which(chars[[ii]] == "%")
        if (length(percentlocs) > 0) {
          chars[[ii]][percentlocs] <- "\\%"
        }
      }
      colnames(tbl) <- sapply(chars, function(x)
        paste(x, sep = "", collapse = ""))
      if (bold.colnames) {
        colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
      }
      if (bold.varnames) {
        tbl[1, 1] <- paste("$\\textbf{", tbl[1, 1], "}$")
      }
      if (bold.varlevels) {
        tbl[2:nrow(tbl), 1] <- paste("$\\textbf{", tbl[2: nrow(tbl), 1], "}$",
                                     sep = "")
      }
    }

    # Create table for multivariate case
  } else {

    # Initialize table
    tbl <- matrix("", nrow = nrow(counts) + 1, ncol = ncol(counts) + 4)

    # Figure out text.label
    if (is.null(text.label)) {
      if (cell == "n") {
        part1 <- "n"
      } else if (cell %in% c("tot.percent", "col.percent", "row.percent")) {
        part1 <- "%"
      } else if (cell %in% c("tot.prop", "col.prop", "row.prop")) {
        part1 <- "prop."
      } else if (cell == "n/totn") {
        part1 <- "n/row total"
      } else if (cell == "n/coln") {
        part1 <- "n/column total"
      } else if (cell == "n/rown") {
        part1 <- "n/row total"
      }
      if (parenth == "none") {
        text.label <- paste(", ", part1, sep = "")
      } else if (parenth == "se") {
        text.label <- paste(", ", part1, " (SE)", sep = "")
      } else if (parenth == "ci") {
        text.label <- paste(", ", part1, " (95% CI)", sep = "")
      } else if (parenth %in% c("tot.percent", "col.percent", "row.percent")) {
        text.label <- paste(", ", part1, " (%)", sep = "")
      } else if (parenth %in% c("tot.prop", "col.prop", "row.prop")) {
        text.label <- paste(", ", part1, " (prop.)", sep = "")
      }
    } else if (text.label == "none") {
      text.label <- NULL
    }

    # Add variable name and levels of Y to first row
    tbl[, 1] <- c(paste(yname, text.label, sep = ""),
                  paste("  ", ylevels, sep = ""))

    # Add N column
    tbl[1, 2] <- sprintf("%.0f", sum(counts))

    # Cell values and parentheses for overall y column
    if (cell == "n") {
      cells <- sprintf("%.0f", rowSums(counts))
    } else if (cell %in% c("tot.percent", "col.percent", "row.percent")) {
      cells <- sprintf(spf, rowSums(props))
    } else if (cell %in% c("tot.prop", "col.prop", "row.prop")) {
      cells <- sprintf(spf, rowSums(props) / 100)
    } else if (cell %in% c("n/totn", "n/coln", "n/rown")) {
      cells <- paste(rowSums(counts), "/", sum(counts), sep = "")
    }
    if (parenth == "none") {
      tbl[2:nrow(tbl), 3] <- cells
    } else if (parenth == "se") {
      if (cell %in% c("tot.percent", "col.percent", "row.percent", "n/totn",
                      "n/coln", "n/rown")) {
        parentheses <- sqrt(rowSums(props) / 100 * (1 - rowSums(props) / 100) /
                              sum(counts)) * 100
      } else if (cell %in% c("tot.prop", "col.prop", "row.prop")) {
        parentheses <- sqrt(rowSums(props) / 100 * (1 - rowSums(props) / 100) /
                              sum(counts))
      }
      tbl[2:nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    } else if (parenth == "ci") {
      if (cell %in% c("tot.percent", "col.percent", "row.percent")) {
        conf95 <- matrix(NA, nrow = nrow(counts), ncol = 2)
        for (ii in 1: nrow(counts)) {
          conf95[ii, 1: 2] <- binom.test(x = sum(counts[ii, ]),
                                         n = sum(counts))$conf.int * 100
        }
        tbl[2: nrow(tbl), 3] <-
          paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep,
                sprintf(spf, conf95[, 2]), ")", sep = "")
      } else if (cell %in% c("tot.prop", "col.prop", "row.prop")) {
        conf95 <- matrix(NA, nrow = nrow(counts), ncol = 2)
        for (ii in 1: nrow(counts)) {
          conf95[ii, 1: 2] <- binom.test(x = sum(counts[ii, ]),
                                         n = sum(counts))$conf.int
        }
        tbl[2: nrow(tbl), 3] <-
          paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep,
                sprintf(spf, conf95[, 2]), ")", sep = "")
      }
    } else if (parenth %in% c("tot.percent", "col.percent", "row.percent")) {
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, rowSums(props)), ")", sep = "")
    } else if (parenth %in% c("tot.prop", "col.prop", "row.prop")) {
      tbl[2: nrow(tbl), 3] <-
        paste(cells, " (", sprintf(spf, rowSums(props) / 100), ")", sep = "")
    }

    # Cell values and parentheses for each cell
    for (ii in 1: ncol(counts)) {

      if (cell == "n") {
        cells <- sprintf("%.0f", counts[, ii])
      } else if (cell == "tot.percent") {
        cells <- sprintf(spf, counts[, ii] / sum(counts) * 100)
      } else if (cell == "col.percent") {
        cells <- sprintf(spf, counts[, ii] / sum(counts[, ii]) * 100)
      } else if (cell == "row.percent") {
        cells <- sprintf(spf, counts[, ii] / rowSums(counts) * 100)
      } else if (cell == "tot.prop") {
        cells <- sprintf(spf, counts[, ii] / sum(counts))
      } else if (cell == "col.prop") {
        cells <- sprintf(spf, counts[, ii] / sum(counts[, ii]))
      } else if (cell == "row.prop") {
        cells <- sprintf(spf, counts[, ii] / rowSums(counts))
      } else if (cell == "n/totn") {
        cells <- paste(counts[, ii], "/", sum(counts), sep = "")
      } else if (cell == "n/coln") {
        cells <- paste(counts[, ii], "/", sum(counts[, ii]), sep = "")
      } else if (cell == "n/rown") {
        cells <- paste(counts[, ii], "/", rowSums(counts), sep = "")
      }

      if (parenth == "none") {
        tbl[2: nrow(tbl), (3 + ii)] <- cells
      } else if (parenth == "se") {
        if (cell == "tot.percent") {
          parentheses <- sqrt(counts[, ii] / sum(counts) *
                                (1 - counts[, ii] / sum(counts)) /
                                sum(counts)) * 100
        } else if (cell == "col.percent") {
          parentheses <- sqrt(counts[, ii] / sum(counts[, ii]) *
                                (1 - counts[, ii] / sum(counts[, ii])) /
                                sum(counts[, ii])) * 100
        } else if (cell == "row.percent") {
          parentheses <- sqrt(counts[, ii] / rowSums(counts) *
                                (1 - counts[, ii] / rowSums(counts)) /
                                rowSums(counts)) * 100
        } else if (cell == "tot.prop") {
          parentheses <- sqrt(counts[, ii] / sum(counts) *
                                (1 - counts[, ii] / sum(counts)) / sum(counts))
        } else if (cell == "col.prop") {
          parentheses <- sqrt(counts[, ii] / sum(counts[, ii]) *
                                (1 - counts[, ii] / sum(counts[, ii])) /
                                sum(counts[, ii]))
        } else if (cell == "row.prop") {
          parentheses <- sqrt(counts[, ii] / rowSums(counts) *
                                (1 - counts[, ii] / rowSums(counts)) /
                                rowSums(counts))
        }
        tbl[2: nrow(tbl), (3 + ii)] <- paste(cells, " (",
                                            sprintf(spf, parentheses), ")",
                                            sep = "")
      } else if (parenth == "ci") {
        conf95 <- matrix(NA, nrow = nrow(counts), ncol = 2)
        if (cell == "tot.percent") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts))$conf.int * 100
          }
        } else if (cell == "col.percent") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts[, ii]))$conf.int * 100
          }
        } else if (cell == "row.percent") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts[ii, ]))$conf.int * 100
          }
        } else if (cell == "tot.prop") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts))$conf.int
          }
        } else if (cell == "col.prop") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts[, ii]))$conf.int
          }
        } else if (cell == "row.prop") {
          for (jj in 1:nrow(counts)) {
            conf95[jj, 1: 2] <- binom.test(x = counts[jj, ii],
                                           n = sum(counts[ii, ]))$conf.int
          }
        }
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep,
                sprintf(spf, conf95[, 2]), ")", sep = "")
      } else if (parenth == "tot.percent") {
        parentheses <- counts[, ii] / sum(counts) * 100
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      } else if (parenth == "col.percent") {
        parentheses <- counts[, ii] / sum(counts[, ii]) * 100
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      } else if (parenth == "row.percent") {
        parentheses <- counts[, ii] / rowSums(counts) * 100
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      } else if (parenth == "tot.prop") {
        parentheses <- counts[, ii] / sum(counts)
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      } else if (parenth == "col.prop") {
        parentheses <- counts[, ii] / sum(counts[, ii])
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      } else if (parenth == "row.prop") {
        parentheses <- counts[, ii] / rowSums(counts)
        tbl[2: nrow(tbl), (3 + ii)] <-
          paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
      }
    }

    # Statistical test
    if (! p.include | nrow(counts) == 1) {
      pval = "-"
    } else {
      if (test == "chi") {
        pval <- chisq.test(x = x, y = y)$p.value
        message(paste("Pearson's chi-square test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
      } else if (test == "fisher") {
        pval <- fisher.test(x = x, y = y)$p.value
        message(paste("Fisher's exact test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
      } else if (test == "z") {
        pval <- prop.test(x = counts, correct = FALSE)$p.value
        message(paste("A z-test (without continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
      } else if (test == "z.continuity") {
        pval <- prop.test(x = counts)$p.value
        message(paste("A z-test (with continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
      }
      tbl[1, ncol(tbl)] <- formatp(p = pval, cuts = p.cuts,
                                   decimals = p.decimals,
                                   lowerbound = p.lowerbound,
                                   leading0 = p.leading0, avoid1 = p.avoid1)
    }

    # If xlevels unspecified, set to actual values
    xvals <- colnames(counts)
    if (is.null(xlevels)) {
      if (!is.null(quantiles)) {
        if (quantile.vals) {
          xlevels <-
            paste("Q", 1: length(xvals), " ", as.character(xvals), sep = "")
        } else {
          xlevels <- paste("Q", 1: length(xvals), sep = "")
        }
      } else {
        xlevels <- as.character(xvals)
      }
    }

    # If y binary and compress is TRUE, compress table to a single row
    if (nrow(counts) <= 2 & compress) {
      if (is.null(compress.val)) {
        tbl <-
          matrix(c(paste(yname, text.label, sep = ""),
                   tbl[3, 2: (ncol(tbl) - 1)], tbl[1, ncol(tbl)]), nrow = 1)
      } else {
        whichrow <- which(rownames(counts) == as.character(compress.val)) + 1
        tbl <-
          matrix(c(paste(yname, text.label, sep = ""),
                   tbl[whichrow, 2: (ncol(tbl) - 1)],
                   tbl[1, ncol(tbl)]), nrow = 1)
      }
    }

    # Add column names, with sample sizes for each group if requested
    if (! n.headings) {
      colnames(tbl) <- c(variable.colname, "N", "Overall", xlevels, "P")
    } else {
      colnames(tbl) <- c(variable.colname, "N",
                         paste(c("Overall", xlevels), " (n = ",
                               c(sum(counts), apply(counts, 2, sum)), ")",
                               sep = ""), "P")
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

    # If latex is TRUE, do some re-formatting
    if (latex) {
      if (p.include) {
        plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
        if (length(plocs) > 0) {
          tbl[plocs, "P"] <-
            paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
        }
      }
      spacelocs <- which(substr(tbl[, variable.colname], 1, 2) == "  ")
      if (length(spacelocs) > 0) {
        tbl[spacelocs, variable.colname] <-
          paste("$\\hskip .4cm$",
                substring(tbl[spacelocs, variable.colname], 3), sep = "")
      }
      chars <- strsplit(tbl[, variable.colname], "")
      for (ii in 1: length(chars)) {
        percentlocs <- which(chars[[ii]] == "%")
        if (length(percentlocs) > 0) {
          chars[[ii]][percentlocs] <- "\\%"
        }
      }
      tbl[, variable.colname] <- sapply(chars, function(x)
        paste(x, sep = "", collapse = ""))
      if (bold.colnames) {
        colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
      }
      if (bold.varnames) {
        tbl[1, 1] <- paste("$\\textbf{", tbl[1, 1], "}$")
      }
      if (bold.varlevels) {
        tbl[2: nrow(tbl), 1] <-
          paste("$\\textbf{", tbl[2: nrow(tbl), 1], "}$", sep = "")
      }
    }

  }

  # Print html version of table if requested
  if (print.html) {

    tbl.xtable <-
      xtable(tbl, align = paste("ll",
                                paste(rep("r", ncol(tbl) - 1), collapse = ""),
                                sep = "", collapse = ""))
    print(tbl.xtable, include.rownames = FALSE, type = "html",
          file = html.filename,
          sanitize.text.function = function(x) {
            ifelse(substr(x, 1, 1) == " ", paste("&nbsp &nbsp", x), x)
          })

  }

  # Return table
  return(tbl)

}
