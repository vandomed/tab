#' Create Frequency Table (for Complex Survey Data)
#'
#' Creates an I-by-J frequency table comparing the distribution of \code{y}
#' across levels of \code{x}.
#'
#' Basically \code{\link{tabmedians}} for complex survey data. Relies heavily on
#' the \pkg{survey} package.
#'
#' @param formula Formula, e.g. \code{Race ~ Sex}.
#' @param design Survey design object from \code{\link[survey]{svydesign}}.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total unweighted sample size, \code{"N"}
#' for total weighted sample size, \code{"overall"} for overall distribution of
#' \code{y}, \code{"xgroups"} for distributions of \code{y} for each \code{x}
#' group, and \code{"p"} for Chi-square p-value.
#' @param cell Character string specifying what statistic to display in cells.
#' Choices are \code{"n"}, \code{"N"}, and \code{"col.percent"}.
#' @param parenth Character string specifying what statistic to display in
#' parentheses. Choices are \code{"none"}, \code{"n"}, \code{"N"},
#' \code{"col.percent"}, \code{"se"}, and \code{"ci"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yname Character string with a label for the \code{y} variable.
#' @param ylevels Character vector with labels for the levels of \code{y}. Note
#' that levels of \code{y} are listed in the order that they appear when you run
#' \code{table(y, x)}.
#' @param compress.binary Logical value for whether to compress binary \code{y}
#' variable to a single row, excluding the first level rather than showing both.
#' @param yname.row Logical value for whether to include a row displaying the
#' name of the \code{y} variable.
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param svychisq.list List of arguments to pass to
#' \code{\link[survey]{svychisq}}.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display unweighted sample
#' sizes in parentheses in column headings.
#' @param N.headings Logical value for whether to display weighted sample sizes
#' in parentheses in column headings.
#' @param kable Logical value for whether to return a
#' \code{\link[knitr]{kable}}.
#'
#'
#'
#' @return \code{\link[knitr]{kable}} or character matrix.
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
#' # Compare race distribution by sex
#' tabfreq.svy(Race ~ Sex, design = design) %>% kable()
#'
#'
#' @export
tabfreq.svy <- function(formula,
                        design,
                        columns = c("xgroups", "p"),
                        cell = "col.percent",
                        parenth = "se",
                        sep.char = ", ",
                        xlevels = NULL,
                        yname = NULL,
                        ylevels = NULL,
                        compress.binary = FALSE,
                        yname.row = TRUE,
                        text.label = NULL,
                        decimals = 1,
                        svychisq.list = NULL,
                        formatp.list = NULL,
                        n.headings = FALSE,
                        N.headings = FALSE,
                        kable = TRUE) {

  # Error checking
  if (! is.null(formula) && class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! "survey.design" %in% class(design)) {
    stop("The input 'design' must be a survey design object.")
  }
  if (! all(columns %in% c("n", "N", "overall", "xgroups", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'N', 'overall', 'xgroups', 'p'.")
  }
  if (! cell %in% c("n", "N", "col.percent")) {
    stop("The input 'cell' must be one of the following: 'n', 'N', 'col.percent'.")
  }
  if (! parenth %in% c("none", "n", "N", "col.percent", "se", "ci")) {
    stop("The input 'parenth' must be one of the following: 'none', 'n', 'N', 'col.pecent', 'se', 'ci'.")
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
  if (! (is.numeric(decimals) && decimals >= 0 &&
         decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(svychisq.list) &&
      ! (is.list(svychisq.list) && all(names(svychisq.list) %in%
                                       names(as.list(args(svychisq)))))) {
    stop("The input 'svychisq.list' must be a named list of arguments to pass to 'svychisq'.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in%
                                      names(as.list(args(formatp)))))) {
    stop("The input 'formatp.list' must be a named list of arguments to pass to 'formatp'.")
  }
  if (! is.logical(n.headings)) {
    stop("The input 'n.headings' must be a logical.")
  }
  if (! is.logical(N.headings)) {
    stop("The input 'n.headings' must be a logical.")
  }
  if (! is.logical(kable)) {
    stop("The input 'kable' must be a logical.")
  }

  # Get variable names etc.
  varnames <- all.vars(formula)
  xvarname <- varnames[2]
  yvarname <- varnames[1]
  if (is.null(yname)) yname <- yvarname

  # Drop missing values
  design <- subset(design, complete.cases(design$variables[, c(xvarname, yvarname)]))
  # design <- eval(parse(text = paste("subset(design, ! is.na(", xvarname, ") & ! is.na(", yvarname, "))", sep = "")))
  # design <- eval(str2expression(paste("subset(design, ! is.na(", xvarname, ") & ! is.na(", yvarname, "))", sep = "")))

  # Extract x and y values
  x <- design$variables[, xvarname]
  y <- design$variables[, yvarname]

  # Calculate various statistics
  counts <- table(y, x)
  rowsums.counts <- rowSums(counts)
  colsums.counts <- colSums(counts)
  n <- sum(counts)

  svycounts <- svytable(as.formula(paste("~", yvarname, "+", xvarname, sep = "")),
                        design = design)
  rowsums.svycounts <- rowSums(svycounts)
  colsums.svycounts <- colSums(svycounts)
  N <- sum(svycounts)

  xvals <- colnames(counts)
  yvals <- rownames(counts)

  # If xlevels or ylevels unspecified, set to actual values
  if (is.null(xlevels)) xlevels <- xvals
  if (is.null(ylevels)) ylevels <- yvals

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Initialize table
  df <- data.frame(Variable = ylevels, stringsAsFactors = FALSE)

  # Loop through and add columns requested
  for (column in columns) {

    if (column == "n") {

      df$N <- ""
      df$N[1] <- n

    } else if (column == "N") {

      df$N <- ""
      df$N[1] <- N

    } else if (column == "overall") {

      svymean.overall <- svymean(as.formula(paste("~", yvarname, sep = "")), design = design)
      percents <- as.data.frame(svymean.overall)$mean * 100
      ses <- as.data.frame(svymean.overall)$SE * 100

      if (cell == "n") {
        part1 <- rowsums.counts
      } else if (cell == "N") {
        part1 <- rowsums.svycounts
      } else if (cell == "col.percent") {
        part1 <- percents
      }
      if (parenth == "none") {
        part2 <- NULL
      } else if (parenth == "n") {
        part2 <- rowsums.counts
      } else if (parenth == "N") {
        part2 <- rowsums.svycounts
      } else if (parenth == "col.percent") {
        part2 <- percents
      } else if (parenth == "se") {
        part2 <- ses
      } else if (parenth == "ci") {
        zcrit <- qnorm(p = 0.975)
        lower <- percents - zcrit * ses
        upper <- percents + zcrit * ses
        part2 <- paste(" (", sprintf(spf, lower), sep.char,
                       sprintf(spf, upper), ")", sep = "")
      }

      df$Overall <- paste(part1, part2, sep = "")

    } else if (column == "xgroups") {

      svymeans <- lapply(X = xvals, FUN = function(x) {
        as.data.frame(svymean(as.formula(paste("~", yvarname, sep = "")),
                              design = subset(design, design$variables[, xvarname] == x)))
      })
      # svymeans <- lapply(X = xvals, FUN = function(x) {
      #   as.data.frame(svymean(as.formula(paste("~", yvarname, sep = "")),
      #                         design = eval(parse(text = paste("subset(design, ", xvarname, " == '", x, "')", sep = "")))))
      # })
      # svymeans <- lapply(X = xvals, FUN = function(x) {
      #   as.data.frame(svymean(as.formula(paste("~", yvarname, sep = "")),
      #           design = eval(str2expression(paste("subset(design, ", xvarname, " == '", x, "')", sep = "")))))
      # })
      col.percents <- do.call(cbind, lapply(svymeans, function(x) x[[1]])) * 100
      ses <- do.call(cbind, lapply(svymeans, function(x) x[[2]])) * 100

      if (cell == "n") {
        part1 <- sprintf("%.0f", counts)
      } else if (cell == "N") {
        part1 <- sprintf("%.0f", svycounts)
      } else if (cell == "col.percent") {
        part1 <- sprintf(spf, col.percents)
      }
      if (parenth == "none") {
        part2 <- NULL
      } else if (parenth == "n") {
        part2 <- paste(" (", sprintf("%.0f", counts), ")", sep = "")
      } else if (parenth == "N") {
        part2 <- paste(" (", sprintf("%.0f", svycounts), ")", sep = "")
      } else if (parenth == "col.percent") {
        part2 <- paste(" (", sprintf(spf, col.percents), ")", sep = "")
      } else if (parenth == "se") {
        part2 <- paste(" (", sprintf(spf, ses), ")", sep = "")
      } else if (parenth == "ci") {
        zcrit <- qnorm(0.975)
        lower <- col.percents - zcrit * ses
        upper <- col.percents + zcrit * ses
        part2 <- paste(" (", sprintf(spf, lower), sep.char,
                       sprintf(spf, upper), ")", sep = "")
      }

      newcols <- matrix(paste(part1, part2, sep = ""), ncol = length(xlevels),
                        dimnames = list(NULL, xlevels))
      df <- cbind(df, newcols, stringsAsFactors = FALSE)

    } else if (column == "p") {

      p <- do.call(
        svychisq,
        c(list(formula = as.formula(paste("~", xvarname, "+", yvarname, sep = "")),
               design = design),
          svychisq.list)
      )$p.value
      df$P <- ""
      df$P[1] <- do.call(formatp, c(list(p = p), formatp.list))
    }

  }

  # Remove first row if requested
  if (compress.binary & length(ylevels) == 2) {
    row1 <- df[1, , drop = FALSE]
    df <- df[-1, , drop = FALSE]
    summary.cols <- which(names(df) %in% c("n", "N", "P"))
    df[1, summary.cols] <- row1[1, summary.cols]
  }

  # Add yname row and indent ylevels if requested
  if (yname.row) {
    spaces <- "&nbsp; &nbsp; &nbsp;"
    row1 <- df[1, , drop = FALSE]
    df[, 1] <- paste(spaces, df[, 1], sep = "")
    df <- rbind(c(yname, rep("", ncol(df) - 1)), df)
    summary.cols <- which(colnames(df) %in% c("n", "N", "P"))
    df[1, summary.cols] <- row1[1, summary.cols]
    df[2, summary.cols] <- ""
  }

  # Add text.label to first entry of first column, whether it happens to be
  # yname or ylevels[1]
  if (is.null(text.label)) {
    if (cell == "n") {
      part1 <- "n"
    } else if (cell == "N") {
      part1 <- "N"
    } else if (cell == "col.percent") {
      part1 <- "%"
    }
    if (parenth == "none") {
      text.label <- paste(", ", part1, sep = "")
    } else if (parenth == "n") {
      text.label <- paste(", ", part1, " (n)", sep = "")
    } else if (parenth == "N") {
      text.label <- paste(", ", part1, " (N)", sep = "")
    } else if (parenth == "col.percent") {
      text.label <- paste(", ", part1, " (%)", sep = "")
    } else if (parenth == "se") {
      text.label <- paste(", ", part1, " (SE)", sep = "")
    } else if (parenth == "ci") {
      text.label <- paste(", ", part1, " (95% CI)", sep = "")
    }
  }
  df[1, 1] <- paste(df[1, 1], text.label, sep = "")

  # Add sample sizes to column headings if requested
  if (n.headings) {
    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", n, ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (n = ", colsums.counts, ")", sep = "")
  } else if (N.headings) {
    names(df)[names(df) == "Overall"] <- paste("Overall (N = ", N, ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (N = ", colsums.svycounts, ")", sep = "")
  }

  # Return table
  if (! kable) return(df)
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
