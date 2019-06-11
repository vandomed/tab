#' Generate Summary Table Comparing Group Means (Complex Survey Data)
#'
#' Creates table (or figure) comparing the mean of \code{y} across levels of
#' \code{x}.
#' 
#' Basically \code{\link{tabmeans}} for survey data. Relies heavily on the 
#' \strong{survey} package [1,2].
#'
#' @section Note:
#' If you want to paste your tables into Microsoft Word, you can use either of
#' these approaches:
#'
#' \enumerate{
#'
#' \item Use the \emph{write.cb} function in \strong{Kmisc} [2].
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
#'
#'
#'
#'
#'
#'
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

tabmeans_svy <- function(x, y, svy, latex = FALSE, xlevels = NULL,
                         yname = "Y variable", test = "Wald", decimals = 1,
                         p.decimals = c(2, 3), p.cuts = 0.01,
                         p.lowerbound = 0.001, p.leading0 = TRUE,
                         p.avoid1 = FALSE, n.column = FALSE, n.headings = TRUE,
                         bold.colnames = TRUE, bold.varnames = FALSE,
                         variable.colname = "Variable", print.html = FALSE,
                         html.filename = "table1.html") {

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
  if (! test %in% c("Wald", "LRT")) {
    stop("For test input, please enter 'Wald' or 'LRT'")
  }
  if (!is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
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

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Save x and y as character strings
  xstring <- x
  ystring <- y

  # Extract vectors x and y
  x <- svy$variables[, xstring]
  y <- svy$variables[, ystring]

  # Update survey object to include y and x explicitly
  svy2 <- update(svy, y = y, x = x)

  # Drop missing values if present
  locs <- which(!is.na(x) & !is.na(y))
  if (length(locs) < nrow(svy2)) {
    svy2 <- subset(svy2, !is.na(x) & !is.na(y))
    x <- svy2$variables[, xstring]
    y <- svy2$variables[, ystring]
  }

  # Get unique values of x
  xvals <- sort(unique(x))

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- xvals
  }

  # Initialize table
  tbl <- matrix("", nrow = 1, ncol = length(xlevels) + 4)

  # Get means and SE's overall and by levels of x, and get sample size in each x
  totmean <- svymean(y, design = svy2)
  means <- svyby(~y, by = ~x, FUN = svymean, design = svy2)
  ns <- tapply(X = y, INDEX = x, FUN = length)

  # Add mean (SE) values to table
  tbl[1, 1] <- paste(yname, ", M (SE)", sep = "")
  tbl[1, 2] <- sprintf("%.0f", sum(ns))
  tbl[1, 3] <- paste(sprintf(spf, totmean), " (",
                     sprintf(spf, sqrt(attr(totmean, "var"))), ")", sep = "")
  tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means$"y"), " (",
                                   sprintf(spf, means$"se"), ")", sep = "")

  # ANOVA
  fit1 <- svyglm(y ~ 1, design = svy2)
  fit2 <- svyglm(y ~ as.factor(x), design = svy2)
  pval <- anova(fit1, fit2, method = test)$p
  tbl[1, ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals,
                               lowerbound = p.lowerbound, leading0 = p.leading0,
                               avoid1 = p.avoid1)

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

  # If latex is TRUE, do some re-formatting
  if (latex == TRUE) {
    plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
    }
    if (bold.colnames == TRUE) {
      colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
    }
    if (bold.varnames == TRUE) {
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

  # Return table
  return(tbl)

}
