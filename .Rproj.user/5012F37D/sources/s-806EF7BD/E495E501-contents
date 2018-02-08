#' Generate Multi-row Table Comparing Multiple Variables Across Levels of One
#' Categorical Variable
#'
#' Basically provides an alternative to making multiple calls to
#' \code{\link{tabmeans}}, \code{\link{tabmedians}}, and \code{\link{tabfreq}},
#' then using \code{\link[base]{rbind}} to combine the results into a single
#' table.
#'
#'
#' @inherit tabmeans references
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#'
#'
#' @param dataset Data frame.
#'
#' @param xvarname Character string with name of column variable. Should be one
#' of \code{names(dataset)}.
#'
#' @param yvarnames Character vector with names of row variables. Each element
#' should be one of \code{names(dataset)}.
#'
#' @param ymeasures Character vector specifying whether each \code{y} variable
#' should be summarized by mean, median, or frequency. For example, if
#' \code{yvarnames} has length 3 and you wish to display frequencies for the
#' first variable, means for the second, and medians for the third, you would
#' set \code{ymeasures = c("freq", "mean", "median")}. If unspecified, function
#' displays frequencies for any factor variable or numeric variable with 5 or
#' fewer unique values, and means for numeric variables with more than five
#' levels.
#'
#' @param listwise.deletion Logical value for whether observations with missing
#' values for any \code{y} variable should be excluded entirely (as opposed to
#' using all available data for each comparison).
#'
#' @param ynames Character vector with labels for the \code{y} variables.
#'
#' @param ylevels Character vector or list of character vectors with labels for
#' the levels of each categorical \code{y} variable.
#'
#' @param quantiles Numeric value. If specified, function compares \code{y}
#' variables across quantiles of \code{x}. For example, if \code{x} contains BMI
#' values and \code{yvarnames} includes HDL and race, setting
#' \code{quantiles = 3} compares mean BMI and distribution of race across BMI
#' tertiles.
#'
#' @param cell Character string specifying what statistic should appear in cells
#' of the table for frequency comparisons. Choices are \code{"n"} for counts,
#' \code{"tot.percent"} for table percentage, \code{"col.percent"} for column
#' percentage, \code{"row.percent"} for row percentage, \code{"tot.prop"} for
#' table proportion, \code{"col.prop"} for column proportion, \code{"row.prop"}
#' for row proportion, \code{"n/totn"} for count/total counts, \code{"n/coln"}
#' for count/column count, and \code{"n/rown"} for count/row count.
#'
#' @param freq.parenth Character string specifying what statistic should appear
#' in parentheses after cell values for frequency comparisons. Choices are
#' \code{"none"}, \code{"se"} for standard error of requested percentage or
#' proportion, \code{"ci"} for 95\% confidence interval for requested percentage
#' or proportion, and \code{"tot.percent"}, \code{"col.percent"},
#' \code{"row.percent"}, \code{"tot.prop"}, \code{"col.prop"}, and
#' \code{"row.prop"} for various percentages and proportions.
#'
#' @param freq.text.label Character string with text to put after \code{y}
#' variable names for frequency comparisons, identifying what cell values and
#' parentheses represent.
#'
#' @param freq.tests Character string or vector specifying which test for
#' association between \code{x} and \code{y} should be used for each frequency
#' comparison. Choices are \code{"chi"} for Pearson's chi-squared test,
#' \code{"fisher"} for Fisher's exact test, \code{"z"} for z test without
#' continuity correction; and \code{"z.continuity"} for z test with continuity
#' correction. \code{"z"} and \code{"z.continuity"} can only be used if \code{x}
#' and \code{y} are binary.
#'
#' @param means.parenth Character string specifying what statistic should appear
#' in parentheses after the means in each cell for mean comparisons. Choices are
#' \code{"none"}, \code{"sd"} for standard deviation, \code{"se"} for standard
#' error, \code{"t.ci"} for 95\% confidence interval based on t distribution,
#' and \code{"z.ci"} for 95\% confidence interval based on z distribution.
#'
#' @param means.text.label Character string with text to put after \code{y}
#' variable names for mean comparisons, identifying what cell values and
#' parentheses represent.
#'
#' @param variance Character string specifying which version of the two-sample
#' t-test to use for mean comparisons, supposing \code{x} has two levels.
#' Choices are \code{"equal"} for equal variance t-test, \code{"unequal"} for
#' unequal variance t-test, and \code{"ftest"} for F test to determine which to
#' use.
#'
#' @param medians.parenth Character string specifying what values are shown in
#' parentheses after the medians in each cell for median comparisons. Choices
#' are \code{"none"}, \code{"iqr"}, \code{"range"}, \code{"minmax"},
#' \code{"q1q3"} for first and third quartiles, and \code{"ci"} for 95\%
#' confidence interval for the medians (based on binomial probabilities if one
#' or more groups have n less than 10, otherwise based on normal approximation
#' to binomial).
#'
#' @param medians.text.label Character string with text to put after the
#' \code{y} variable name for median comparisons, identifying what cell values
#' and parentheses represent.
#'
#' @param decimals Numeric value or vector specifying number of decimal places
#' for numbers other than p-values for each \code{y} variable.
#'
#' @param overall.column Logical value for whether to include a column showing
#' overall means/medians/frequencies for \code{y}.
#'
#' @param compress Logical value for whether binary \code{y} variables should
#' be displayed as a single row rather than two.
#'
#' @param bold.varnames Logic value for whether to use bold font for
#' \code{y} variable names in the first column. Only used if \code{latex = TRUE}.
#'
#' @param bold.varlevels Logical value for whether levels of categorical
#' \code{y} variables should appear in bold font in the first column. Only used
#' if \code{latex = TRUE}.
#'
#'
#' @return Character matrix with table comparing means/medians/frequencies of
#' \code{y} variables across levels of \code{x}.
#'
#'
#' @examples
#' # Load in sample dataset d
#' data(d)
#'
#' # Compare age, sex, race, and BMI in control vs. treatment group using
#' # listwise deletion
#' table1 <- tabmulti(dataset = d, xvarname = "Group",
#'                    yvarnames = c("Age", "Sex", "Race", "BMI"))
#'
#' # Repeat using pairwise deletion
#' table2 <- tabmulti(dataset = d, xvarname = "Group", n.column = TRUE,
#'                    n.headings = FALSE,
#'                    yvarnames = c("Age", "Sex", "Race", "BMI"),
#'                    listwise.deletion = FALSE)
#'
#'
#' # Same as table1, but compare medians rather than means for BMI
#' table3 <- tabmulti(dataset = d, xvarname = "Group",
#'                    yvarnames = c("Age", "Sex", "Race", "BMI"),
#'                    ymeasures = c("mean", "freq", "freq", "median"))
#'
#'
#' @export
tabmulti <- function(dataset, xvarname, yvarnames, ymeasures = NULL,
                     listwise.deletion = TRUE, latex = FALSE, xlevels = NULL,
                     ynames = yvarnames, ylevels = NULL, quantiles = NULL,
                     quantile.vals = FALSE, parenth.sep = "-", cell = "n",
                     freq.parenth = NULL, freq.text.label = NULL,
                     freq.tests = "chi", means.parenth = "sd",
                     means.text.label = NULL, variance = "unequal",
                     medians.parenth = "iqr", medians.text.label = NULL,
                     decimals = NULL, p.include = TRUE, p.decimals = c(2, 3),
                     p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
                     p.avoid1 = FALSE, overall.column = TRUE, n.column = FALSE,
                     n.headings = TRUE, compress = FALSE, bold.colnames = TRUE,
                     bold.varnames = FALSE, bold.varlevels = FALSE,
                     variable.colname = "Variable", print.html = FALSE,
                     html.filename = "table1.html") {

  # If any inputs are not correct class, return error
  if (! is.matrix(dataset) & ! is.data.frame(dataset)) {
    stop("For dataset input, please enter matrix or data frame with variables of interest")
  }
  if (! is.character(xvarname)) {
    stop("For xvarname input, please enter character string with name of column variable")
  }
  if (! all(is.character(yvarnames))) {
    stop("For yvarnames input, please enter character string or vector of character strings with name(s) of row variable(s)")
  }
  if (! is.null(ymeasures) && ! all(ymeasures %in% c("mean", "median", "freq"))) {
    stop("For ymeasures input, please enter character string or vector of character strings of same length as yvarnames")
  }
  if (! is.logical(listwise.deletion)) {
    stop("For listwise.deletion input, please enter TRUE or FALSE")
  }
  if (! is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! is.null(xlevels) && ! is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (! all(is.character(ynames))) {
    stop("For ynames input, please enter character string or vector of character strings of same length as yvarnames")
  }
  if (! is.null(ylevels) &&
      ! all(unlist(lapply(X = ylevels,
                          FUN = function(x) all(is.character(x)))))) {
    stop("For ylevels input, please enter vector or list of vectors of character strings")
  }
  if (! is.null(quantiles) &&
      ! (is.numeric(quantiles) & length(quantiles) == 1 && quantiles > 1 &
         quantiles == round(quantiles))) {
    stop("For quantiles input, please enter a whole number greater than 1")
  }
  if (! is.logical(quantile.vals)) {
    stop("For quantile.vals input, please enter TRUE or FALSE")
  }
  if (! is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string")
  }
  if (! cell %in% c("n", "percent", "tot.percent", "col.percent", "row.percent",
                    "tot.prop", "col.prop", "row.prop", "n/totn", "n/coln",
                    "n/rown")) {
    stop("For cell input, please enter 'n', 'tot.percent', 'col.percent', 'row.percent', 'tot.prop', 'col.prop', 'row.prop', 'n/totn', 'n/coln', or 'n/rown'")
  }
  if (! is.null(freq.parenth) &&
      ! freq.parenth %in% c("none", "se", "ci", "tot.percent", "col.percent",
                            "row.percent", "tot.prop", "col.prop",
                            "row.prop")) {
    stop("For freq.parenth input, please enter 'none', 'se', 'ci', 'tot.percent', 'col.percent', 'row.percent', 'tot.prop', 'col.prop', or 'row.prop'")
  }
  if (! is.null(freq.text.label) && ! is.character(freq.text.label)) {
    stop("For freq.text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (! all(freq.tests %in% c("chi", "fisher", "z", "z.continuity"))) {
    stop("For freq.tests input, please enter character string or vector of character strings indicating what statistical test should be performed for each categorical row variable. Each element should be 'chi', 'fisher', 'z', or 'z.continuity'")
  }
  if (! means.parenth %in% c("none", "sd", "se", "t.ci", "z.ci", "none")) {
    stop("For means.parenth input, please enter 'none', 'sd', 'se', 't.ci', or 'z.ci'")
  }
  if (! is.null(means.text.label) && !is.character(means.text.label)) {
    stop("For means.text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (! variance %in% c("equal", "unequal", "ftest")) {
    stop("For variance input, please enter 'equal', 'unequal', or 'ftest'")
  }
  if (! medians.parenth %in% c("none", "iqr", "range", "minmax", "q1q3")) {
    stop("For medians.parenth input, please enter 'none', 'iqr', 'range', 'minmax', or 'q1q3'")
  }
  if (! is.null(medians.text.label) && ! is.character(medians.text.label)) {
    stop("For medians.text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (! is.null(decimals) && ! all(is.numeric(decimals))) {
    stop("For decimals input, please enter numeric value or vector of numeric values indicating how many decimal places should be used in reporting statistics for each row variable")
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
  if (! is.logical(n.headings)) {
    stop("For n.headings input, please enter TRUE or FALSE")
  }
  if (! is.logical(compress)) {
    stop("For compress input, please enter TRUE or FALSE")
  }
  if (! is.logical(bold.colnames)) {
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

  # If listwise.deletion is TRUE, drop observations with missing values for
  # column variable or any row variables
  if (listwise.deletion){

    dataset <- dataset[which(!is.na(dataset[, xvarname])), ]
    for (ii in 1: length(yvarnames)) {
      dataset <- dataset[which(!is.na(dataset[, yvarnames[ii]])), ]
    }

  }

  # If ymeasures is NULL, guess what measures are appropriate based on each
  # variable
  if (is.null(ymeasures)) {
    ymeasures <- c()
    for (ii in 1:length(yvarnames)) {
      if (is.factor(dataset[, yvarnames[ii]]) |
          is.character(dataset[, yvarnames[ii]]) |
          length(unique(dataset[! is.na(dataset[, yvarnames[ii]]),
                                yvarnames[ii]])) <= 5) {
        ymeasures <- c(ymeasures, "freq")
      } else {
        ymeasures <- c(ymeasures, "mean")
      }
    }
  }

  # If ymeasures is single value, create vector of repeat values
  if (length(ymeasures) == 1) {
    ymeasures <- rep(ymeasures, length(yvarnames))
  }

  # If freq.tests is a single value, create vector of repeat values
  if (length(freq.tests) == 1) {
    freq.tests <- rep(freq.tests, sum(ymeasures == "freq"))
  }

  # If decimals is a single value, create vector of repeat values
  if (length(decimals) == 1) {
    decimals <- rep(decimals, length(ymeasures))
  }

  # If ylevels is a vector, convert to a list
  if (! is.null(ylevels) && ! is.list(ylevels)) {
    ylevels <- list(ylevels)
  }

  # Call tabmeans, tabmedians, or tabfreq repeatedly
  mediansindex <- 0
  meansindex <- 0
  freqindex <- 0
  for (ii in 1:length(yvarnames)) {
    if (ymeasures[ii] == "mean") {
      meansindex <- meansindex + 1
      current <- tabmeans(x = dataset[, xvarname], y = dataset[, yvarnames[ii]],
                          latex = latex, variance = variance, xlevels = xlevels,
                          yname = ynames[ii], quantiles = quantiles,
                          quantile.vals = quantile.vals,
                          parenth = means.parenth,
                          text.label = means.text.label, parenth.sep = parenth.sep,
                          decimals = decimals[ii], p.include = p.include,
                          p.decimals = p.decimals, p.cuts = p.cuts,
                          p.lowerbound = p.lowerbound, p.leading0 = p.leading0,
                          p.avoid1 = p.avoid1, overall.column = overall.column,
                          n.column = n.column, n.headings = n.headings,
                          bold.colnames = bold.colnames,
                          bold.varnames = bold.varnames,
                          variable.colname = variable.colname)
    } else if (ymeasures[ii] == "median") {
      mediansindex <- mediansindex + 1
      current <- tabmedians(x = dataset[, xvarname],
                            y = dataset[, yvarnames[ii]], latex = latex,
                            xlevels = xlevels, yname = ynames[ii],
                            quantiles = quantiles, quantile.vals = quantile.vals,
                            parenth = medians.parenth,
                            text.label = medians.text.label,
                            parenth.sep = parenth.sep, decimals = decimals[ii],
                            p.include = p.include, p.decimals = p.decimals,
                            p.cuts = p.cuts, p.lowerbound = p.lowerbound,
                            p.leading0 = p.leading0, p.avoid1 = p.avoid1,
                            overall.column = overall.column, n.column = n.column,
                            n.headings = n.headings,
                            bold.colnames = bold.colnames,
                            bold.varnames = bold.varnames,
                            variable.colname = variable.colname)
    } else if (ymeasures[ii] == "freq") {
      freqindex <- freqindex + 1
      current <- tabfreq(x = dataset[, xvarname], y = dataset[, yvarnames[ii]],
                         latex = latex, xlevels = xlevels, yname = ynames[ii],
                         ylevels = ylevels[[freqindex]], quantiles = quantiles,
                         quantile.vals = quantile.vals, cell = cell,
                         parenth = freq.parenth, text.label = freq.text.label,
                         parenth.sep = parenth.sep,
                         test = freq.tests[freqindex], decimals = decimals[ii],
                         p.include = p.include, p.decimals = p.decimals,
                         p.cuts = p.cuts, p.lowerbound = p.lowerbound,
                         p.leading0 = p.leading0, p.avoid1 = p.avoid1,
                         overall.column = overall.column, n.column = n.column,
                         n.headings = n.headings, compress = compress,
                         bold.colnames = bold.colnames,
                         bold.varnames = bold.varnames,
                         bold.varlevels = bold.varlevels,
                         variable.colname = variable.colname)
    }
    if (ii == 1) {
      results <- current
    } else {
      results <- rbind(results, current)
    }
  }
  rownames(results) <- NULL

  # Print html version of table if requested
  if (print.html) {

    results.xtable <-
      xtable(results,
             align = paste("ll",
                           paste(rep("r", ncol(results) - 1), collapse = ""),
                           sep = "", collapse = ""))
    print(results.xtable, include.rownames = FALSE, type = "html",
          file = html.filename, sanitize.text.function = function(x) {
            ifelse(substr(x, 1, 1) == " ", paste("&nbsp &nbsp", x), x)
          })

  }

  # Return results matrix
  return(results)

}
