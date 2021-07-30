#' Create Table Comparing Group Means (for Complex Survey Data)
#'
#' Creates a table comparing the mean of \code{y} across levels of \code{x}.
#'
#' Basically \code{\link{tabmeans}} for complex survey data. Relies heavily on
#' the \pkg{survey} package.
#'
#'
#' @param formula Formula, e.g. \code{BMI ~ Sex}.
#' @param design Survey design object from \code{\link[survey]{svydesign}}.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"n"} for total sample size, \code{"overall"} for
#' overall mean, \code{"xgroups"} for \code{x} group means, \code{"diff"} for
#' difference in \code{x} group means (this one and the next two are only
#' available for binary \code{x}), \code{"diffci"} for 95% CI for difference in
#' \code{x} group means, \code{"diff.ci"} for difference in group means and 95%
#' confidence interval, and \code{"p"} for p-value.
#' @param parenth Character string specifying what statistic to display in
#' parentheses after the means. Choices are \code{"none"}, \code{"sd"},
#' \code{"se"},  \code{"t.ci"}, \code{"z.ci"}, \code{"range"}, and
#' \code{"minmax"}.
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param xlevels Character vector with labels for the levels of \code{x}, used
#' in column headings.
#' @param yname Character string with a label for the \code{y} variable.
#' @param text.label Character string with text to put after the \code{y}
#' variable name, identifying what cell values and parentheses represent
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param anova.svyglm.list List of arguments to pass to
#' \code{\link[survey]{anova.svyglm}}. Only used if \code{x} has three or more
#' levels.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#' @param n.headings Logical value for whether to display group sample sizes in
#' parentheses in column headings.
#' @param N.headings Logical value for whether to display weighted sample sizes
#' in parentheses in column headings.
#' @param kable Logical value for whether to return a
#' \code{\link[knitr]{kable}}.
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
#' # Compare mean BMI by sex
#' (meanstable <- tabmeans.svy(BMI ~ Sex, design = design))
#'
#'
#' @export
tabmeans.svy <- function(formula,
                         design,
                         columns = c("xgroups", "p"),
                         parenth = "sd",
                         sep.char = ", ",
                         xlevels = NULL,
                         yname = NULL,
                         text.label = NULL,
                         decimals = 1,
                         anova.svyglm.list = NULL,
                         formatp.list = NULL,
                         n.headings = FALSE,
                         N.headings = FALSE,
                         kable = TRUE) {

  # Error checking
  if (class(formula) != "formula") {
    stop("The input 'formula' must be a formula.")
  }
  if (! "survey.design" %in% class(design)) {
    stop("The input 'design' must be a survey design object.")
  }
  if (! all(columns %in% c("n", "overall", "xgroups", "diff", "diffci",
                           "diff.ci", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'n', 'overall', 'xgroups', 'diff', 'diffci', 'diff.ci', 'p'.")
  }
  if (! parenth %in% c("none", "sd", "se", "t.ci", "z.ci", "range", "minmax")) {
    stop("The input 'parenth' must be one of the following: 'none', 'sd', 'se', 't.ci', 'z.ci', 'range', 'minmax'.")
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
  if (! is.null(text.label) && ! is.character(text.label)) {
    stop("The input 'text.label' must be a character string.")
  }
  if (! is.null(decimals) && ! (is.numeric(decimals) && decimals >= 0 &&
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
  if (! is.logical(N.headings)) {
    stop("The input 'N.headings' must be a logical.")
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
  svyby.svymean <- svyby(as.formula(paste("~", yvarname, sep = "")),
                         by = as.formula(paste("~", xvarname, sep = "")),
                         design = design, FUN = svymean)
  means <- svyby.svymean[[yvarname]]
  ses <- svyby.svymean$se
  ns <- table(x)

  xvals <- names(ns)
  num.groups <- length(xvals)

  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) xlevels <- xvals

  # If decimals is unspecified, set to a reasonable value
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
  spf <- paste("%0.", decimals, "f", sep = "")

  # Hypothesis test
  if (num.groups == 2) {
    fit <- svyttest(formula, design = design)
    diffmeans <- -fit$estimate
    diffmeans.ci <- -rev(as.numeric(fit$conf.int))
    p <- fit$p.value
  } else {
    fit1 <- svyglm(as.formula(paste0(yvarname, "~ 1")), design = design)
    fit2 <- svyglm(formula, design = design)
    fit <- do.call(anova, c(list(object = fit1, object2 = fit2), anova.svyglm.list))
    p <- as.numeric(fit$p)
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
  df <- data.frame(Variable = paste(yname, text.label, sep = ""))

  # Loop through and add columns requested
  for (column in columns) {

    if (column == "n") {

      df$N <- sum(ns)

    } else if (column == "overall") {

      svymean.overall <- svymean(as.formula(paste("~", yvarname, sep = "")),
                                 design = design)
      mean.y <- svymean.overall[[yvarname]]
      se.y <- sqrt(attr(svymean.overall, "var"))

      if (parenth == "none") {
        df$Overall <- sprintf(spf, mean.y)
      } else if (parenth == "sd") {
        sd.y <- sqrt(svyvar(as.formula(paste("~", yvarname, sep = "")),
                            design = design)[[yvarname]])
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, sd.y), ")", sep = "")
      } else if (parenth == "se") {
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, se.y), ")", sep = "")
      } else if (parenth == "t.ci") {
        ttest.overall <- svyttest(as.formula(paste(yvarname, "~ 1", sep = "")), design = design)
        tcrit <- qt(p = 0.975, df = ttest.overall$parameter)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, mean.y - tcrit * se.y), sep.char,
                            sprintf(spf, mean.y + tcrit * se.y), ")", sep = "")
      } else if (parenth == "z.ci") {
        zcrit <- qnorm(p = 0.975)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf, mean.y - zcrit * se.y), sep.char,
                            sprintf(spf, mean.y + zcrit * se.y), ")", sep = "")
      } else if (parenth == "range") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf.r, diff(range(y))), ")", sep = "")
      } else if (parenth == "minmax") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        df$Overall <- paste(sprintf(spf, mean.y), " (",
                            sprintf(spf.r, min(y)), sep.char,
                            sprintf(spf.r, max(y)), ")", sep = "")
      }

    } else if (column == "xgroups") {

      if (parenth == "none") {
        newcols <- paste(sprintf(spf, means))
      } else if (parenth == "sd") {
        sds <- sqrt(svyby(as.formula(paste("~", yvarname, sep = "")),
                          by = as.formula(paste("~", xvarname, sep = "")),
                          design = design, FUN = svyvar)[[yvarname]])
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, sds), ")", sep = "")
      } else if (parenth == "se") {
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, ses), ")", sep = "")
      } else if (parenth == "t.ci") {
        ttests <- lapply(X = xvals, FUN = function(x) {
          svyttest(as.formula(paste(yvarname, "~ 1", sep = "")),
                   design = subset(design, design$variables[, xvarname] == x))
        })
        # ttests <- lapply(X = xvals, FUN = function(x) {
        #   svyttest(as.formula(paste(yvarname, "~ 1", sep = "")),
        #            design = eval(parse(text = paste("subset(design, ", xvarname, " == '", x, "')", sep = ""))))
        # })
        # ttests <- lapply(X = xvals, FUN = function(x) {
        #   svyttest(as.formula(paste(yvarname, "~ 1", sep = "")),
        #            design = eval(str2expression(paste("subset(design, ", xvarname, " == '", x, "')", sep = ""))))
        # })
        tcrits <- qt(p = 0.975, df = sapply(ttests, function(x) x$parameter))
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, means - tcrits * ses), sep.char,
                         sprintf(spf, means + tcrits * ses), ")", sep = "")
      } else if (parenth == "z.ci") {
        zcrit <- qnorm(p = 0.975)
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf, means - zcrit * ses), sep.char,
                         sprintf(spf, means + zcrit * ses), ")", sep = "")
      } else if (parenth == "range") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        ranges <- tapply(X = y, INDEX = x, FUN = function(x) diff(range(x)))
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf.r, ranges), ")", sep = "")
      } else if (parenth == "minmax") {
        spf.r <- ifelse(all(round(y, 0) == y), "%0.0f", spf)
        mins <- tapply(X = y, INDEX = x, FUN = min)
        maxes <- tapply(X = y, INDEX = x, FUN = max)
        newcols <- paste(sprintf(spf, means), " (",
                         sprintf(spf.r, mins), sep.char,
                         sprintf(spf.r, maxes), ")", sep = "")
      }
      names(newcols) <- xlevels
      df <- cbind(df, do.call(rbind, list(newcols)))

    } else if (column == "diff") {

      df$`Diff.` <- sprintf(spf, diffmeans)

    } else if (column == "diffci") {

      df$`95% CI for Diff.` <- paste(sprintf(spf, diffmeans.ci[1]), sep.char,
                                     sprintf(spf, diffmeans.ci[2]), sep = "")

    } else if (column == "diff.ci") {

      df$`Diff. (95% CI)` <- paste(sprintf(spf, diffmeans), " (",
                                   sprintf(spf, diffmeans.ci[1]), sep.char,
                                   sprintf(spf, diffmeans.ci[2]), ")", sep = "")

    } else if (column == "p") {

      df$P <- do.call(formatp, c(list(p = p), formatp.list))

    }

  }

  # Add sample sizes to column headings if requested
  if (n.headings) {
    names(df)[names(df) == "Overall"] <- paste("Overall (n = ", sum(ns), ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (n = ", ns, ")", sep = "")
  }
  if (N.headings) {
    Ns <- svytable(as.formula(paste("~", xvarname, sep = "")), design = design)
    names(df)[names(df) == "Overall"] <- paste("Overall (N = ", sum(Ns), ")", sep = "")
    names(df)[names(df) %in% xlevels] <- paste(xlevels, " (N = ", Ns, ")", sep = "")
  }

  # Return table
  if (! kable) return(df)
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
