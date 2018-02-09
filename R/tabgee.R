#' Generate Summary Table for Fitted Generalized Estimating Equation (GEE) Model
#'
#'
#' Creates table summarizing a GEE model fit using the \code{\link[gee]{gee}}
#' function [1].
#'
#' Interaction and polynomial terms are compatible with \code{tabgee}. If
#' interaction terms are included, the table will be formatted a little
#' differently. Basically including an interaction is equivalent to setting
#' \code{basic.form = TRUE}. All variable names and levels will be exactly as
#' they appear when you run \code{summary(geefit)}, where \code{geefit} is the
#' object returned from a call to \code{\link[gee]{gee}}.
#'
#'
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#' @inheritParams tabglm
#'
#'
#' @param geefit Object returned from \code{\link[gee]{gee}}.
#'
#' @param n.id Logical value for whether to include a column for the number of
#' unique IDs (i.e. clusters).
#'
#' @param n.total Logical value for whether to include a column for the total
#' number of observations used.
#'
#' @param or Logical value for whether to include columns for odds ratios and
#' associated Wald 95\% confidence intervals. Only used for logistic regression.
#'
#' @param robust Logical value for whether robust standard errors should be used
#' (i.e. from sandwich estimator).
#'
#' @param data Data frame containing variables passed to \code{\link[gee]{gee}}
#' to create \code{geefit}. Only needs to be specified when one or more of the
#' predictors is a factor variable and \code{basic.form = FALSE}.
#'
#'
#' @return Character matrix with table summarizing the fitted GEE.
#'
#'
#' @examples
#' # Load in sample dataset d and convert to long format
#' data(d)
#' d2 <- reshape(data = d, varying = c("bp.1", "bp.2", "bp.3", "highbp.1",
#'                                     "highbp.2", "highbp.3"),
#'               timevar = "bp.visit", direction = "long")
#' d2 <- d2[order(d2$id), ]
#'
#' # Create labels for race levels
#' races <- c("White", "Black", "Mexican American", "Other")
#'
#' # Test whether predictors are associated with blood pressure at 1, 2, and 3
#' # months
#' geefit1 <- gee(bp ~ Age + Sex + Race + BMI + Group, id = id, data = d2,
#'                corstr = "unstructured")
#'
#' # Create summary table using tabgee
#' geetable1 <- tabgee(geefit = geefit1, data = d2, n.id = TRUE, n.total = TRUE,
#'                     xlabels = c("Intercept", "Age", "Male", "Race", races,
#'                                 "BMI", "Treatment"))
#'
#' # Test whether predictors are associated with high blood pressure at 1, 2,
#' and 3 months
#' geefit2 <- gee(highbp ~ Age + Sex + Race + BMI + Group, id = id, data = d2,
#'                family = binomial, corstr = "unstructured")
#'
#' # Create summary table using tabgee
#' geetable2 <- tabgee(geefit = geefit2, data = d2, ci.beta = FALSE,
#'                     xlabels = c("Intercept", "Age", "Male", "Race", races,
#'                                 "BMI", "Treatment"))
#'
#'
#' @references
#' 1. Carey, V.J. (2015). gee: Generalized Estimation Equation Solver. R package
#' version 4.13-19. Ported to R by Thomas Lumley and Brian Ripley.
#' \url{https://cran.r-project.org/package=gee}.
#'
#' 2. Dahl, D.B. (2016). xtable: Export Tables to LaTeX or HTML. R package
#' version 1.8-2, \url{https://cran.r-project.org/package=xtable}.
#'
#' 3. Ushley, K. (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
#'\url{https://CRAN.R-project.org/package=Kmisc}.
#'
#'
#' @export
tabgee <- function(geefit, latex = FALSE, xlabels = NULL, ci.beta = TRUE,
                   decimals = 2, p.decimals = c(2, 3), p.cuts = 0.01,
                   p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
                   basic.form = FALSE, intercept = TRUE, n.id = FALSE,
                   n.total = FALSE, or = TRUE, robust = TRUE, data = NULL,
                   greek.beta = FALSE, binary.compress = TRUE,
                   bold.colnames = TRUE, bold.varnames = FALSE,
                   bold.varlevels = FALSE, variable.colname = "Variable",
                   print.html = FALSE, html.filename = "table1.html") {

  # If any inputs are not correct class, return error
  if (! all(class(geefit) == c("gee", "glm"))) {
    stop("For geefit input, please enter an object returned from the gee function")
  }
  if (! is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! is.logical(ci.beta)) {
    stop("For ci.beta input, please enter TRUE or FALSE")
  }
  if (! is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
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
  if (! is.logical(basic.form)) {
    stop("For basic.form input, please enter TRUE or FALSE")
  }
  if (! is.logical(intercept)) {
    stop("For intercept input, please enter TRUE or FALSE")
  }
  if (! is.logical(n.id)) {
    stop("For n.id input, please enter TRUE or FALSE")
  }
  if (! is.logical(n.total)) {
    stop("For n.total input, please enter TRUE or FALSE")
  }
  if (! is.logical(or)) {
    stop("For or input, please enter TRUE or FALSE")
  }
  if (! is.logical(robust)) {
    stop("For robust input, please enter TRUE or FALSE")
  }
  if (! is.null(data)) {
    if (!(is.data.frame(data) | is.matrix(data))) {
      stop("For data input, please enter data frame or matrix.")
    }
  }
  if (! is.logical(greek.beta)) {
    stop("For greek.beta input, please enter TRUE or FALSE")
  }
  if (! is.logical(binary.compress)) {
    stop("For binary.compress input, please enter TRUE or FALSE")
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

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Store gee.fit results
  coef <- summary(geefit)$coefficients
  model <- attr(geefit$terms, "dataClasses")
  xvars <- c()
  xnames <- c()
  variable.classes <- c()
  for (ii in 2: (length(model) - 1)) {
    class.split <- unlist(strsplit(model[ii], "[.]"))
    if (class.split[1] == "nmatrix") {
      name.ii <- substring(strsplit(names(class.split)[2], ",")[[1]][1], 6)
      order.ii <- as.numeric(class.split[2])
      variable.classes <- c(variable.classes, rep("numeric", order.ii))
      xvars <- c(xvars, name.ii)
      xnames <- c(xnames, name.ii)
      if (order.ii >= 2) {
        xnames <- c(xnames, paste(name.ii, "squared"))
      }
      if (order.ii >= 3) {
        xnames <- c(xnames, paste(name.ii, "cubed"))
      }
      if (order.ii >= 4) {
        xnames <- c(xnames, paste(name.ii, "^", 4: order.ii))
      }
    } else {
      variable.classes <- c(variable.classes, model[ii])
      xvars <- c(xvars, names(model)[ii])
      xnames <- c(xnames, names(model)[ii])
    }
  }

  # Get subset of data with no missing values
  if (!is.null(data)) {
    data <- data[, xvars]
    data <- data[complete.cases(data), ]
  }

  # Column for SE's and Z's based on robust input
  secol <- ifelse(! robust, 2, 4)
  zcol <- ifelse(! robust, 3, 5)

  # Initialized vectors for formatting factor variables in table
  spaces <- c()
  refs <- c()

  # Get indices of variable names
  predcounter <- 0
  pred <- c()
  for (ii in 2: (length(model) - 1)) {
    pred[ii - 1] <- predcounter + 1
    if (substr(model[ii], 1, 7) == "nmatrix") {
      predcounter <- predcounter +
        as.numeric(strsplit(model[ii], "[.]")[[1]][2])
    } else if (! model[ii] == "factor" |
               (model[ii] == "factor" &
                length(unique(data[, names(model)[ii]])) == 2 &
                binary.compress)) {
      predcounter <- predcounter + 1
    } else {
      predcounter <- predcounter + length(unique(data[, names(model)[ii]]))
    }
  }

  # Initialize table
  tbl <- matrix("", nrow = 100, ncol = 8)
  tbl[1, 2] <- length(unique(geefit$id))
  tbl[1, 3] <- sprintf("%0.0f", geefit$nobs)

  # Create index variables for table and gee coefficients
  tabindex <- 1
  coefindex <- 1

  # Enter intercept information if available
  if (rownames(coef)[1] == "(Intercept)" & intercept) {
    beta <- coef[1, 1]
    se <- coef[1, secol]
    z <- coef[1, zcol]
    p <- pnorm(-abs(z)) * 2
    tbl[1, 1] <- "Intercept"
    tbl[1, 4] <- paste(sprintf(spf, coef[1, 1]), " (", sprintf(spf, se), ")",
                       sep = "")
    tbl[1, 5] <- paste("(", sprintf(spf, beta - 1.96 * se), ", ",
                       sprintf(spf, beta + 1.96 * se), ")", sep = "")
    tbl[1, 6: 7] <- "-"
    tbl[1, 8] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                         lowerbound = p.lowerbound, leading0 = p.leading0,
                         avoid1 = p.avoid1)
    tabindex <- tabindex + 1
    coefindex <- coefindex + 1

  } else {
    coefindex <- coefindex + 1
  }

  # If there are one or more interaction terms OR basic.form is TRUE, just do
  # basic formatting straight from the table of coefficients
  if ((":" %in% unlist(strsplit(rownames(coef), ""))) | basic.form |
      (sum(model == "factor") > 0 & is.null(data))) {
    beta <- coef[2:nrow(coef), 1]
    se <- coef[2:nrow(coef), secol]
    or <- exp(beta)
    z <- coef[tabindex: nrow(coef), zcol]
    p <- pnorm(-abs(z)) * 2
    tbl[2: nrow(coef), 1] <- rownames(coef)[-1]
    tbl[2: nrow(coef), 4] <- paste(sprintf(spf, beta), " (", sprintf(spf, se),
                                  ")", sep = "")
    tbl[2: nrow(coef), 5] <- paste("(", sprintf(spf, beta - 1.96 * se), ", ",
                                  sprintf(spf, beta + 1.96 * se), ")", sep = "")
    tbl[2: nrow(coef), 6] <- sprintf(spf, exp(beta))
    tbl[2: nrow(coef), 7] <-
      paste("(", sprintf(spf, exp(beta - 1.96 * se)), ", ",
            sprintf(spf, exp(beta + 1.96 * se)), ")", sep = "")
    tbl[tabindex: nrow(coef), 8] <-
      formatp(p = p, cuts = p.cuts, decimals = p.decimals,
              lowerbound = p.lowerbound, leading0 = p.leading0,
              avoid1 = p.avoid1)
    tabindex <- nrow(coef) + 1
  } else {

    # Otherwise format factors neatly
    for (ii in 1: length(xnames)) {

      if (variable.classes[ii] != "factor" ||
          (variable.classes[ii] == "factor" &
           length(unique(data[, xnames[ii]])) == 2 & binary.compress))  {
        beta <- coef[coefindex, 1]
        se <- coef[coefindex, secol]
        or <- exp(beta)
        z <- coef[coefindex, zcol]
        p <- pnorm(-abs(z)) * 2
        tbl[tabindex, 1] <- xnames[ii]
        tbl[tabindex, 4] <-
          paste(sprintf(spf, beta), " (", sprintf(spf, se), ")", sep = "")
        tbl[tabindex, 5] <- paste("(", sprintf(spf, beta - 1.96 * se), ", ",
                                  sprintf(spf, beta + 1.96 * se), ")", sep = "")
        tbl[tabindex, 6] <- sprintf(spf, exp(beta))
        tbl[tabindex, 7] <-
          paste("(", sprintf(spf, exp(beta - 1.96 * se)), ", ",
                sprintf(spf, exp(beta + 1.96 * se)), ")", sep = "")
        tbl[tabindex, 8] <-
          formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                  lowerbound = p.lowerbound, leading0 = p.leading0,
                  avoid1 = p.avoid1)
        tabindex <- tabindex + 1
        coefindex <- coefindex + 1

      } else {
        levels <- sort(unique(data[, xnames[ii]]))
        if (length(levels) == 2 & binary.compress) {
          beta <- coef[coefindex, 1]
          se <- coef[coefindex, secol]
          or <- exp(beta)
          z <- coef[coefindex, zcol]
          p <- pnorm(-abs(z)) * 2
          tbl[tabindex, 1] <- xnames[coefindex]
          tbl[tabindex, 4] <-
            paste(sprintf(spf, beta), " (", sprintf(spf, se), ")", sep = "")
          tbl[tabindex, 5] <-
            paste("(", sprintf(spf, beta - 1.96 * se), ", ",
                  sprintf(spf, beta + 1.96 * se), ")", sep = "")
          tbl[tabindex, 6] <- sprintf(spf, exp(beta))
          tbl[tabindex, 7] <-
            paste("(", sprintf(spf, exp(beta - 1.96 * se)), ", ",
                  sprintf(spf, exp(beta + 1.96 * se)), ")", sep = "")
          tbl[tabindex, 8] <-
            formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                    lowerbound = p.lowerbound, leading0 = p.leading0,
                    avoid1 = p.avoid1)
          tabindex <- tabindex + 1
          coefindex <- coefindex + 1
        } else {
          tbl[tabindex, 1] <- xnames[ii]
          tabindex <- tabindex + 1
          tbl[tabindex, 1] <- paste("  ", levels[1], " (ref)", sep = "")
          tbl[tabindex, 4: 8] <- "-"
          spaces <- c(spaces, tabindex)
          refs <- c(refs, tabindex)
          tabindex <- tabindex + 1

          for (jj in 2: length(levels)) {
            tbl[tabindex, 1] <- paste("  ", levels[jj], sep = "")
            beta <- coef[coefindex, 1]
            se <- coef[coefindex, secol]
            or <- exp(beta)
            z <- coef[coefindex, zcol]
            p <- pnorm(-abs(z)) * 2
            tbl[tabindex, 4] <-
              paste(sprintf(spf, beta), " (", sprintf(spf, se), ")", sep = "")
            tbl[tabindex, 5] <-
              paste("(", sprintf(spf, beta - 1.96 * se), ", ",
                    sprintf(spf, beta + 1.96 * se), ")", sep = "")
            tbl[tabindex, 6] <- sprintf(spf, exp(beta))
            tbl[tabindex, 7] <-
              paste("(", sprintf(spf, exp(beta - 1.96 * se)), ", ",
                    sprintf(spf, exp(beta + 1.96 * se)), ")", sep = "")
            tbl[tabindex, 8] <-
              formatp(p = p, cuts = p.cuts, decimals = p.decimals,
                      lowerbound = p.lowerbound, leading0 = p.leading0,
                      avoid1 = p.avoid1)
            spaces <- c(spaces, tabindex)
            tabindex <- tabindex + 1
            coefindex <- coefindex + 1
          }
        }
      }
    }
  }

  # Truncate table at correct number of rows
  tbl <- tbl[1: (tabindex - 1),, drop = FALSE]

  # Add column names
  colnames(tbl) <- c(variable.colname, "Clusters", "Observations", "Beta (SE)",
                     "95% CI for Beta", "OR", "95% CI for OR", "P")

  # Remove n columns if requested
  if (! n.id) {
    tbl <- tbl[, -which(colnames(tbl) == "Clusters"), drop = FALSE]
  }
  if (! n.total) {
    tbl <- tbl[, -which(colnames(tbl) == "Observations"), drop = FALSE]
  }

  # Remove CI column if requested
  if (! ci.beta) {
    tbl <- tbl[, -which(colnames(tbl) == "95% CI for Beta"), drop = FALSE]
  }

  # Adjust OR columns if necessary
  if (geefit$family$link == "log") {
    colnames(tbl)[colnames(tbl) == "OR"] <- "exp(Beta)"
    colnames(tbl)[colnames(tbl) == "95% CI for OR"] <- "95% CI for exp(Beta)"
  } else if (! (geefit$family$link == "logit" & geefit$family$family %in%
                c("binomial", "quasi", "quasibibinomial"))) {
    tbl <-
      tbl[, -which(colnames(tbl) %in% c("OR", "95% CI for OR")), drop = FALSE]
  }

  # Add variable labels if possible
  if (! is.null(xlabels)) {
    xlabels[spaces] <- paste("  ", xlabels[spaces], sep = "")
    xlabels[refs] <- paste(xlabels[refs], "(ref)")
    tbl[1: nrow(tbl), 1] <- xlabels
  }

  # If latex is TRUE, do some re-formatting
  if (latex) {
    if (greek.beta) {
      colnames(tbl)[which(colnames(tbl) == "Beta (SE)")] <-
        "$\\hat{\\beta}$ (SE)"
      colnames(tbl)[which(colnames(tbl) == "95% CI for Beta")] <-
        "95% CI for $\\beta$"
      colnames(tbl)[which(colnames(tbl) == "exp(Beta)")] <- "exp($\\beta)$"
      colnames(tbl)[which(colnames(tbl) == "95% CI for exp(Beta)")] <-
        "95\\% CI for exp($\\beta$)"
    }
    plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
    }
    spacelocs <- which(substr(tbl[, variable.colname], 1, 2) == "  ")
    if (length(spacelocs) > 0) {
      tbl[spacelocs, variable.colname] <-
        paste("$\\hskip .4cm$",
              substring(tbl[spacelocs, variable.colname], 3), sep = "")
    }
    chars <- strsplit(colnames(tbl), "")
    for (ii in 1: length(chars)) {
      percentlocs <- which(chars[[ii]] == "%")
      if (length(percentlocs) > 0) {
        chars[[ii]][percentlocs] <- "\\%"
      }
    }
    colnames(tbl) <- sapply(chars, function(x)
      paste(x, sep = "", collapse = ""))
    if (bold.colnames == TRUE) {
      colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
    }
    if (bold.varnames == TRUE) {
      tbl[pred, 1] <- paste("$\\textbf{", tbl[pred, 1], "}$")
    }
    if (bold.varlevels == TRUE) {
      tbl[c(1: nrow(tbl))[! c(1:nrow(tbl)) %in% pred], 1] <-
        paste("$\\textbf{", tbl[c(1: nrow(tbl))[! c(1:nrow(tbl)) %in% pred], 1],
              "}$", sep = "")
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

  # Return tbl
  return(tbl)

}
