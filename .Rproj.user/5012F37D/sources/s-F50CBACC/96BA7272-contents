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
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"beta"}, \code{"se"}, \code{"beta.se"},
#' \code{"beta.betaci"}, \code{"betaci"}, \code{"or"}, \code{"or.orci"},
#' \code{"orci"}, \code{"z"}, and \code{"p"}.
#'
#' @param robust Logical value for whether 'robust' standard errors should be
#' used for inference.
#'
#' @param data Data frame containing variables passed to \code{\link[gee]{gee}}
#' to create \code{geefit}. Only needs to be specified when one or more of the
#' predictors is a factor variable and \code{compres.factors = FALSE}.
#'
#'
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
#' # GEE: Blood pressure at 1, 2, and 3 months vs. predictors
#' geefit1 <- gee(bp ~ Age + Sex + Race + BMI + Group, id = id, data = d2,
#'                corstr = "unstructured")
#' (geetable1 <- tabgee(geefit = geefit1, data = d2))
#'
#' # GEE: High blood pressure at 1, 2, and 3 months vs. predictors. Display
#' # factors in "compressed" format
#' geefit2 <- gee(highbp ~ Age + Sex + Race + BMI + Group, id = id, data = d2,
#'                family = binomial, corstr = "unstructured")
#' (geetable2 <- tabgee(geefit = geefit2, data = d2, compress.factors = TRUE))
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
#'@export
tabgee <- function(geefit, columns = NULL, robust = TRUE, xlabels = NULL,
                   compress.factors = FALSE, data = NULL, ci.sep = ", ",
                   format.xtable = FALSE, decimals = 2, p.decimals = c(2, 3),
                   p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
                   p.avoid1 = FALSE, greek.beta = FALSE, bold.colnames = TRUE,
                   variable.colname = "Variable", print.html = FALSE,
                   html.filename = "table1.html") {

  # Extract info from geefit
  summary.geefit <- summary(geefit)
  coefmat <- summary.geefit$coefficients
  rownames.coefmat <- rownames(coefmat)
  betas <- coefmat[, "Estimate"]
  if (robust) {
    ses <- coefmat[, "Robust S.E."]
    zs <- coefmat[, "Robust z"]
  } else {
    ses <- coefmat[, "Naive S.E."]
    zs <- coefmat[, "Naive z"]
  }
  ps <- pnorm(-abs(zs)) * 2
  intercept <- rownames.coefmat[1] == "(Intercept)"
  gee.fam <- geefit$family$family
  gee.link <- geefit$family$link

  # Default columns to include depending on family of GEE
  if (is.null(columns)) {
    if (gee.fam == "binomial" & gee.link == "logit") {
      columns <- c("beta.se", "or.orci", "p")
    } else {
      columns <- c("beta.se", "p")
    }
  }

  # Determine whether xlabels has to be created
  create.xlabels <- is.null(xlabels)

  # Choose version of beta and leading spaces to use
  space.char <- ifelse(format.xtable, "$\\hskip .4cm$", "  ")
  beta.char <- ifelse(format.xtable & greek.beta, "$\\hat{\\beta}$", "Beta")

  # Determine whether there are factors, interactions, or polynomials
  classes <- attr(geefit$terms, "dataClasses")
  x.factors <- names(classes)[classes == "factor"]
  factors <- length(x.factors) > 0

  interactions <- length(grep(pattern = ":", x = rownames.coefmat)) > 1

  polynomial.rows <- grep(pattern = "poly(", x = rownames.coefmat, fixed = TRUE)
  polynomials <- length(polynomial.rows) > 0

  # If necessary, force compress.factors to be TRUE and notify user for reason
  if (interactions & ! compress.factors) {
    message("The 'compress.factors' input is being switched to TRUE because the fitted GEE includes interaction terms. This limitation may be addressed in future versions of 'tabgee'.")
    compress.factors <- TRUE
  } else if (compress.factors & is.null(data) & create.xlabels) {
    message("The compress.factors input is being switched to TRUE because reference group labels are not available. If you really want compress.factors = FALSE, you'll need to re-run with 'data' or 'xlabels' specified.")
    compress.factors <- TRUE
  }

  # Determine row indices for table entries and create xlabels vector
  if (compress.factors) {

    # Compressed factor formatting

    # All rows of table have entries
    ref.rows <- c()
    entry.rows <- 1: nrow(coefmat)

    # Create xlabels
    if (create.xlabels) {

      # Start with row names
      if (intercept) {
        xlabels <- c("Intercept", rownames.coefmat[-1])
      } else {
        xlabels <- rownames.coefmat
      }

      # Clean up polynomials
      if (polynomials) {

        for (ii in polynomial.rows) {
          xlabel.parts <- unlist(strsplit(x = xlabels[ii], split = ","))
          varname.ii <- substring(xlabel.parts[1], first = 6)
          poly.order <- as.numeric(substring(xlabel.parts[3],
                                             first = nchar(xlabel.parts[3])))
          if (poly.order == 1) {
            xlabels[ii] <- varname.ii
          } else if (poly.order == 2) {
            xlabels[ii] <- paste(varname.ii, "squared")
          } else if (poly.order == 3) {
            xlabels[ii] <- paste(varname.ii, "cubed")
          } else {
            xlabels[ii] <- paste(varname.ii, poly.order, sep = "^")
          }
        }

      }

      # Clean up factors (imperfect solution - could go wrong!)
      if (factors | interactions) {

        for (ii in 1: length(x.factors)) {
          xlabels <- unlist(lapply(xlabels, function(x)
            gsub(pattern = x.factors[ii], replacement = "", x = x)))
        }

      }

    }

  } else {

    # Expanded factor formatting
    rowcount <- ifelse(intercept, 1, 0)
    entry.rows <- rowcount
    ref.rows <- c()
    if (create.xlabels & intercept) {
      xlabels <- "Intercept"
    }
    for (ii in 1:
         length(unlist(strsplit(x = deparse(geefit$call, width.cutoff = 500),
                                split = "+", fixed = TRUE)))) {

      rowcount <- rowcount + 1
      varname.ii <- deparse(geefit$terms[ii][[3]])
      if (substr(varname.ii, 1, 4) == "poly") {

        # Clean up polynomial
        varname.ii.split <-
          unlist(strsplit(substring(varname.ii, first = 6), split = ","))
        varname.ii <- varname.ii.split[1]
        poly.order <- as.numeric(varname.ii.split[2])
        if (create.xlabels) {
          if (poly.order == 1) {
            xlabels[rowcount] <- varname.ii
          } else if (poly.order == 2) {
            xlabels[rowcount: (rowcount + 1)] <-
              c(varname.ii, paste(varname.ii, "squared"))
          } else if (poly.order == 3) {
            xlabels[rowcount: (rowcount + 2)] <-
              c(varname.ii, paste(varname.ii, c("squared", "cubed")))
          } else {
            xlabels[rowcount: (rowcount + poly.order - 1)] <-
              c(varname.ii, paste(varname.ii, 2: poly.order, sep = "^"))
          }
        }
        entry.rows <- c(entry.rows, rowcount: (rowcount + poly.order - 1))
        rowcount <- rowcount + poly.order - 1

      } else if (varname.ii %in% x.factors) {

        # Clean up factor
        locs <- which(substr(rownames.coefmat, start = 1,
                             stop = nchar(varname.ii)) == varname.ii)
        levels.ii <- levels(data[[varname.ii]])
        nlevels.ii <- length(levels.ii)
        new.entries <- (rowcount + 1): (rowcount + nlevels.ii)
        if (create.xlabels) {
          xlabels[rowcount] <- varname.ii
          xlabels[new.entries] <- levels.ii
        }
        xlabels[new.entries] <-
          paste(space.char, xlabels[new.entries], sep = "")
        xlabels[rowcount + 1] <- paste(xlabels[rowcount + 1], "(ref)")
        entry.rows <- c(entry.rows, (rowcount + 2): (rowcount + nlevels.ii))
        ref.rows <- c(ref.rows, rowcount + 1)
        rowcount <- rowcount + nlevels.ii

      } else {

        entry.rows <- c(entry.rows, rowcount)
        if (create.xlabels) {
          xlabels[rowcount] <- varname.ii
        }

      }
    }
  }

  # Initialize table
  tbl <- matrix(xlabels, ncol = 1, dimnames = list(NULL, variable.colname))
  nrows <- nrow(tbl)

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Loop through column input and add each
  for (ii in 1: length(columns)) {

    column.ii <- columns[ii]

    if (column.ii == "n") {

      # N
      tbl <- cbind(tbl, matrix(c(length(geefit$residuals), rep("", nrows - 1)),
                               ncol = 1, dimnames = list(NULL, "N")))

    } else if (column.ii == "events") {

      # Events
      tbl <- cbind(tbl, matrix(c(sum(geefit$model[, 1]), rep("", nrows - 1)),
                               ncol = 1, dimnames = list(NULL, "Events")))

    } else if (column.ii == "beta") {

      # Beta
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, beta.char))
      newcol[entry.rows, 1] <- sprintf(spf, betas)
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "se") {

      # SE
      newcol <- matrix("-", ncol = 1, nrow = nrows, dimnames = list(NULL, "SE"))
      newcol[entry.rows, 1] <- sprintf(spf, ses)
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "beta.se") {

      # Beta (SE)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste(beta.char, "(SE)")))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, betas), " (", sprintf(spf, ses), ")", sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "beta.betaci") {

      # Beta (95% CI)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste(beta.char, "(95% CI)")))
      zcrit <- qnorm(0.975)
      newcol[entry.rows, 1] <- paste(sprintf(spf, betas), " (",
                                     sprintf(spf, betas - zcrit * ses), ci.sep,
                                     sprintf(spf, betas + zcrit * ses), ")",
                                    sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "betaci") {

      # 95% CI for Beta
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste("95% CI for", beta.char)))
      zcrit <- qnorm(0.975)
      newcol[entry.rows, 1] <- paste(sprintf(spf, betas - zcrit * ses), ci.sep,
                                     sprintf(spf, betas + zcrit * ses), ")",
                                     sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "or") {

      # OR
      newcol <- matrix("", ncol = 1, nrow = nrows, dimnames = list(NULL, "OR"))
      newcol[entry.rows, 1] <- sprintf(spf, exp(betas))
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "or.orci") {

      # OR (95% CI)
      newcol <- matrix("-", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "OR (95% CI)"))
      zcrit <- pnorm(0.975)
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, exp(betas)), " (",
              sprintf(spf, exp(betas - zcrit * ses)), ci.sep,
              sprintf(spf, exp(betas + zcrit * ses)), ")", sep = "")
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "orci") {

      # 95% CI for OR
      newcol <- matrix("-", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "95% CI for OR"))
      zcrit <- pnorm(0.975)
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, exp(betas - zcrit * ses)), ci.sep,
              sprintf(spf, exp(betas + zcrit * ses)), sep = "")
      if (intercept) {
        newcol[1, 1] < -"-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "z") {

      # z
      newcol <- matrix("-", ncol = 1, nrow = nrows, dimnames = list(NULL, "z"))
      newcol[entry.rows, 1] <- sprintf(spf, zs)
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "p") {

      # P
      newcol <- matrix("", ncol = 1, nrow = nrows, dimnames = list(NULL, "P"))
      newcol[entry.rows, 1] <-
        formatp(p = ps, cuts = p.cuts, decimals = p.decimals,
                lowerbound = p.lowerbound, leading0 = p.leading0,
                avoid1 = p.avoid1)
      newcol[ref.rows, 1] <- "-"

    }

    # Add column to table
    tbl <- cbind(tbl, newcol)

  }

  # Add bold column names if requested
  if (format.xtable & bold.colnames) {
    colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
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
