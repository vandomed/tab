#' Generate Summary Table for Fitted Generalized Linear Model (GLM)
#'
#' Creates table summarizing a GLM fit using the \code{\link[stats]{glm}}
#' function.
#'
#'
#' @inherit tabmeans references
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#'
#'
#' @param glmfit Object returned from \code{\link[stats]{glm}}.
#'
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"beta"}, \code{"se"}, \code{"beta.se"},
#' \code{"beta.betaci"}, \code{"betaci"}, \code{"or"}, \code{"or.orci"},
#' \code{"orci"}, \code{"test"} (for z/t test statistic), and \code{"p"}.
#'
#' @param xlabels Character vector with labels for the \code{x} variables and
#' their levels. Often useful to leave as \code{NULL} first, see how the table
#' looks, and then re-run with labels where they need to be.
#'
#' @param compress.factors Logical value for whether to display factor variables should
#' as one row for each of the non-reference group levels, as opposed to one row
#' for the variable name and one row for each of the levels including the
#' reference group.
#'
#' @param ci.sep Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#'
#' @param greek.beta Logical value for whether to display the Greek letter beta
#' rather than "Beta". Only used if \code{latex = TRUE}.
#'
#'
#' @return Character matrix summarizing the fitted GLM.
#'
#'
#' @examples
#' # Load in sample dataset d and drop rows with missing values
#' data(d)
#' d <- d[complete.cases(d), ]
#'
#' # Linear regression: BMI vs. age, sex, race, and treatment
#' glmfit1 <- glm(BMI ~ Age + Sex + Race + Group, data = d)
#' (lintable <- tabglm(glmfit = glmfit))
#'
#' # Logistic regression: 1-year mortality vs. age, sex, race, and treatment.
#' # Also, use "compressed" format for factors
#' glmfit2 <- glm(death_1yr ~ Age + Sex + Race + Group, data = d,
#'                family = binomial)
#' (logtable1 <- tabglm(glmfit = glmfit2, compress.factors = TRUE))
#'
#' # Logistic regression with higher-order terms
#' glmfit3 <- glm(death_1yr ~ poly(Age, 2, raw = TRUE) + Sex + BMI + Sex * BMI,
#'                data = d, family = "binomial")
#' (logtable2 <- tabglm(glmfit = glmfit3))
#'
#'
#' @export
tabglm <- function(glmfit, columns = NULL, xlabels = NULL,
                   compress.factors = FALSE, ci.sep = ", ", xtable = FALSE,
                   decimals = 2, p.decimals = c(2, 3), p.cuts = 0.01,
                   p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
                   greek.beta = FALSE, bold.colnames = TRUE,
                   variable.colname = "Variable", print.html = FALSE,
                   html.filename = "table1.html") {

  # Extract info from glmfit
  summary.glmfit <- summary(glmfit)
  coefmat <- summary.glmfit$coefficients
  rownames.coefmat <- rownames(coefmat)
  betas <- coefmat[, "Estimate"]
  ses <- coefmat[, "Std. Error"]
  ps <- coefmat[, 4]
  df <- glmfit$df.residual
  intercept <- rownames.coefmat[1] == "(Intercept)"
  glm.fam <- glmfit$family$family
  glm.link <- glmfit$family$link

  # Default columns to include depending on family of GLM
  if (is.null(columns)) {
    if (glm.fam == "binomial" & glm.link == "logit") {
      columns <- c("beta.se", "or.orci", "p")
    } else {
      columns <- c("beta.se", "p")
    }
  }

  # Determine whether xlabels has to be created
  if (is.null(xlabels)) {
    create.xlabels <- TRUE
    if (intercept) {
      xlabels <- "Intercept"
    } else {
      xlabels <- c()
    }
  }

  # Determine whether there are factors, interactions, or polynomials
  glmfit.xlevels <- glmfit$xlevels
  x.factors <- names(glmfit.xlevels)
  factors <- length(x.factors) > 0

  interactions <- length(grep(pattern = ":", x = rownames.coefmat)) > 1

  polynomial.rows <- grep(pattern = "poly(", x = rownames.coefmat, fixed = TRUE)
  polynomials <- length(polynomial.rows) > 0

  # Choose version of beta and leading spaces to use
  space.char <- ifelse(xtable, "$\\hskip .4cm$", "  ")
  beta.char <- ifelse(xtable & greek.beta, "$\\hat{\\beta}$", "Beta")

  # Determine row indices for table entries and create xlabels vector
  if (interactions) {

    # If there are interactions, just do basic formatting
    entry.rows <- 1: nrow(coefmat)
    if (create.xlabels) {
      if (intercept) {
        xlabels <- c(xlabels, rownames(coefmat)[-1])
      } else {
        xlabels <- rownames(coefmat)
      }
    }
    for (ii in 1: length(x.factors)) {
      xlabels <- unlist(lapply(xlabels, function(x)
        gsub(pattern = x.factors[ii], replacement = "", x = x)))
    }
    if (polynomials & create.xlabels) {
      for (ii in polynomial.rows) {
        xlabel.parts <- unlist(strsplit(x = xlabels[ii], split = ","))
        varname.ii <- substr(xlabel.parts[1], start = 6, stop = 100)
        poly.order <- as.numeric(xlabel.parts[2])
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
  } else {

    # Otherwise, format more neatly
    rowcount <- 1
    entry.rows <- 1
    ref.rows <- c()
    for (ii in 1:
         length(unlist(strsplit(x = deparse(glmfit$call, width.cutoff = 500),
                                split = "+", fixed = TRUE)))) {

      rowcount <- rowcount + 1
      varname.ii <- deparse(glmfit$terms[ii][[3]])
      if (substr(varname.ii, 1, 4) == "poly") {

        # If X is polynomial...
        varname.ii.split <-
          unlist(strsplit(substr(varname.ii, start = 6, stop = 100), split = ","))
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

        # If X is a factor variable...
        levels.ii <- as.character(unlist(glmfit$xlevels[varname.ii]))
        nlevels.ii <- length(levels.ii)
        if (compress.factors) {
          if (create.xlabels) {
            xlabels[rowcount: (rowcount + nlevels.ii - 2)] <- levels.ii[-1]
          }
          entry.rows <- c(entry.rows, rowcount: (rowcount + nlevels.ii - 2))
          rowcount <- rowcount + nlevels.ii - 2
        } else {
          if (create.xlabels) {
            xlabels[rowcount: (rowcount + nlevels.ii)] <- c(varname.ii, levels.ii)
          }
          xlabels[(rowcount + 1): (rowcount + nlevels.ii)] <-
            paste(space.char, xlabels[(rowcount + 1): (rowcount + nlevels.ii)],
                  sep = "")
          xlabels[rowcount + 1] <-
            paste(xlabels[rowcount + 1], " (ref)", sep = "")
          entry.rows <- c(entry.rows, (rowcount + 2): (rowcount + nlevels.ii))
          ref.rows <- c(ref.rows, rowcount + 1)
          rowcount <- rowcount + nlevels.ii
        }
      } else {
        if (create.xlabels) {
          xlabels[rowcount] <- varname.ii
        }
        entry.rows <- c(entry.rows, rowcount)
      }
    }
  }

  # Initialize table
  tbl <- matrix(xlabels, ncol = 1, dimnames = list(NULL, variable.colname))
  nrows <- nrow(tbl)

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Loop through column input and add each
  # Note: May want to do LaTeX formatting here...
  for (ii in 1: length(columns)) {

    column.ii <- columns[ii]

    if (column.ii == "n") {

      # N
      tbl <- cbind(tbl, matrix(c(length(glmfit$residuals), rep("", nrows - 1)),
                               ncol = 1, dimnames = list(NULL, "N")))

    } else if (column.ii == "events") {

      # Events
      tbl <- cbind(tbl, matrix(c(sum(glmfit$model[, 1]), rep("", nrows - 1)),
                               ncol = 1, dimnames = list(NULL, "Events")))

    } else if (column.ii == "beta") {

      # Beta
      newcol <- matrix("", ncol = 1, nrow = nrows,
                        dimnames = list(NULL, beta.char))
      newcol[entry.rows, ] <- sprintf(spf, betas)

    } else if (column.ii == "se") {

      # SE
      newcol <- matrix("-", ncol = 1, nrow = nrows, dimnames = list(NULL, "SE"))
      newcol[entry.rows, ] <- sprintf(spf, ses)

    } else if (column.ii == "beta.se") {

      # Beta (SE)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste(beta.char, "(SE)")))
      newcol[entry.rows, ] <-
        paste(sprintf(spf, betas), " (", sprintf(spf, ses), ")", sep = "")

    } else if (column.ii == "beta.betaci") {

      # Beta (95% CI)
      ci.glmfit <- confint(glmfit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste(beta.char, "(95% CI)")))
      newcol[entry.rows, ] <- paste(sprintf(spf, betas), " (",
                                    sprintf(spf, ci.glmfit[, 1]), ci.sep,
                                    sprintf(spf, ci.glmfit[, 2]), ")", sep = "")

    } else if (column.ii == "betaci") {

      # 95% CI for Beta
      ci.glmfit <- confint(glmfit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, paste("95% CI for", beta.char)))
      newcol[entry.rows, ] <- paste(sprintf(spf, ci.glmfit[, 1]), ci.sep,
                                    sprintf(spf, ci.glmfit[, 2]), sep = "")

    } else if (column.ii == "or") {

      # OR
      newcol <- matrix("", ncol = 1, nrow = nrows, dimnames = list(NULL, "OR"))
      newcol[entry.rows, ] <- sprintf(spf, exp(betas))
      if (intercept) {
        newcol[1, ] <- "-"
      }

    } else if (column.ii == "or.orci") {

      # OR (95% CI)
      ci.glmfit <- confint(glmfit)
      newcol <- matrix("-", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "OR (95% CI)"))
      newcol[entry.rows, ] <-
        paste(sprintf(spf, exp(betas)), " (",
              sprintf(spf, exp(ci.glmfit[, 1])), ci.sep,
              sprintf(spf, exp(ci.glmfit[, 2])), ")", sep = "")

    } else if (column.ii == "orci") {

      # 95% CI for OR
      ci.glmfit <- confint(glmfit)
      newcol <- matrix("-", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "95% CI for OR"))
      newcol[entry.rows, ] <- paste(sprintf(spf, exp(ci.glmfit[, 1])), ci.sep,
                                    sprintf(spf, exp(ci.glmfit[, 2])), sep = "")

    } else if (column.ii == "test") {

      # t or z
      newcol <-
        matrix("-", ncol = 1, nrow = nrows,
               dimnames = list(NULL, substr(colnames(coefmat)[3],
                                            start = 1, stop = 1)))
      newcol[entry.rows, ] <- sprintf(spf, coefmat[, 3])

    } else if (column.ii == "p") {

      # P
      newcol <- matrix("", ncol = 1, nrow = nrows, dimnames = list(NULL, "P"))
      newcol[entry.rows] <-
        formatp(p = ps, cuts = p.cuts, decimals = p.decimals,
                lowerbound = p.lowerbound, leading0 = p.leading0,
                avoid1 = p.avoid1)

    }

    # Add dash to reference group cells and add column to table
    newcol[ref.rows, ] <- "-"
    tbl <- cbind(tbl, newcol)

  }

  # Add bold column names if requested
  if (xtable & bold.colnames) {
    colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
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

  # Return tbl
  return(tbl)

}
