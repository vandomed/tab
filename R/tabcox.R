#' Generate Summary Table for Fitted Cox Proportional Hazards (Cox PH) Model
#'
#' Fits a Cox PH regression model using the \pkg{survival} package [1, 2] and
#' creates a table summarizing the results.
#'
#'
#' @inheritSection tabmeans Note
#' @inheritParams tabmeans
#' @inheritParams tabglm
#'
#'
#' @param fit Object returned from \code{\link[survival]{coxph}}.
#'
#' @param columns Character vector specifying what columns to include. Choies
#' for each element are \code{"events"}, \code{"beta"}, \code{"se"},
#' \code{"beta.se"}, \code{"beta.betaci"}, \code{"betaci"}, \code{"hr"},
#' \code{"hr.hrci"}, \code{"hrci"}, \code{"z"}, and \code{"p"}.
#'
#'
#' @return Character matrix summarizing the fitted Cox PH model.
#'
#'
#' @examples
#' # Load in sample dataset and drop rows with missing values
#' data(tabdata)
#' tabdata <- tabdata[complete.cases(tabdata), ]
#'
#' # Test whether race is associated with survival
#' coxfit1 <- coxph(Surv(time = tabdata$time, event = tabdata$delta) ~ Race,
#'                  data = tabdata)
#' coxtable1 <- tabcox(fit = coxfit1)
#'
#' # Test whether age, sex, race, and treatment group are associated with
#' # survival
#' coxfit2 <- coxph(Surv(time = tabdata$time, event = tabdata$delta) ~
#'                  Age + Sex + Race + Group, data = tabdata)
#' coxtable2 <- tabcox(fit = coxfit2)
#'
#'
#' @references
#' 1. Therneau, T. (2015). A Package for Survival Analysis in S. R package
#' version 2.38. \url{https://cran.r-project.org/package=survival}.
#'
#' 2. Therneau, T.M. and Grambsch, P.M. (2000). Modeling Survival Data:
#' Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.
#'
#' 3. Dahl, D.B. (2016). xtable: Export Tables to LaTeX or HTML. R package
#' version 1.8-2, \url{https://cran.r-project.org/package=xtable}.
#'
#' 4. Ushley, K. (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
#' \url{https://CRAN.R-project.org/package=Kmisc}.
#'
#'
#'@export
tabcox <- function(fit, columns = c("beta.se", "hr.hrci", "p"), xlabels = NULL,
                   compress.factors = FALSE, omit.refgroups = compress.factors,
                   sep.char = ", ", latex = FALSE, decimals = 2,
                   p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
                   p.leading0 = TRUE, p.avoid1 = FALSE,
                   variable.colname = "Variable", print.html = FALSE,
                   html.filename = "table1.html") {

  # Extract info from fit
  summary.fit <- summary(fit)
  coefmat <- summary.fit$coefficients
  rownames.coefmat <- rownames(coefmat)
  betas <- coefmat[, "coef"]
  hrs <- coefmat[, "exp(coef)"]
  ses <- coefmat[, "se(coef)"]
  zs <- coefmat[, "z"]
  ps <- coefmat[, "Pr(>|z|)"]
  intercept <- rownames.coefmat[1] == "(Intercept)"

  # Determine whether xlabels has to be created
  create.xlabels <- is.null(xlabels)

  # Determine whether there are factors, interactions, or polynomials
  fit.xlevels <- fit$xlevels
  x.factors <- names(fit.xlevels)
  factors <- length(x.factors) > 0

  interaction.rows <- grep(pattern = ":", x = rownames.coefmat)
  interactions <- length(interaction.rows) > 1

  polynomial.rows <- grep(pattern = "poly(", x = rownames.coefmat, fixed = TRUE)
  polynomials <- length(polynomial.rows) > 0

  # If necessary, force compress.factors to be TRUE and notify user of reason
  if (interactions & (compress.factors != FALSE | omit.refgroups != FALSE)) {
    message("Because the fitted GEE has interaction terms, 'compress.factors' and 'omit.refgroups' are being set to TRUE. This limitation may be addressed in future versions of 'tabgee'.")
    compress.factors <- omit.refgroups <- TRUE
  }

  # Determine row indices for table entries and create xlabels vector
  if (compress.factors == TRUE & omit.refgroups == TRUE) {

    # Compressed factor formatting

    # All rows of table have entries
    entry.rows <- 1: nrow(coefmat)
    ref.rows <- NULL

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

      # Clean up factors
      if (factors) {
        for (ii in 1: length(x.factors)) {
          xlabels <- unlist(lapply(xlabels, function(x)
            gsub(pattern = x.factors[ii], replacement = "", x = x)))
        }
      }

      # Clean up interactions
      if (interactions) {
        for (ii in 1: length(x.factors)) {
          xlabels[interaction.rows] <-
            unlist(lapply(xlabels[interaction.rows], function(x)
              gsub(pattern = x.factors[ii], replacement = "", x = x)))
        }
      }

    }

  } else {

    # Expanded factor formatting

    rowcount <- ifelse(intercept, 1, 0)
    entry.rows <- rowcount
    factorname.rows <- c()
    ref.rows <- c()
    bfactorname.rows <- c()
    blevel.rows <- c()
    bref.rows <- c()

    if (create.xlabels & intercept) {
      xlabels <- "Intercept"
    }
    for (ii in 1:
         length(unlist(strsplit(x = deparse(fit$call, width.cutoff = 500),
                                split = "+", fixed = TRUE)))) {

      rowcount <- rowcount + 1
      varname.ii <- deparse(fit$terms[ii][[3]])
      if (substr(varname.ii, 1, 4) == "poly") {

        # Clean up polynomial
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

        # Clean up factor
        levels.ii <- as.character(unlist(fit$xlevels[varname.ii]))
        nlevels.ii <- length(levels.ii)
        new.entries <- (rowcount + 1): (rowcount + nlevels.ii)
        if (create.xlabels) {
          xlabels[rowcount] <- varname.ii
          xlabels[new.entries] <- levels.ii
        }
        xlabels[new.entries] <-
          paste("  ", xlabels[new.entries], sep = "")
        xlabels[rowcount + 1] <- paste(xlabels[rowcount + 1], "(ref)")

        entry.rows <- c(entry.rows, (rowcount + 2): (rowcount + nlevels.ii))
        factorname.rows <- c(factorname.rows, rowcount)
        ref.rows <- c(ref.rows, rowcount + 1)
        if (nlevels.ii == 2) {
          bfactorname.rows <- c(bfactorname.rows, rowcount)
          bref.rows <- c(bref.rows, rowcount + 1)
          blevel.rows <- c(blevel.rows, rowcount + 1, rowcount + 2)
        }
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
      newcol <- matrix(c(length(fit$residuals), rep("", nrows - 1)), ncol = 1,
                       dimnames = list(NULL, "N"))

    } else if (column.ii == "events") {

      # Events
      newcol <- matrix(c(fit$nevent, rep("", nrows - 1)), ncol = 1,
                    dimnames = list(NULL, "Events"))

    } else if (column.ii == "beta") {

      # Beta
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "Beta"))
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
                       dimnames = list(NULL, "Beta (SE)"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, betas), " (", sprintf(spf, ses), ")", sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "beta.betaci") {

      # Beta (95% CI)
      ci.fit <- confint(fit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "Beta (95% CI)"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, betas), " (",
              sprintf(spf, ci.fit[, 1]), sep.char,
              sprintf(spf, ci.fit[, 2]), ")", sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "betaci") {

      # 95% CI for Beta
      ci.fit <- confint(fit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "95% CI for Beta"))
      newcol[entry.rows, 1] <- paste(sprintf(spf, ci.fit[, 1]), sep.char,
                                     sprintf(spf, ci.fit[, 2]), sep = "")
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "hr") {

      # HR
      newcol <- matrix("", ncol = 1, nrow = nrows, dimnames = list(NULL, "HR"))
      newcol[entry.rows, 1] <- sprintf(spf, hrs)
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "hr.hrci") {

      # HR (95% CI)
      ci.fit <- summary.fit$conf.int
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "HR (95% CI)"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, hrs), " (",
              sprintf(spf, ci.fit[, "lower .95"]), sep.char,
              sprintf(spf, ci.fit[, "upper .95"]), ")", sep = "")
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "hrci") {

      # 95% CI for HR
      ci.fit <- summary.fit$conf.int
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "95% CI for OR"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, ci.fit[, "lower .95"]), sep.char,
              sprintf(spf, ci.fit[, "upper .95"]), sep = "")
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "z") {

      # z
      newcol <- matrix("-", ncol = 1, nrow = nrows, dimnames = list(NULL, "Z"))
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

  # Drop rows with factor variable names and reference groups if requested
  if ((compress.factors != FALSE | omit.refgroups != FALSE) & !
      (compress.factors == TRUE & omit.refgroups == TRUE)) {
    rows.remove <- c()
    if (compress.factors == "binary") {
      tbl[blevel.rows, 1] <- gsub(pattern = "  ", replacement = "",
                                  x = tbl[blevel.rows, 1], fixed = TRUE)
      rows.remove <- c(rows.remove, bfactorname.rows)
    } else if (compress.factors == TRUE) {
      tbl[, 1] <- gsub(pattern = "  ", replacement = "",
                       x = tbl[, 1], fixed = TRUE)
      rows.remove <- c(rows.remove, factorname.rows)
    }
    if (omit.refgroups == "binary") {
      rows.remove <- c(rows.remove, bref.rows)
    } else if (omit.refgroups == TRUE) {
      rows.remove <- c(rows.remove, ref.rows)
    }
    tbl <- tbl[-rows.remove, , drop = FALSE]
  }

  # Reformat for xtable if necessary
  if (latex) {
    tbl[tbl == "-"] <- "--"
    tbl[, 1] <- gsub(pattern = "  ", replacement = "\\ \\ \\ \\ ",
                     x = tbl[, 1], fixed = TRUE)
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
