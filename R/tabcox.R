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
#'
#' @param coxfit Object returned from \code{\link[survival]{coxph}}.
#'
#' @param events Logical value for whether to include a number of events
#' (i.e. uncensored observations) column.
#'
#' @param beta.se Logical value for whether to include Beta (SE) column.
#'
#' @param hr.ci Logical value for whether to include HR (95\% CI) column showing
#' estimated hazard ratio and associated confidence interval.
#'
#' @param ci.sep Character string with separator to place between lower and
#' upper bound of confidence interval. Typically \code{"-"} or \code{", "}.
#'
#' @param p Logical value for whether to include p-value column.
#'
#'
#' @return Character matrix with table summarizing the fitted Cox PH model.
#'
#'
#' @examples
#' # Load in sample dataset d and drop rows with missing values
#' data(d)
#' d <- d[complete.cases(d), ]
#'
#' # Test whether race is associated with survival
#' coxfit1 <- coxph(Surv(time = d$time, event = d$delta) ~ Race, data = d)
#' coxtable1 <- tabcox(coxfit1)
#'
#' # Test whether age, sex, race, and treatment group are associated with
#' # survival
#' coxfit2 <- coxph(Surv(time = d$time, event = d$delta) ~ Age + Sex + Race +
#'                  Group, data = d)
#' coxtable2 <- tabcox(coxfit2)
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
tabcox <- function(coxfit, latex = FALSE, xlabels = NULL, decimals = 2,
                   p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
                   p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE,
                   events = FALSE, beta.se = TRUE, hr.ci = TRUE, ci.sep = ", ",
                   p = TRUE, greek.beta = FALSE, binary.compress = TRUE,
                   bold.colnames = TRUE, variable.colname = "Variable",
                   print.html = FALSE, html.filename = "table1.html") {

  # Check that coxfit is right class
  if (class(coxfit) != "coxph") {
    stop("For coxfit input, please enter an object returned from the coxph function")
  }

  # Extract info from coxfit
  summary.coxfit <- summary(coxfit)
  coef.matrix <- summary.coxfit$coefficients

  # Get variable names for factors
  x.factors <- names(coxfit$xlevels)

  # Determine whether xlabels has to be created
  if (is.null(xlabels)) {
    create.xlabels <- TRUE
    xlabels <- c()
  }

  # Create/reformat xlabels and determine row indices for numbers
  if (is.null(x.factors)) {
    entry.rows <- 1: nrow(coef.matrix)
    if (create.xlabels) {
      xlabels <- rownames(coef.matrix)
    }
  } else {
    rowcount <- 0
    entry.rows <- c()
    indent.rows <- c()
    for (ii in 1: length(unlist(strsplit(paste(as.character(coxfit$call),
                                               collapse = "_"),
                                         split = "+", fixed = TRUE)))) {
      rowcount <- rowcount + 1
      varname.ii <- as.character(coxfit$terms[ii][[3]])
      factor.ii <- varname.ii %in% x.factors
      if (factor.ii) {
        levels.ii <- as.character(unlist(coxfit$xlevels[varname.ii]))
        nlevels.ii <- length(levels.ii)
        if (nlevels.ii == 2 & binary.compress) {
          if (create.xlabels) {
            xlabels[rowcount] <-
              as.character(unlist(coxfit$xlevels[varname.ii]))[-1]
          }
          entry.rows <- c(entry.rows, rowcount)
        } else {
          if (create.xlabels) {
            xlabels[rowcount: (rowcount + nlevels.ii)] <- c(varname.ii, levels.ii)
          }
          xlabels[(rowcount + 1): (rowcount + nlevels.ii)] <-
            paste("  ", xlabels[(rowcount + 1): (rowcount + nlevels.ii)],
                  sep = "")
          xlabels[rowcount + 1] <-
            paste(xlabels[rowcount + 1], " (ref)", sep = "")
          entry.rows <- c(entry.rows, (rowcount + 2): (rowcount + nlevels.ii))
          indent.rows <- c(indent.rows, (rowcount + 1): (rowcount + nlevels.ii))
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

  # Add N column if requested
  if (n) {
    tbl <- cbind(tbl, matrix(c(coxfit$n, rep("", nrows - 1)), ncol = 1,
                             dimnames = list(NULL, "N")))
  }

  # Add events column if requested
  if (events) {
    tbl <- cbind(tbl, matrix(c(coxfit$nevent, rep("", nrows - 1)), ncol = 1,
                             dimnames = list(NULL, "Events")))
  }

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Add Beta (SE) column if requested
  if (beta.se) {
    beta.se.col <- matrix("", ncol = 1, nrow = nrows,
                          dimnames = list(NULL, "Beta (SE)"))
    beta.se.col[entry.rows] <-
      paste(sprintf(spf, coef.matrix[, "coef"]), " (",
            sprintf(spf, coef.matrix[, "se(coef)"]), ")", sep = "")
    tbl <- cbind(tbl, beta.se.col)
  }

  # Add HR (95% CI) column if requested
  if (hr.ci) {
    hr.ci.col <- matrix("", ncol = 1, nrow = nrows,
                        dimnames = list(NULL, "HR (95% CI)"))
    hr.ci.col[entry.rows] <-
      paste(sprintf(spf, coef.matrix[, "exp(coef)"]), " (",
            sprintf(spf, summary.coxfit$conf.int[, "lower .95"]), ci.sep,
            sprintf(spf, summary.coxfit$conf.int[, "upper .95"]), ")", sep = "")
    tbl <- cbind(tbl, hr.ci.col)
  }

  # Add p-value column if requested
  if (p) {
    p.col <- matrix("", ncol = 1, nrow = nrows,
                    dimnames = list(NULL, "P"))
    p.col[entry.rows] <-
      formatp(p = coef.matrix[, "Pr(>|z|)"], cuts = p.cuts,
              decimals = p.decimals, lowerbound = p.lowerbound,
              leading0 = p.leading0, avoid1 = p.avoid1)
    tbl <- cbind(tbl, p.col)
  }

  # Reformat for LaTeX if requested
  if (latex) {
    if (greek.beta) {
      colnames(tbl)[which(colnames(tbl) == "Beta (SE)")] <-
        "$\\hat{\\beta}$ (SE)"
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
  }

  # Print html version of table if requested
  if (print.html) {

    tbl.xtable <-
      xtable(tbl, align = paste("ll", paste(rep("r", ncol(tbl) - 1),
                                            collapse = ""),
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
