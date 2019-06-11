#' Generate Summary Table for Fitted Generalized Linear Model (GLM)
#'
#' Creates table summarizing a GLM fit using the \code{\link[stats]{glm}}
#' function.
#'
#'
#' @inheritParams tabmeans
#' @param latex Logical value for whether to format table so it is
#' ready for printing in LaTeX via \code{\link[xtable]{xtable}} or
#' \code{\link[knitr]{kable}}.
#'
#'
#' @param fit Object returned from \code{\link[stats]{glm}}.
#'
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"beta"}, \code{"se"}, \code{"betaci"} for 95\% CI
#' for Beta, \code{"beta.se"} for Beta (SE), \code{"beta.ci"} for Beta
#' (95\% CI), \code{"or"}, \code{"orci"} for 95\% CI for OR, \code{"or.ci"} for
#' OR (95\% CI), \code{"hr"}, \code{"hrci"} for 95\% CI for HR, \code{"hr.ci"}
#' for HR (95\% CI), \code{"test"} for z/t statistic, and \code{"p"}. Note that
#' if you request OR's or HR's the function will trust exponentiated betas
#' correspond to these quantities.
#'
#' @param xlabels Character vector with labels for the \code{x} variables and
#' their levels. Often useful to leave as \code{NULL} first, see how the table
#' looks, and then re-run with labels where they need to be.
#'
#' @param compress.factors Logical value for whether to display factor variables
#' in compressed format, with variable names omitted and levels not indented.
#' Can also be \code{"binary"} to compress binary factor variables only.
#'
#'
#'
#' @return Character matrix summarizing the fitted GLM.
#'
#'
#' @examples
#' # Load in sample dataset and drop rows with missing values
#' data(tabdata)
#' tabdata <- tabdata[complete.cases(tabdata), ]
#'
#' # Linear regression: BMI vs. age, sex, race, and treatment
#' glmfit1 <- glm(BMI ~ Age + Sex + Race + Group, data = tabdata)
#' (lintable <- tabglm(fit = glmfit1))
#'
#' # Logistic regression: 1-year mortality vs. age, sex, race, and treatment.
#' # Display factors in "compressed" format
#' glmfit2 <- glm(death_1yr ~ Age + Sex + Race + Group, data = tabdata,
#'                family = binomial)
#' (logtable1 <- tabglm(fit = glmfit2, compress.factors = TRUE))
#'
#' # Logistic regression with higher-order terms
#' glmfit3 <- glm(death_1yr ~ poly(Age, 2, raw = TRUE) + Sex + BMI + Sex * BMI,
#'                data = tabdata, family = "binomial")
#' (logtable2 <- tabglm(fit = glmfit3))
#'
#'
fit <- glm(BMI ~ Age + Sex + Race + Group, data = tabdata)
fit <- glm(bp.2 ~ poly(bp.1, 2, raw = TRUE) + Age + BMI + Sex + Age*BMI + Age*Sex + Sex*Race, data = tabdata)
columns = NULL
xlabels = NULL
compress.factors = FALSE
sep.char = ", "
latex = FALSE
decimals = 2
formatp.list = NULL
print.html = FALSE
html.filename = "table1.html"
# Note to Dane: Allow for decimals to be vector-valued!
tabglm <- function(fit,
                   columns = NULL,
                   xlabels = NULL,
                   compress.factors = FALSE,
                   sep.char = ", ",
                   latex = FALSE,
                   decimals = 2,
                   #p.decimals = c(2, 3),
                   #p.cuts = 0.01,
                   #p.lowerbound = 0.001,
                   #p.leading0 = TRUE,
                   #p.avoid1 = FALSE,
                   formatp.list = NULL,
                   print.html = FALSE,
                   html.filename = "table1.html") {

  # # Extract info from fit
  # invisible(capture.output(summary.fit <- summary(fit)))
  # coefmat <- summary.fit$coefficients
  # rownames.coefmat <- rownames(coefmat)
  # betas <- coefmat[, "Estimate"]
  # ses <- coefmat[, "Std. Error"]
  # ps <- coefmat[, 4]
  # df <- fit$df.residual
  # intercept <- rownames.coefmat[1] == "(Intercept)"
  # glm.family <- fit$family$family
  # glm.link <- fit$family$link

  # Extract info from fit
  invisible(capture.output(summary.fit <- summary(fit)))
  coefmat <- summary.fit$coefficients
  rownames.coefmat <- rownames(coefmat)
  intercept <- attr(fit$terms, "intercept") == 1

  # If columns unspecified, figure out reasonable defaults
  if (is.null(columns)) {

    glm.family <- fit$family$family
    glm.link <- fit$family$link
    if (glm.family == "binomial" & glm.link == "logit") {
      columns <- c("beta.se", "or.ci", "p")
    } else if (glm.family == "poisson" & glm.link == "log") {
      columns <- c("beta.se", "hr.ci", "p")
    } else {
      columns <- c("beta.se", "betaci", "p")
    }

  }

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Initialize table
  #df <- dplyr::tibble(Variable = rownames.coefmat)
  df <- data.frame(Variable = rownames.coefmat, stringsAsFactors = FALSE)

  # Loop through and add columns requested
  for (column in columns) {
    if (column == "beta") {
      df$`Beta` <- sprintf(spf, coefmat[, "Estimate"])
    } else if (column == "se") {
      df$`SE` <- sprintf(spf, coefmat[, "Std. Error"])
    } else if (column == "betaci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste("(", sprintf(spf, confint.fit[, 1]), ", ",
                                sprintf(spf, confint.fit[, 2]), ")", sep = "")
      #if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "beta.se") {
      df$`Beta (SE)` <- paste(sprintf(spf, coefmat[, "Estimate"]), " (",
                              sprintf(spf, coefmat[, "Std. Error"]), ")",
                              sep = "")
    } else if (column == "beta.ci") {
      confint.fit <- confint(fit)
      df$`Beta (95% CI)` <- paste(sprintf(spf, coefmat[, "Estimate"]), " (",
                                  sprintf(spf, confint.fit[, 1]), ", ",
                                  sprintf(spf, confint.fit[, 2]), ")", sep = "")
      #if (intercept) df$`Beta (95% CI)`[1] <- "-"
    } else if (column == "or") {
      df$`OR` <- sprintf(spf, exp(coefmat[, "Estimate"]))
      if (intercept) df$`OR`[1] <- "-"
    } else if (column == "orci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste("(", sprintf(spf, exp(confint.fit[, 1])), ", ",
                                sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "or_ci") {
      confint.fit <- confint(fit)
      df$`OR (95% CI)` <- paste(sprintf(spf, exp(fit$coef)), " (",
                                sprintf(spf, exp(confint.fit[, 1])), ", ",
                                sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`OR (95% CI)`[1] <- "-"
    } else if (column == "hr") {
      df$`HR` <- sprintf(spf, exp(coefmat[, "Estimate"]))
    } else if (column == "hrci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste("(", sprintf(spf, exp(confint.fit[, 1])), ", ",
                           sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "hr.ci") {
      confint.fit <- confint(fit)
      df$`HR (95% CI)` <- paste(sprintf(spf, exp(coefmat[, "Estimate"])), " (",
                                sprintf(spf, exp(confint.fit[, 1])), ", ",
                                sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`HR (95% CI)`[1] <- "-"
    } else if (column == "z") {
      df$`z` <- sprintf(spf, coefmat[, "Estimate"] / coefmat[, "Std. Error"])
    } else if (column == "test") {
      if ("t value" %in% colnames(coefmat)) {
        df$`t` <- sprintf(spf, coefmat[, "t value"])
      } else if ("z value" %in% colnames(coefmat)) {
        df$`z` <- sprintf(spf, coefmat[, "z value"])
      }
    } else if (column == "p") {
      df$`P` <- do.call(formatp, c(list(p = coefmat[, ncol(coefmat)]),
                                   formatp.list))
    }
  }

  # 1. Go through xlevels and clean up factors.
  # 2. Go through term.labels and look for polynomials. Clean them up.
  # 3. Go through term.labels and look for interactions. Clean them up.

  # Clean up factor variables
  xlevels <- fit$xlevels
  if (length(xlevels) > 0) {
    for (ii in 1: length(xlevels)) {
      varname.ii <- names(xlevels)[ii]
      levels.ii <- xlevels[[ii]]
      locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
      df$Variable[locs] <- gsub(pattern = varname.ii, replacement = "  ", x = df$Variable[locs])
      newrows <- matrix("", nrow = 2, ncol = ncol(df), dimnames = list(NULL, names(df)))
      newrows[2, ] <- "-"
      newrows[, 1] <- c(varname.ii, paste("  ", paste(levels.ii[1], " (ref)",
                                                          sep = ""), sep = ""))
      df <- rbind(df[1: (locs[1] - 1), ], newrows, df[locs[1]: nrow(df), ])
    }
  }

  # Clean up interaction terms. If both numeric, just change : to " by ". If one
  # numeric and one factor... unsure. If both factor, add "Var1 by Var2" row and
  # then just clean up labels.

  # How about this. If both factors, add row for Var1 by Var2 interaction, and
  # then just show various combination of levels, maybe with comma in between?
  # If one continuous and other factor, same? If both continuous, just reformat?
  interactions <- grep(":", attr(fit$terms, "term.labels"), value = TRUE)
  for (interaction.ii in interactions) {
    components <- unlist(strsplit(interaction.ii, ":"))
    locs <- intersect(grep(components[1], df$Variable),
                      grep(components[2], df$Variable))
    if (length(locs) == 1) {
      df$Variable[locs] <- paste(components, collapse = " by ")
    } else {
      labs <- df$Variable[locs]
      labs <- gsub(components[1], "", labs)
      labs <- gsub(components[2], "", labs)
      labs <- gsub(":", ", ", labs)
      df$Variable[locs] <- paste("  ", labs, sep = "")
      newrow <- matrix("", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
      newrow[1, 1] <- paste(components, collapse = " by ")
      df <- rbind(df[1: (locs[1] - 1), ], newrow, df[locs[1]: nrow(df), ])
    }
  }

      #labs <- paste(xlevels[components])
      df$Variable[locs] <- sub(components[1], df$Variable[locs], "")
      newrows <- matrix("", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
      newrows[1, 1] <- paste(components, collapse = " by ")
      df <- rbind(df[1: (locs[1] - 1), ], newrows, df[locs[1]: nrow(df), ])



      locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
      df$Variable[locs] <- gsub(pattern = varname.ii, replacement = "  ", x = df$Variable[locs])
      newrows <- matrix("", nrow = 2, ncol = ncol(df), dimnames = list(NULL, names(df)))
      newrows[2, ] <- "-"
      newrows[, 1] <- c(varname.ii, paste("  ", paste(levels.ii[1], " (ref)",
                                                      sep = ""), sep = ""))
      df <- rbind(df[1: (locs[1] - 1), ], newrows, df[locs[1]: nrow(df), ])
    }
  }

  term.labels <- attr(fit$terms, "term.labels")
  dataClasses <- attr(fit$terms, "dataClasses")
  interactions <- term.labels[grep(":", term.labels)]
  if (length(interactions) > 0) {
    for (ii in 1: length(interactions)) {
      interaction.ii <- interactions[ii]
      components <- unlist(strsplit(interaction.ii, ":"))
      num.numeric <- sum(dataClasses[components] == "numeric")
      if (num.numeric == 2)) {
        loc <- which(df$Variable == interaction.ii)
        df$Variable[loc] <- paste(components, collapse = " by ")
      } else if (num.numeric == 1) {

        # Find terms that include both variable names and :, and remove variable
        # names and replace : with , ... Want "Age, Black" "Age, White" etc.
        factor.component <- names
        factor.levels <- xlevels[[factor.component]]
        sapply(factor.com)
        locs <- intersect(grep(components[1], df$Variable),
                          grep(components[2], df$Variable))

locs <- which(grep("df$Variable)

        factor.levels <- length(xlevels[[factor.component]])
        if (factor.levels == 2) {
          loc <- which(df$Variable == )
          df$Variable[]
        }
      }


    }
  }

        add_row(df, newrows, .before = locs[1])


      row1 <- paste(varname.ii, levels.ii[2], sep = "")
      num.newrows <- levels.ii
      newrows <- df[1: length(levels.ii), ]
      newrows$Variable <-



      loc <- which(df$Variable == paste(varname.ii, levels.ii[2], sep = ""))
      df <- rbind(df[1: (loc - 1)])

      rbind(df[1: 3, ], NA, df[4: nrow(df), ])
      df2 <- df %>%
        dplyr::add_row(NA, .before = loc)

      loc <- which(rownames.coefmat == paste(varname.ii, levels.ii[2], sep = ""))
    }
  }

  # Go through predictors and clean up as necessary
  predictors <- attr(fit$terms, "term.labels")
  classes <- attr(fit$terms, "dataClasses")[-1]
  for (predictor in predictors) {

    predictor.ii <- predictors[ii]
    class.ii <- classes[ii]
    if (class.ii == "factor") {
      loc <- which()
    }

  }


  for (ii in 1: length())

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

  # # If necessary, force compress.factors to be TRUE and notify user of reason
  # if (interactions & (compress.factors != FALSE | omit.refgroups != FALSE)) {
  #     message("Because the fitted GLM has interaction terms, 'compress.factors' and 'omit.refgroups' are being set to TRUE. This limitation may be addressed in future versions of 'tabglm'.")
  #   compress.factors <- omit.refgroups <- TRUE
  # }

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
  tbl <- matrix(xlabels, ncol = 1, dimnames = list(NULL, "Variable"))
  nrows <- nrow(tbl)

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Loop through column input and add each
  for (ii in 1: length(columns)) {

    column.ii <- columns[ii]

    if (column.ii == "n") {

      # N
      newcol <- matrix(c(length(fit$residuals), rep("", nrows - 1)),
                       ncol = 1, dimnames = list(NULL, "N"))

    } else if (column.ii == "events") {

      # Events
      newcol <- matrix(c(sum(fit$model[, 1]), rep("", nrows - 1)),
                       ncol = 1, dimnames = list(NULL, "Events"))

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
      ci.fit <- confint(fit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "OR (95% CI)"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, exp(betas)), " (",
              sprintf(spf, exp(ci.fit[, 1])), sep.char,
              sprintf(spf, exp(ci.fit[, 2])), ")", sep = "")
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "orci") {

      # 95% CI for OR
      ci.fit <- confint(fit)
      newcol <- matrix("", ncol = 1, nrow = nrows,
                       dimnames = list(NULL, "95% CI for OR"))
      newcol[entry.rows, 1] <-
        paste(sprintf(spf, exp(ci.fit[, 1])), sep.char,
              sprintf(spf, exp(ci.fit[, 2])), sep = "")
      if (intercept) {
        newcol[1, 1] <- "-"
      }
      newcol[ref.rows, 1] <- "-"

    } else if (column.ii == "test") {

      # t or z
      newcol <-
        matrix("", ncol = 1, nrow = nrows,
               dimnames = list(NULL, substr(colnames(coefmat)[3],
                                            start = 1, stop = 1)))
      newcol[entry.rows, 1] <- sprintf(spf, coefmat[, 3])
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

    tbl[tbl == "--"] <- "-"
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
