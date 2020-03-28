#' Create Summary Table for Fitted Cox Proportional Hazards Model
#'
#' Creates a table summarizing a GEE fit using the \code{\link[survival]{coxph}}
#' function.
#'
#'
#' @param fit Fitted \code{\link[survival]{coxph}} object.
#' @param columns Character vector specifying what columns to include. Choies
#' for each element are \code{"events"}, \code{"beta"}, \code{"se"},
#' \code{"beta.se"}, \code{"beta.betaci"}, \code{"betaci"}, \code{"hr"},
#' \code{"hr.hrci"}, \code{"hrci"}, \code{"z"}, and \code{"p"}.
#' @param var.labels Named list specifying labels to use for certain predictors.
#' For example, if \code{fit} includes a predictor named "race"
#' that you want to label "Race/ethnicity" and a predictor named "age_yrs" that
#' you want to label "Age (years)", use
#' \code{var.labels = list(race = "Race/ethnicity", age_yrs = "Age (years)"}.
#' @param factor.compression Integer value from 1 to 5 controlling how much
#' compression is applied to factor predictors (higher value = more
#' compression). If 1, rows are Variable, Level 1 (ref), Level 2, ...; if 2,
#' rows are Variable (ref = Level 1), Level 2, ...; if 3, rows are Level 1
#' (ref), Level 2, ...; if 4, rows are Level 2 (ref = Level 1), ...; if 5, rows
#' are Level 2, ...
#' @param sep.char Character string with separator to place between lower and
#' upper bound of confidence intervals. Typically \code{"-"} or \code{", "}.
#' @param decimals Numeric value specifying number of decimal places for numbers
#' other than p-values.
#' @param formatp.list List of arguments to pass to \code{\link[tab]{formatp}}.
#'
#'
#' @return \code{\link[knitr]{kable}}.
#'
#'
#' @examples
#' # Cox PH model with age, sex, race, and treatment
#' library("survival")
#' fit <- coxph(
#'   Surv(time = time, event = delta) ~ Age + Sex + Race + Group,
#'   data = tabdata
#' )
#' tabcoxph(fit)
#'
#' # Can also use piping
#' fit %>% tabcoxph()
#'
#' # Same as previous, but with custom labels for Age and Race and factors
#' # displayed in slightly more compressed format
#' fit %>%
#'   tabcoxph(
#'     var.labels = list(Age = "Age (years)", Race = "Race/ethnicity"),
#'     factor.compression = 2
#'   )
#'
#' # Cox PH model with some higher-order terms
#' fit <- coxph(
#'   Surv(time = time, event = delta) ~
#'   poly(Age, 2, raw = TRUE) + Sex + Race + Group + Race*Group,
#'   data = tabdata
#' )
#' fit %>% tabcoxph()
#'
#'
#' @references
#' 1. Therneau, T. (2015). A Package for Survival Analysis in S. R package
#' version 2.38. \url{https://cran.r-project.org/package=survival}.
#'
#' 2. Therneau, T.M. and Grambsch, P.M. (2000). Modeling Survival Data:
#' Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.
#'
#'
#'@export
tabcoxph <- function(fit,
                     columns = c("beta.se", "hr.ci", "p"),
                     var.labels = NULL,
                     factor.compression = 1,
                     sep.char = ", ",
                     decimals = 2,
                     formatp.list = NULL) {

  # Error checking
  if (! "coxph" %in% class(fit)) {
    stop("The input 'fit' must be a fitted 'coxph'.")
  }
  if (! is.null(columns) &&
      ! all(columns %in% c("beta", "se", "betaci", "beta.se", "beta.ci", "or",
                           "hr", "hrci", "hr.ci", "z", "p"))) {
    stop("Each element of 'columns' must be one of the following: 'beta', 'se', 'betaci', 'beta.se', 'beta.ci', 'hr', 'hrci', 'hr.ci', 'z', 'p'.")
  }
  if (! factor.compression %in% 1: 5) {
    stop("The input 'factor.compression' must be set to 1, 2, 3, 4, or 5.")
  }
  if (! is.character(sep.char)) {
    stop("The input 'sep.char' must be a character string.")
  }
  if (! (is.numeric(decimals) && decimals >= 0 &&
         decimals == as.integer(decimals))) {
    stop("The input 'decimals' must be a non-negative integer.")
  }
  if (! is.null(formatp.list) &&
      ! (is.list(formatp.list) && all(names(formatp.list) %in%
                                      names(as.list(args(formatp)))))) {
    stop("The input 'format.p' must be a named list of arguments to pass to 'formatp'.")
  }

  # Extract info from fit
  invisible(capture.output(summary.fit <- summary(fit)))
  coefmat <- summary.fit$coefficients
  rownames.coefmat <- rownames(coefmat)
  betas <- coefmat[, "coef"]
  hrs <- coefmat[, "exp(coef)"]
  ses <- coefmat[, "se(coef)"]
  zs <- coefmat[, "z"]
  ps <- coefmat[, "Pr(>|z|)"]
  confint.fit <- confint(fit)
  lower <- confint.fit[, 1]
  upper <- confint.fit[, 2]

  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")

  # Initialize table
  df <- data.frame(Variable = rownames.coefmat, stringsAsFactors = FALSE)

  # Loop through and add columns requested
  for (column in columns) {

    if (column == "beta") {

      df$`Beta` <- sprintf(spf, betas)

    } else if (column == "se") {

      df$`SE` <- sprintf(spf, ses)

    } else if (column == "betaci") {

      df$`95% CI` <- paste("(", sprintf(spf, lower), sep.char,
                           sprintf(spf, upper), ")", sep = "")

    } else if (column == "beta.se") {

      df$`Beta (SE)` <- paste(sprintf(spf, betas), " (",
                              sprintf(spf, ses), ")", sep = "")

    } else if (column == "beta.ci") {

      df$`Beta (95% CI)` <- paste(sprintf(spf, betas), " (",
                                  sprintf(spf, lower), sep.char,
                                  sprintf(spf, upper), ")", sep = "")

    }  else if (column == "hr") {

      df$`HR` <- sprintf(spf, exp(betas))

    } else if (column == "hrci") {

      df$`95% CI` <- paste("(", sprintf(spf, exp(lower)), sep.char,
                           sprintf(spf, exp(upper)), ")", sep = "")

    } else if (column == "hr.ci") {

      df$`HR (95% CI)` <- paste(sprintf(spf, exp(betas)), " (",
                                sprintf(spf, exp(lower)), sep.char,
                                sprintf(spf, exp(upper)), ")", sep = "")

    } else if (column == "z") {

      df$`z` <- sprintf(spf, zs)

    } else if (column == "p") {

      df$`P` <- do.call(formatp, c(list(p = ps), formatp.list))

    }

  }

  # Clean up factor variables
  spaces <- "&nbsp; &nbsp; &nbsp;"
  xlevels <- fit$xlevels
  if (length(xlevels) > 0) {
    for (ii in 1: length(xlevels)) {
      varname.ii <- names(xlevels)[ii]
      levels.ii <- xlevels[[ii]]
      locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
      if (factor.compression == 1) {

        # Rows are Variable, Level 1 (ref), Level 2, ...
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = spaces,
                                  x = df$Variable[locs], fixed = TRUE)
        newrows <- matrix("", nrow = 2, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrows[2, ] <- "-"
        newrows[1, 1] <- ifelse(varname.ii %in% names(var.labels), var.labels[[varname.ii]], varname.ii)
        newrows[2, 1] <- paste(spaces, paste(levels.ii[1], " (ref)", sep = ""), sep = "")
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrows, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 2) {

        # Rows are Variable (ref = Level 1), Level 2, ...
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = spaces, x = df$Variable[locs])
        newrow <- matrix("", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrow[1, 1] <- paste(
          ifelse(varname.ii %in% names(var.labels), var.labels[[varname.ii]], varname.ii),
          " (ref = ", levels.ii[1], ")", sep = ""
        )
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrow, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 3) {

        # Rows are Level 1 (ref), Level 2, ...
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = "", x = df$Variable[locs])
        newrow <- matrix("-", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrow[1, 1] <- paste(levels.ii[1], " (ref)", sep = "")
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrow, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 4) {

        # Rows are Level 2 (ref = Level 1), ...
        df$Variable[locs] <- paste(
          gsub(pattern = varname.ii, replacement = "", x = df$Variable[locs]),
          " (ref = ", levels.ii[1], ")", sep = ""
        )

      } else if (factor.compression == 5) {

        # Rows are Level 2, ...
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = "", x = df$Variable[locs])

      }
    }
  }

  # Clean up interaction terms
  interactions <- grep(":", attr(fit$terms, "term.labels"), value = TRUE)
  for (interaction.ii in interactions) {
    components <- unlist(strsplit(interaction.ii, ":"))
    locs <- intersect(grep(components[1], df$Variable),
                      grep(components[2], df$Variable))
    if (length(locs) == 1) {
      components <- c(
        ifelse(components[1] %in% names(var.labels), var.labels[[components[1]]], components[1]),
        ifelse(components[2] %in% names(var.labels), var.labels[[components[2]]], components[2])
      )
      df$Variable[locs] <- paste(components, collapse = " by ")
    } else {
      labs <- df$Variable[locs]
      labs <- gsub(components[1], "", labs)
      labs <- gsub(components[2], "", labs)
      labs <- gsub(":", ", ", labs)
      df$Variable[locs] <- paste(spaces, labs, sep = "")
      newrow <- matrix("", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
      components <- c(
        ifelse(components[1] %in% names(var.labels), var.labels[[components[1]]], components[1]),
        ifelse(components[2] %in% names(var.labels), var.labels[[components[2]]], components[2])
      )
      newrow[1, 1] <- paste(components, collapse = " by ")
      df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrow, df[locs[1]: nrow(df), ])
    }
  }

  # Clean up polynomial terms
  polynomials <- grep("poly(", attr(fit$terms, "term.labels"), fixed = TRUE, value = TRUE)
  for (polynomial.ii in polynomials) {
    split.ii <- unlist(strsplit(polynomial.ii, split = ", "))
    varname.ii <- substring(split.ii[1], first = 6)
    poly.order <- as.numeric(split.ii[2])
    locs <- grep(polynomial.ii, df$Variable, fixed = TRUE)
    varname.ii <- ifelse(varname.ii %in% names(var.labels), var.labels[[varname.ii]], varname.ii)
    if (poly.order == 1) {
      df$Variable[locs] <- varname.ii
    } else if (poly.order == 2) {
      df$Variable[locs] <- c(varname.ii, paste(varname.ii, "squared"))
    } else if (poly.order == 3) {
      df$Variable[locs] <- c(varname.ii, paste(varname.ii, c("squared", "cubed")))
    } else {
      df$Variable[locs] <- c(varname.ii, paste(varname.ii, 2: poly.order, sep = "^"))
    }
  }

  # Add user-specified labels for numeric variables
  if (! is.null(var.labels)) {
    dataClasses <- attr(fit$terms, "dataClasses")
    numerics <- names(dataClasses[dataClasses == "numeric"])
    if (length(numerics) > 0) {
      for (varname.ii in numerics) {
        loc <- which(df$Variable == varname.ii)
        if (length(loc) == 1) {
          df$Variable[loc] <- ifelse(varname.ii %in% names(var.labels),
                                     var.labels[[varname.ii]], varname.ii)
        }
      }
    }
  }

  # Remove row names and return table
  rownames(df) <- NULL
  return(df %>% kable(escape = FALSE) %>% kable_styling(full_width = FALSE))

}
