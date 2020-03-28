#' Create Summary Table for Fitted Generalized Estimating Equation Model
#'
#' Creates a table summarizing a GEE fit using the \code{\link[gee]{gee}}
#' function.
#'
#'
#' @param fit Fitted \code{\link[gee]{gee}} object.
#' @param data Data frame that served as 'data' in function call to
#' \code{\link[gee]{gee}}. Only needs to be specified if one or more of the
#' predictors is a factor and \code{factor.compression} is 1, 2, 3, or 4.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"beta"}, \code{"se"}, \code{"betaci"} for 95\% CI
#' for Beta, \code{"beta.se"} for Beta (SE), \code{"beta.ci"} for Beta
#' (95\% CI), \code{"or"}, \code{"orci"} for 95\% CI for OR, \code{"or.ci"} for
#' OR (95\% CI), \code{"hr"}, \code{"hrci"} for 95\% CI for HR, \code{"hr.ci"}
#' for HR (95\% CI), \code{"z"} for z statistic, and \code{"p"}. If OR's or HR's
#' are requested, the function will trust that exponentiated betas correspond to
#' these quantities.
#' @param robust Logical value for whether to use robust standard errors.
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
#' # Load in sample dataset and convert to long format
#' tabdata2 <- reshape(data = tabdata,
#'                     varying = c("bp.1", "bp.2", "bp.3", "highbp.1",
#'                                 "highbp.2", "highbp.3"),
#'                     timevar = "bp.visit", direction = "long")
#' tabdata2 <- tabdata2[order(tabdata2$id), ]
#'
#' # Blood pressure at 1, 2, and 3 months vs. age, sex, race, and treatment
#' library("gee")
#' fit <- gee(bp ~ Age + Sex + Race + Group, id = id, data = tabdata2,
#'            corstr = "unstructured")
#' tabgee(fit)
#'
#' # Can also use piping
#' fit %>% tabgee(data = tabdata2)
#'
#' # Same as previous, but with custom labels for Age and Race and factors
#' # displayed in slightly more compressed format
#' fit %>%
#'   tabgee(
#'     data = tabdata2,
#'     var.labels = list(Age = "Age (years)", Race = "Race/ethnicity"),
#'     factor.compression = 2
#'   )
#'
#' # GEE with some higher-order terms
#' # higher-order terms
#' fit <- gee(
#'   highbp ~ poly(Age, 2, raw = TRUE) + Sex + Race + Group + Race*Group,
#'   id = id,
#'   data = tabdata2,
#'   family = "binomial",
#'   corstr = "unstructured"
#' )
#' fit %>% tabgee(data = tabdata2)
#'
#'
#'@export
tabgee <- function(fit,
                   data = NULL,
                   columns = NULL,
                   robust = TRUE,
                   var.labels = NULL,
                   factor.compression = 1,
                   sep.char = ", ",
                   decimals = 2,
                   formatp.list = NULL) {

  # Error checking
  if (! "gee" %in% class(fit)) {
    stop("The input 'fit' must be a fitted 'gee'.")
  }
  if (! is.null(columns) &&
      ! all(columns %in% c("beta", "se", "betaci", "beta.se", "beta.ci", "or",
                           "orci", "or.ci", "hr", "hrci", "hr.ci", "z",
                           "p"))) {
    stop("Each element of 'columns' must be one of the following: 'beta', 'se', 'betaci', 'beta.se', 'beta.ci', 'or', 'orci', 'or.ci', 'hr', 'hrci', 'hr.ci', 'z', 'p'.")
  }
  if (! is.logical(robust)) {
    stop("The input 'robust' must be a logical.")
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
  betas <- coefmat[, "Estimate"]
  if (robust) {
    ses <- coefmat[, "Robust S.E."]
  } else {
    ses <- coefmat[, "Naive S.E."]
  }
  lower <- betas - qnorm(0.975) * ses
  upper <- betas + qnorm(0.975) * ses
  zs <- betas / ses
  ps <- pnorm(-abs(zs)) * 2
  intercept <- attr(fit$terms, "intercept") == 1

  # If columns unspecified, figure out reasonable defaults
  if (is.null(columns)) {

    gee.family <- fit$family$family
    gee.link <- fit$family$link
    if (gee.family == "binomial" & gee.link == "logit") {
      columns <- c("beta.se", "or.ci", "p")
    } else if (gee.family == "poisson" & gee.link == "log") {
      columns <- c("beta.se", "hr.ci", "p")
    } else {
      columns <- c("beta.se", "betaci", "p")
    }

  }

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

    } else if (column == "or") {

      df$`OR` <- sprintf(spf, exp(betas))
      if (intercept) df$`OR`[1] <- "-"

    } else if (column == "orci") {

      df$`95% CI` <- paste("(", sprintf(spf, exp(lower)), sep.char,
                           sprintf(spf, exp(upper)), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"

    } else if (column == "or.ci") {

      df$`OR (95% CI)` <- paste(sprintf(spf, exp(betas)), " (",
                                sprintf(spf, exp(lower)), sep.char,
                                sprintf(spf, exp(upper)), ")", sep = "")
      if (intercept) df$`OR (95% CI)`[1] <- "-"

    } else if (column == "hr") {

      df$`HR` <- sprintf(spf, exp(betas))

    } else if (column == "hrci") {

      df$`95% CI` <- paste("(", sprintf(spf, exp(lower)), sep.char,
                           sprintf(spf, exp(upper)), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"

    } else if (column == "hr.ci") {

      df$`HR (95% CI)` <- paste(sprintf(spf, exp(betas)), " (",
                                sprintf(spf, exp(lower)), sep.char,
                                sprintf(spf, exp(upper)), ")", sep = "")
      if (intercept) df$`HR (95% CI)`[1] <- "-"

    } else if (column == "z") {

      df$`z` <- sprintf(spf, zs)

    } else if (column == "p") {

      df$`P` <- do.call(formatp, c(list(p = ps), formatp.list))

    }

  }

  # Remove parentheses around intercept
  if (intercept) df$Variable[1] <- "Intercept"

  # If data is unspecified, use highest possible factor compression
  if (is.null(data)) {
    if (factor.compression != 5) {
      message("Changed 'factor.compression' from ", factor.compression, " to 5 because 'data' is unspecified.")
      factor.compression <- 5
    }
  }

  # Clean up factor variables
  spaces <- "&nbsp; &nbsp; &nbsp;"
  dataClasses <- attr(fit$terms, "dataClasses")
  factors <- names(dataClasses[dataClasses == "factor"])
  if (length(factors) > 0) {

    for (varname.ii in factors) {
      if (factor.compression == 1) {

        # Rows are Variable, Level 1 (ref), Level 2, ...
        levels.ii <- levels(data[, varname.ii])
        locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = spaces,
                                  x = df$Variable[locs], fixed = TRUE)
        newrows <- matrix("", nrow = 2, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrows[2, ] <- "-"
        newrows[1, 1] <- ifelse(varname.ii %in% names(var.labels), var.labels[[varname.ii]], varname.ii)
        newrows[2, 1] <- paste(spaces, paste(levels.ii[1], " (ref)", sep = ""), sep = "")
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrows, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 2) {

        # Rows are Variable (ref = Level 1), Level 2, ...
        levels.ii <- levels(data[, varname.ii])
        locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = spaces, x = df$Variable[locs])
        newrow <- matrix("", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrow[1, 1] <- paste(
          ifelse(varname.ii %in% names(var.labels), var.labels[[varname.ii]], varname.ii),
          " (ref = ", levels.ii[1], ")", sep = ""
        )
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrow, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 3) {

        # Rows are Level 1 (ref), Level 2, ...
        levels.ii <- levels(data[, varname.ii])
        locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
        df$Variable[locs] <- gsub(pattern = varname.ii, replacement = "", x = df$Variable[locs])
        newrow <- matrix("-", nrow = 1, ncol = ncol(df), dimnames = list(NULL, names(df)))
        newrow[1, 1] <- paste(levels.ii[1], " (ref)", sep = "")
        df <- rbind(df[setdiff(1: locs[1], locs[1]), ], newrow, df[locs[1]: nrow(df), ])

      } else if (factor.compression == 4) {

        # Rows are Level 2 (ref = Level 1), ...
        levels.ii <- levels(data[, varname.ii])
        locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
        df$Variable[locs] <- paste(
          gsub(pattern = varname.ii, replacement = "", x = df$Variable[locs]),
          " (ref = ", levels.ii[1], ")", sep = ""
        )

      } else if (factor.compression == 5) {

        # Rows are Level 2, ...
        levels.ii <- levels(data[, varname.ii])
        locs <- which(df$Variable %in% paste(varname.ii, levels.ii, sep = ""))
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
