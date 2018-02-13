#' Split Continuous Variable into Quantile Groups
#' 
#' Splits a continuous variable into quantiles groups. Basically combines 
#' \code{\link[stats]{quantile}} and \code{\link[base]{cut}} into a single 
#' function.
#' 
#' @param x Numeric vector.
#' @param groups Numeric value indicating how many quantile groups should be 
#' created.
#' @param ... Further arguments to pass to \code{\link[stats]{quantile}} or \code{\link{cut}}.
#' 
#' @return Factor variable.
#' 
#' @examples 
#' # Convert values from N(0, 1) into quintiles (i.e. 5 groups)
#' x <- rnorm(1000)
#' groups <- quant_groups(x, 5)
#' table(groups)
#' 
#' @export
quant_groups <- function(x, groups = 5, ...) {
  
  # Calculate quantiles
  quantiles <- quantile(x, probs = seq(0, 1, 1 / groups), na.rm = TRUE, ...)
  
  # Create quantile groups
  groups <- cut(x, breaks = quantiles, include.lowest = TRUE, ...)
  
  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ",
                paste(table(groups), collapse = ", "),
                ". ", num.missing, " missing.",
                sep = ""))
  return(groups)

}