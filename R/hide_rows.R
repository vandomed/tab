#' Hide rows of selected levels of dichotomous variables from a Table
#' (e.g. "no" category of Smoking)
#'
#' @param tab Data frame.
#' @param rows Character string with name of levels (rows) which should be hidden.
#'
#' @return Data frame which you can print in R (e.g. with \strong{xtable}'s
#' \code{\link[xtable]{xtable}} or \strong{knitr}'s \code{\link[knitr]{kable}}).
#'
#' @examples
#' # Compare age, sex, and race by treatment group
#' # Hide level "Male" of Variable "sex"
#' tabmulti(Age + Sex + Race ~ Group, data = tabdata) %>% hide_rows(rows = c("male")) %>% kable()
#'
#'
#' @export
hide_rows <- function(tab, rows, ...){

  rows <- paste("\\ \\ \\ ", tolower(rows), sep = "")
  tab <- tab[which(!tolower(tab$Variable) %in% rows),]

  return(tab)

}
