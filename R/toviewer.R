#' Output a Table to the RStudio Viewer
#'
#' Does some basic formatting and then calls \code{\link[knitr]{kable}} and
#' \code{\link[kableExtra]{kable_styling}} to print table to Viewer.
#'
#' @param x Character matrix
#'
#' @export
toviewer <- function(x) {

  x$Variable <- gsub("   ", "&nbsp; &nbsp; &nbsp;", x$Variable, fixed = TRUE)
  x %>% kable(row.names = FALSE, escape = FALSE) %>% kable_styling(full_width = FALSE)

}
