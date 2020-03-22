#' Print a GLM Summary Table to the RStudio Viewer
#'
#' You can call this function as you would \code{\link[stats]{glm}} or pass a
#' previously fitted \code{\link[stats]{glm}} object. Either way, the result is
#' a summary table printed to the Viewer.
#'
#' @param ... Arguments to pass to glm.
#'
#' @return kable
#'
#' @examples
#' # Fit and view
#' glm_v(death_1yr ~ Age + Sex + Race, data = tabdata, family = "binomial")
#'
#' # Fit then view
#' fit <- glm(death_1yr ~ Age + Sex + Race, data = tabdata, family = "binomial")
#' glm_v(fit)
#'
#' # Piping is OMG so cool Hashtag HexStickerz
#' fit %>% glm_v()
#'
#' @export
glm_v <- function(...) {

  # Fit GLM if necessary
  arguments <- list(...)
  if ("formula" %in% sapply(arguments, class)) {
    fit <- glm(...)
  } else {
    fit <- as.list(...)
  }
  return(tabglm(fit))
}
