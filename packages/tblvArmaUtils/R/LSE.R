#' Sum of the log of the exponentials
#'
#' Comutes sum(log(exp(x)))
#'
#' @param x numeric vector.
#'
#' @return numeric value with sum(log(exp(x)))
#' @export
#' 
#' @examples
#' LSE(c(10, 20, 30))
LSE <- function(x) {
  .Call(`_tblvArmaUtils_LSE`, x)
}