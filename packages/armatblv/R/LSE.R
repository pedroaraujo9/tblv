#' Log of the sum of exponentials
#'
#' @param x numeric vector
#'
#' @return numeric value with log(sum(exp(x)))
#' @export
#'
#' @examples
#' LSE(c(1, 10, 2, 4)))
LSE = function(x) {
  .Call(`_armatblv_LSE`, x)
}
