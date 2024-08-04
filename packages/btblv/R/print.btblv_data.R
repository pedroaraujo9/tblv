#' Print method for `btblv_data` object
#'
#' Displays information about the the `btblv_data` object.
#'
#' @param x `btblv_data` object generate by `btblv::btblv_data`.
#' @param ... extra arguments.
#'
#' @return message.
#' @export
#'
#' @examples
#' #
#'
print.btblv_data = function(x, ...) {
  N = x$data_list_stan$N
  J = x$data_list_stan$J
  n = x$data_list_stan$n
  Ng = x$data_list_stan$Ng

  cat(paste0(
    "Total number of observations: ", N, "\n",
    "Number of items: ", J, "\n",
    "Number of groups: ", Ng, "\n",
    "Number of individuals (a group at a time point): ", n, "\n"
  ))
}
