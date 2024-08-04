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
#' data("lf")
#' data = create_btblv_data(lf, "mx", "age", "country", "year")
#' data
#'
print.btblv_data = function(x, ...) {
  N = btblv_data$data_list_stan$N
  J = btblv_data$data_list_stan$J
  n = btblv_data$data_list_stan$n
  Ng = btblv_data$data_list_stan$Ng

  cat(paste0(
    "Total number of observations: ", N, "\n",
    "Number of items: ", J, "\n",
    "Number of groups: ", Ng, "\n",
    "Number of individuals (a group at a time point): ", n, "\n"
  ))
}
