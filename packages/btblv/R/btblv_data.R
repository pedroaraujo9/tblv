#' Create object from the class btblv_data
#'
#' Create object with the data to be used in `btblv_fit`
#'
#' @param df `data.frame` with at least `resp_column`, `item_column`, `group_column`, `time_column`.
#' @param resp_col_name string with the name of the column with the (0, 1) data.
#' @param item_col_name string with the name of the column with the items.
#' @param group_col_name string with the name of the columns with the groups.
#' @param time_col_name string with the name of the columns with the time.
#'
#' @return btblv_data object.
#' @export
#'
#' @examples
#' data("hmad_data")
#' data = create_btblv_data(df = hmd_data$life_tables_5x5,
#'                          resp_col_name = "mx",
#'                          item_col_name = "age",
#'                          group_col_name = "country",
#'                          time_col_name = "year")
#'
create_btblv_data = function(df,
                             resp_col_name,
                             item_col_name,
                             group_col_name,
                             time_col_name) {

  assertthat::assert_that(
    "data.frame" %in% class(df),
    msg = "`df` should be a data.frame"
  )

  assertthat::assert_that(
    {
      spaces = as.data.frame(df)[, time_col_name] %>%
        unique() %>%
        sort() %>%
        diff() %>%
        unique()

      length(spaces) == 1
    },
    msg = "time points should be equally spaced for each group of observations."

  )

  assertthat::assert_that(
    (class(resp_col_name) == "character") & (length(resp_col_name) == 1),
    msg = "`resp_col_name` should be a single character"
  )

  assertthat::assert_that(
    (class(item_col_name) == "character") & (length(item_col_name) == 1),
    msg = "`item_col_name` should be a single character"
  )

  assertthat::assert_that(
    (class(group_col_name) == "character") & (length(group_col_name) == 1),
    msg = "`group_col_name` should be a single character"
  )

  assertthat::assert_that(
    (class(time_col_name) == "character") & (length(time_col_name) == 1),
    msg = "`time_col_name` should be a single character"
  )

  columns = c(
    item_col_name, group_col_name, time_col_name, resp_col_name
  )

  df = as.data.frame(df)
  df = df[, columns]
  names(df) = c("item", "group", "time", "y")
  data_df = df %>% as_tibble() %>% arrange(group, time, item)

  # creates unique key for cohort group-time
  data_df$ind = paste0(data_df$group, "-", data_df$time)

  # unique values for the variables
  item = data_df$item %>% unique() %>% sort()
  ind = data_df$ind %>% unique() %>% sort()
  time = data_df$time %>% sort() %>% unique()
  group = data_df$group %>% unique() %>% sort()

  # numeric label label for each variable
  item_label = as.character(1:length(item))
  ind_label = as.character(1:length(ind))
  time_label = as.character(1:length(time))
  group_label = as.character(1:length(group))

  names(item_label) = item
  names(ind_label) = ind
  names(time_label) = time
  names(group_label) = group

  # add columns with the numerical labels
  data_df = data_df %>%
    dplyr::mutate(item_num = recode_factor(item, !!!item_label) %>% as.integer(),
                  ind_num = recode_factor(ind, !!!ind_label) %>% as.integer(),
                  time_num = recode_factor(time, !!!time_label) %>% as.integer(),
                  group_num = recode_factor(group, !!!group_label) %>% as.integer())

  # group and time
  ind_time_id = data_df %>%
    dplyr::select(ind, group, time, ind_num, time_num, group_num) %>%
    dplyr::distinct()

  # find the ind of the past cohort
  ind_time_id = ind_time_id %>%
    dplyr::left_join(
      ind_time_id %>%
        mutate(time_num = time_num + 1) %>%
        select(ind_num, time_num, group) %>%
        rename(lag_ind = ind_num),
      by = c("group", "time_num")
    ) %>%
    dplyr::select(ind, ind_num, lag_ind, group, time, time_num, group_num)

  init_index = ind_time_id %>% filter(is.na(lag_ind))
  past_index = ind_time_id %>% filter(!is.na(lag_ind))

  data_wide = data_df %>%
    dplyr::select(ind, item, y) %>%
    tidyr::spread(item, y)

  data_matrix = data_wide %>% dplyr::select(-ind) %>% as.matrix()

  data_list_stan = list(
    x = data_matrix,
    N = nrow(data_df),
    J = data_df$item %>% unique() %>% length(),
    n = data_df$ind %>% unique() %>% length(),
    Ng = data_df$group %>% unique() %>% length(),
    init_index = init_index$ind_num,
    past_index = past_index$lag_ind,
    current_index = past_index$ind_num,
    group_id = past_index$group_num
  )

  btblv_data = list(
    data = data_df,
    data_wide = data_wide,
    data_list_stan = data_list_stan,
    original_data = df,
    columns = columns
  )

  class(btblv_data) = "btblv_data"
  return(btblv_data)

}

