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
#' lf = hmd_data$life_tables_5x5 |>
#'  dplyr::filter(year %in% seq(1990, 2020, 5))

#' data = create_btblv_data(df = lf,
#'                          resp_col_name = "mx",
#'                          item_col_name = "age",
#'                          group_col_name = "country",
#'                          time_col_name = "year")
#'
#'
create_btblv_data = function(df,
                             resp_col_name,
                             item_col_name,
                             group_col_name,
                             time_col_name) {

  assertthat::assert_that(
    "data.frame" %in% base::class(df),
    msg = "`df` should be a data.frame"
  )

  assertthat::assert_that(
    {
      spaces = base::as.data.frame(df)[, time_col_name] %>%
        base::unique() %>%
        base::sort() %>%
        base::diff() %>%
        base::unique()

      base::length(spaces) == 1
    },
    msg = "time points should be equally spaced for each group of observations."

  )

  assertthat::assert_that(
    (base::class(resp_col_name) == "character") & (base::length(resp_col_name) == 1),
    msg = "`resp_col_name` should be a single character"
  )

  assertthat::assert_that(
    (base::class(item_col_name) == "character") & (base::length(item_col_name) == 1),
    msg = "`item_col_name` should be a single character"
  )

  assertthat::assert_that(
    (base::class(group_col_name) == "character") & (base::length(group_col_name) == 1),
    msg = "`group_col_name` should be a single character"
  )

  assertthat::assert_that(
    (base::class(time_col_name) == "character") & (base::length(time_col_name) == 1),
    msg = "`time_col_name` should be a single character"
  )

  columns = base::c(
    item_col_name, group_col_name, time_col_name, resp_col_name
  )

  # start object
  btblv_data = base::list()
  base::class(btblv_data) = "btblv_data"
  btblv_data$df = df

  df =  base::as.data.frame(df)
  df = df[, columns]
  base::names(df) = c("item", "group", "time", "y")
  data_df = df %>% tibble::as_tibble() %>% dplyr::arrange(group, time, item)

  # creates unique key for cohort group-time
  data_df$ind = base::paste0(data_df$group, "-", data_df$time)

  # unique values for the variables
  item = data_df$item %>% base::unique() %>% base::sort()
  ind = data_df$ind %>% base::unique() %>% base::sort()
  time = data_df$time %>% base::unique() %>% base::sort()
  group = data_df$group %>% base::unique() %>% base::sort()

  # numeric label label for each variable
  item_label = base::as.character(1:base::length(item))
  ind_label = base::as.character(1:base::length(ind))
  time_label = base::as.character(1:base::length(time))
  group_label = base::as.character(1:base::length(group))

  base::names(item_label) = item
  base::names(ind_label) = ind
  base::names(time_label) = time
  base::names(group_label) = group

  # add columns with the numerical labels
  data_df = data_df %>%
    dplyr::mutate(item_num = dplyr::recode_factor(item, !!!item_label) %>% base::as.integer(),
                  ind_num = dplyr::recode_factor(ind, !!!ind_label) %>% base::as.integer(),
                  time_num = dplyr::recode_factor(time, !!!time_label) %>% base::as.integer(),
                  group_num = dplyr::recode_factor(group, !!!group_label) %>% base::as.integer())

  # group and time
  ind_time_id = data_df %>%
    dplyr::select(ind, group, time, ind_num, time_num, group_num) %>%
    dplyr::distinct()

  # find the ind of the past cohort
  ind_time_id = ind_time_id %>%
    dplyr::left_join(
      ind_time_id %>%
        dplyr::mutate(time_num = time_num + 1) %>%
        dplyr::select(ind_num, time_num, group) %>%
        dplyr::rename(lag_ind = ind_num),
      by = c("group", "time_num")
    ) %>%
    dplyr::select(ind, ind_num, lag_ind, group, time, time_num, group_num)

  init_index = ind_time_id %>% dplyr::filter(base::is.na(lag_ind))
  past_index = ind_time_id %>% dplyr::filter(!base::is.na(lag_ind))

  data_wide = data_df %>%
    dplyr::select(ind_num, group_num, time_num, item_num, y) %>%
    tidyr::spread(item_num, y) %>%
    dplyr::arrange(ind_num)

  data_matrix = data_wide %>%
    dplyr::select(-(ind_num:time_num)) %>%
    base::as.matrix()

  data_list_stan = base::list(
    x = data_matrix,
    N = base::nrow(data_df),
    J = data_df$item %>% base::unique() %>% base::length(),
    n = data_df$ind %>% base::unique() %>% base::length(),
    Ng = data_df$group %>% base::unique() %>% base::length(),
    init_index = init_index$ind_num,
    past_index = past_index$lag_ind,
    current_index = past_index$ind_num,
    group_id = past_index$group_num
  )

  btblv_data$data = data_df
  btblv_data$data_wide = data_wide
  btblv_data$data_list_stan = data_list_stan
  btblv_data$columns = columns

  base::return(btblv_data)

}

