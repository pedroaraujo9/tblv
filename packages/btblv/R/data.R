#' Mortality rates from the Human Mortality Database (HMD)
#'
#' Data with age-specific mortality rates and probability of death for
#' different countries over time from the human mortality database
#'
#' @format data.frame `lf`
#' A data frame with 23088 rows and 8 columns:
#' \describe{
#'   \item{year}{Country name}
#'   \item{age}{age group x for [x, x+5)}
#'   \item{mx}{central mortality rate}
#'   \item{qx}{probability of death}
#'   \item{openinterval}{bool indicating with the age group interval is +inf open}
#'   \item{country_code}{country code}
#'   \item{country}{country name}
#'   \item{date_extract}{date of the extraction from HMD}
#' }
#' @source <https://www.mortality.org/>
"lf"
