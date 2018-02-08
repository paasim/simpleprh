#' SIMPLEPRH
#'
#' An R package for accessing the PRH open data API. Currently the
#' package supports obtaining the company name, form, business line and
#' registration date via the Business Information System API although
#' more functionality might be added in the future.
#'
#' @docType package
#' @name simpleytj
#'
#' @importFrom httr GET http_error
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom magrittr "%>%"
#' @importFrom purrr keep map pluck walk
#' @importFrom stringr str_c str_detect
#' @importFrom tibble tibble
NULL
