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
#' @importFrom dplyr filter is.tbl matches mutate_at vars
#' @importFrom httr content GET http_error
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom magrittr "%>%"
#' @importFrom purrr as_vector is_bare_list is_character keep map map_chr
#'  map_if map_lgl map2_lgl modify_if pluck set_names transpose walk "%||%"
#' @importFrom stringr str_c str_detect
#' @importFrom tibble as_tibble tibble
NULL
