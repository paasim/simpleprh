#' Download data from the PRH open data API
#'
#' Download information related to the given Business ID from the
#' Business Information System API. The language of the result can be set
#' with the \code{lang}-parameter.
#'
#' @param id The business ID of the company. Must be of the format 1111111-1,
#'   ie. seven digits followed by a dash and a checksum digit.
#' @param excl_langs A character vector of the languages to be excluded from
#'  the results. By default, some of the fields return the same information in
#'  Finnish, Swedish and English. The value can therefore be for example
#'  \code{c("SE", "EN")} to only return the results in Finnish or \code{"FI"}
#'  to only return the results in English. Defaults to \code{character(0)}, ie.
#'  the results are returned in all available languages.
#' @param return_type The return type. Either \code{"tbl"} for returning a one
#'  row tibble with columns as the fields, or \code{"list"} for returning a
#'  a list. A list is probably simpler for accessing a single result while a
#'  tibble might be more convenient when combining multiple results. Defaults
#'  to \code{"tbl"}.
#'
#' @return A tibble with one row where each column is a field returned by the
#'  API or a list with the elemnents corresponding to the fields. All
#'  \code{NULL}s are converted to \code{NA}s. For more information about the
#'  API, see \url{http://avoindata.prh.fi/ytj_en.html}. For information about
#'  the business line classification, see
#'  \url{http://www.stat.fi/meta/luokitukset/toimiala/001-2008/index_en.html}.
#'
#' @examples
#' \donttest{
#' library(simpleprh)
#' # Download information related to Nokia Oyj
#' bis_dl("0112038-9")
#' }
#'
#' @export
#'
bis_dl <- function(id, excl_langs = character(0), return_type = "tbl") {
  # check that the input variables are valid
  bis_dl_check_input(id, excl_langs, return_type)

  # request information related to the given business id.
  req <- GET("https://avoindata.prh.fi/", path = str_c("bis/v1/", id))
  handle_request_errors(req)

  # transfrom the results into a list
  res_lst <- content(req, "text", "application/json", "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("results", 1) %>%
    names_to_snake_case()

  # remove an unnecessary field
  res_lst$details_uri <- NULL
  res_lst$registration_date <- ymd(res_lst$registration_date %||% NA_character_)

  res <- modify_if(res_lst, is.list,
                   ~list_elem_to_tibble(.x) %>% lang_filter(excl_langs))
  # list_elem_to_tibble inside a list to make it suitable for a list-column
  if (return_type == "tbl") as_tibble(modify_if(res, is.tbl, list)) else res
}
