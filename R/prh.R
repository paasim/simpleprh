#' Download data from the PRH open data API
#'
#' Download information related to the given Business ID from the
#' Business Information System API. The language of the result can be set
#' with the \code{lang}-parameter.
#'
#' @param id The business ID of the company. Must be of the format 1111111-1,
#'   ie. seven digits followed by a dash and a checksum digit.
#' @param lang The language of the results. Defaults to 'FI' and must be either
#' 'FI', 'SE' or 'EN'.
#'
#' @return A tibble with one row and the columns \code{name},
#'  \code{business_id}, \code{company_form}, \code{registration_date},
#'  \code{business_line} and \code{business_line_code}. For information about
#'  the business line classification, see
#'  \url{http://www.stat.fi/meta/luokitukset/toimiala/001-2008/index_en.html}.
#'
#' @examples
#' \donttest{
#' library(simpleprh)
#' # Download information related to Nokia Oyj
#' bis_dl("0112038-9", lang = "EN")
#' }
#'
#' @export
#'
bis_dl <- function(id, lang = "FI") {
  # check that the input variables are valid
  check_input(id, lang)

  # request information related to the given business id.
  req <- str_c("https://avoindata.prh.fi/bis/v1/", id) %>% GET()
  handle_request_errors(req)

  # transfrom the results into a list
  res <- rawToChar(req$content) %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("results", 1)

  # extract the business line information in the given language
  business_lines <- keep(res$businessLines, ~.x$language == lang)

  # result in a tibble
  tibble(name = res$name,
         business_id = res$businessId,
         company_form = res$companyForm,
         registration_date = ymd(res$registrationDate),
         business_line = business_lines[[1]]$name,
         business_line_code = business_lines[[1]]$code)
}
