#' Find business information
#'
#' Query company business ID, registration date, form and correct name with name
#' as a lookup parameter.
#'
#' @param name The name, or par of the name of the company.
#' @param max_results The maximum number of results to be returned. Note, that
#'  the API limits this to \code{1000}.
#' @param results_from The starting index for the results. For example, if a
#'  certain query returned more than \code{1000} results, having this as
#'  \code{0} would return the first \code{1000} results and then setting this
#'  to \code{1000} would return the next \code{1000}.
#'
#' @return A tibble with the fields \code{name}, \code{business_id},
#' \code{registration_date}, \code{company_form} that match the query.
#'
#' @examples
#' \donttest{
#' library(simpleprh)
#' # Download information related to Nokia Oyj
#' bis_lookup("Nokia Oyj")
#' }
#'
#' @export
#'
bis_lookup <- function(name, max_results = 1000L, results_from = 0L) {
  # check that the input variables are valid
  bis_lookup_check_input(name, max_results, results_from)
  max_results <- as.integer(max_results)
  results_from <- as.integer(results_from)

  q <- list(maxResults = max_results, resultsFrom = results_from, name = name)
  # request information related to the given business id.
  req <- GET("https://avoindata.prh.fi/", path = "bis/v1", query = q)
  handle_request_errors(req)

  # transfrom the results into a list
  content(req, "text", "application/json", "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("results") %>%
    map(~.x[c("name", "businessId", "registrationDate", "companyForm")]) %>%
    list_elem_to_tibble()
}