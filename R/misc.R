list_elem_to_tibble <- function(e) {
  # this is to transform NULLs to NAs
  map(e, ~map_if(.x, ~length(.x) == 0L, ~NA_character_)) %>%
    transpose() %>%
    map(as_vector) %>%
    as_tibble() %>%
    names_to_snake_case() %>%
    mutate_at(vars(matches("_date")), ymd)
}

lang_filter <- function(df, excl) {
  if ("language" %in% colnames(df)) df[!(df$language %in% excl), ] else df
}


snake_case <- function(x) gsub("([a-z])([A-Z])", "\\1_\\2", x) %>% tolower()
names_to_snake_case <- function(x) set_names(x, snake_case(names(x)))

bis_dl_check_input <- function(id, excl_lang, return_type) {
  if (!is_scalar_character(id) || !str_detect(id, "^\\d{7}-\\d$") || is.na(id))
    stop("'id' must be a character vector of length one with the format '1234567-8'.")

  if (!is_character(excl_lang)) stop("'excl_lang' must be a character vector.")
  if (!is_scalar_character(return_type) || !(return_type %in% c("tbl", "lst")))
    stop("'return_type' must be either 'tbl' or 'lst'")
}

bis_lookup_check_input <- function(name, max_results, results_from) {
  if (!is_scalar_character(name) || is.na(name))
    stop("'name' must be a character vector of length one.")

  if ((!is_scalar_double(max_results) & !is_scalar_integer(max_results)) ||
      is.na(max_results) || max_results < 1L)
    stop("'max_results' must be a number between 1 and 1000.")

  if ((!is_scalar_double(results_from) & !is_scalar_integer(results_from)) ||
      is.na(results_from) || results_from < 0L)
    stop("'results_from' must be a non-negative number.")
}

handle_request_errors <- function(req) {
  if (http_error(req)) content(req, "text", "application/json", "UTF-8") %>% stop()
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("simpleprh")
  packageStartupMessage("This is simpleprh version ", ver)
}
