list_elem_to_tibble <- function(e, excl_langs) {
  # this is to transform NULLs to NAs
  res <- map(e, ~map_if(.x, ~length(.x) == 0L, ~NA_character_)) %>%
    transpose() %>%
    map(as_vector) %>%
    as_tibble() %>%
    names_to_snake_case() %>%
    mutate_at(vars(matches("_date")), ymd)

  if ("language" %in% colnames(res)) {
    res[!(res$language %in% excl_langs), ]
  } else {
    res
  }
}

snake_case <- function(x) gsub("([a-z])([A-Z])", "\\1_\\2", x) %>% tolower()
names_to_snake_case <- function(x) set_names(x, snake_case(names(x)))

check_input <- function(id, excl_lang, return_type) {
  if (!is_character(id, 1L) || is.na(id) || !str_detect(id, "^\\d{7}-\\d$"))
    stop("'id' must be a character vector of length one with the format '1234567-8'.")

  if (!is_character(excl_lang)) stop("'excl_lang' must be a character vector.")
  if (!is_character(return_type, 1) || !(return_type %in% c("tbl", "lst")))
    stop("'return_type' must be either 'tbl' or 'lst'")
}

handle_request_errors <- function(req) {
  if (http_error(req))
    str_c("\nQuery returned with an error, see the result below:\n",
          rawToChar(req$content)) %>% stop()
}
