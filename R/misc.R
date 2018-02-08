handle_request_errors <- function(req) {
  if (http_error(req))
    str_c("\nQuery returned with an error, see the message below:\n",
          rawToChar(req$content)) %>% stop()
}

check_input <- function(id, lang) {
  if (length(id) != 1 || is.na(id) || !str_detect(id, "^\\d{7}-\\d$")) {
    stop("'id' must be a character vector of length one with the format '1234567-8'.")
  }
  if (length(lang) > 1 || !(lang %in% c("FI", "SE", "EN"))) {
    stop("'lang' must be one of 'FI', 'SE', 'EN'.")
  }
}
