extract_business_lines <- function(res, lang) {
  bl <- keep(res$businessLines, ~.x$language == lang)

  if (length(bl) == 0L) {
    list(name = NA_character_, code = NA_character_)
  } else {
    # take the newest in the case of multiple options
    bl_dates <- map_chr(bl, "registrationDate")
    newest_ind <- which(bl_dates == max(bl_dates))[1]
    bl[[newest_ind]]
  }

}

extract_liquidations <- function(res, lang) {

  liq <- keep(res$liquidations, ~.x$language == lang)

  if (length(liq) == 0L) {
    list(description = NA_character_,
         registrationDate = NA_character_,
         endDate = NA_character_)
  } else {
    # take the newest in the case of multiple options
    liq_dates <- map_chr(liq, "registrationDate")
    newest_ind <- which(liq_dates == max(liq_dates))[1]
    liq[[newest_ind]]
  }
}

check_input <- function(id, lang) {
  if (length(id) != 1 || is.na(id) || !str_detect(id, "^\\d{7}-\\d$")) {
    stop("'id' must be a character vector of length one with the format '1234567-8'.")
  }
  if (length(lang) > 1 || !(lang %in% c("FI", "SE", "EN"))) {
    stop("'lang' must be one of 'FI', 'SE', 'EN'.")
  }
}

handle_request_errors <- function(req) {
  if (http_error(req))
    str_c("\nQuery returned with an error, see the message below:\n",
          rawToChar(req$content)) %>% stop()
}
