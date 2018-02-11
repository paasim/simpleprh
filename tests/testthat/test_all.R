# -Tests for bis_dl

context("bis_dl")
test_that("bis_dl works correctly with correct input", {
  id <- "0112038-9"
  langs <- c("FI", "SE", "EN")
  res <- map(langs, ~bis_dl(id, lang = .x))
  exp_cols <- c("name", "business_id", "company_form",
                "registration_date", "business_line", "business_line_code",
                "liquidation_descr", "liquidation_registration", "liquidation_end")
  walk(res, ~{
    expect_true("tbl_df" %in% class(.x))
    expect_true(nrow(.x) == 1)

    expect_identical(colnames(.x), exp_cols)
    expect_identical(id, .x$business_id)
  })

  expect_true(res[[1]]$business_line != res[[3]]$business_line)
  expect_true(res[[1]]$business_line != res[[2]]$business_line)
  expect_true(res[[2]]$business_line != res[[3]]$business_line)
  expect_true(res[[1]]$business_line_code == res[[3]]$business_line_code)
  expect_true(res[[2]]$business_line_code == res[[3]]$business_line_code)
})

test_that("bis_dl validates the input correctly", {
  id_incorrect <- list(NA, "1", "12345678", "1234567-a", c("0112038-9", "0112038-9"))
  walk(id_incorrect, ~expect_error(bis_dl(.x), regexp = "'id' must be a character"))

  lang_incorrect <- c(NA, "Finnish", "ES")
  walk(lang_incorrect, ~expect_error(bis_dl("0112038-9", lang = .x),
                                     regexp = "'lang' must be"))
})

test_that("extract_business_line works as expected", {
  # list of some examples:
  #  "0108716-0" "0109015-9" "0111282-1" "0112217-3" "0112915-6" "1789039-6"
  res_bl <- list(businessLines = list(
    list(language = "SE", registrationDate = "3", name = "error", code = "error"),
    list(language = "FI", registrationDate = "1", name = "error", code = "error"),
    list(language = "FI", registrationDate = "2", name = "name", code = "code")))
  res_nobl <- list()
  exp_fields <- c("name", "code")

  nobl <- extract_business_lines(res_nobl, "FI")
  expect_true(all(exp_fields %in% names(nobl)))
  val_nobl <- nobl[exp_fields]
  expect_true(all(map_chr(val_nobl, class) == "character"))
  expect_true(all(is.na(val_nobl)))

  bl <- extract_business_lines(res_bl, "FI")
  expect_true(all(exp_fields %in% names(bl)))
  val_bl <- bl[exp_fields]
  expect_true(all(map_chr(val_bl, class) == "character"))
  expect_true(all(!is.na(val_bl)))
})

test_that("extract_liquidations works as expected", {
  # list of some examples
  #  "0576687-3" "2653034-1" "1852002-0" "2695013-5"
  res_noliq <- list()
  res_liq <- list(liquidations = list(
    list(language = "SE", registrationDate = "3", description = "error", endDate = "error"),
    list(language = "FI", registrationDate = "1", description = "error", endDate = "error"),
    list(language = "FI", registrationDate = "2", description = "descr", endDate = NA_character_)))
  exp_fields <- c("description", "registrationDate", "endDate")

  noliq <- extract_liquidations(res_noliq, "FI")
  expect_true(all(exp_fields %in% names(noliq)))
  val_noliq <- noliq[exp_fields]
  expect_true(all(map_chr(val_noliq, class) == "character"))
  expect_true(all(is.na(val_noliq)))

  liq <- extract_liquidations(res_liq, "FI")
  expect_true(all(exp_fields %in% names(liq)))
  val_liq <- liq[exp_fields]
  expect_true(all(map_chr(val_liq, class) == "character"))
  expect_true(any(!is.na(val_liq)))
})

test_that("bis_dl handles http errors correctly", {
  expect_error(bis_dl("1111111-1"), regexp = "Query returned with an error")
})
