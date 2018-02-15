# # -Tests for bis_dl

context("bis_dl")
test_that("bis_dl works correctly with return_type = 'list'", {
  id <- "0112038-9"
  res <- bis_dl(id, return_type = "tbl")
  exp_cols <- c(
    "business_id", "name", "registration_date", "company_form", "liquidations",
    "names", "auxiliary_names", "addresses", "company_forms", "business_lines",
    "languages", "registed_offices", "contact_details", "registered_entries",
    "business_id_changes")
  exp_classes <- c(rep("character", 2), "Date", "character", rep("list", 11))

  expect_true("tbl_df" %in% class(res))
  expect_true(nrow(res) == 1)

  expect_identical(colnames(res), exp_cols)
  expect_true(all(map_chr(res, class) == exp_classes))

  res_lst <- bis_dl(id, return_type = "lst")

  expect_true(is_bare_list(res_lst))
  # for some weird reason, this is the way to untibble that works.
  res_unlisted <- c(res[!map_lgl(res, is.list)],
                    unlist(res[map_lgl(res, is.list)], recursive = FALSE))
  expect_identical(names(res_lst), names(res_unlisted))
  expect_true(all(map2_lgl(res_lst, res_unlisted, identical)))
})

context("misc")
test_that("list_elem_to_tibble works as expected", {
  e1 <- list(list(a = 5L, b_date = "1900-01-01", c = NULL),
            list(a = 10L, b_date = "1901-01-01", c = "2"))
  e2 <- list(list(a = 0L, b_date = "1902-01-01", c = "2", language = "FI"),
             list(a = 0L, b_date = "1903-01-01", c = NULL, language = "SE"),
             list(a = 0L, b_date = "1904-01-01", c = NULL, language = NA_character_))
  res1 <- list_elem_to_tibble(e1, excl_langs = character(0))
  res2 <- list_elem_to_tibble(e2, excl_langs = c("FI", "something"))
  res3 <- list_elem_to_tibble(e1, excl_langs = "SE")
  walk(list(res1, res2, res3), ~{
    expect_true(is.tbl(.x))
    expect_true(all(c("a", "b_date", "c") %in% colnames(.x)))
    exp_class <- c("integer", "Date", "character")
    expect_true(all(map_chr(.x[c("a", "b_date", "c")], class) == exp_class))
  })
  expect_true(nrow(res1) == 2L)
  expect_true(nrow(res2) == 2L)
  expect_true(nrow(res3) == 2L)
})

test_that("input validates works correctly", {
  id_incorrect <- list(NA, "1", "12345678", "1234567-a", c("0112038-9", "0112038-9"))
  walk(id_incorrect, ~expect_error(bis_dl(.x), regexp = "'id' must be a character"))

  excl_lang_incorrect <- list(11, list("Finnish"))
  walk(excl_lang_incorrect, ~expect_error(bis_dl("0112038-9", excl_langs = .x),
                                     regexp = "'excl_lang' must be"))
  ret_type_incorrect <- list(rnorm(1), "tibble", NA)
  walk(ret_type_incorrect, ~expect_error(bis_dl("0112038-9", return_type = .x),
                                         regexp = "'return_type' must be"))
})

test_that("bis_dl handles zero results correctly.", {
  expect_error(bis_dl("1111111-1"), regexp = "Query returned with an error")
})
