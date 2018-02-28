# # -Tests for bis_dl

context("bis_dl")
test_that("bis_dl works as expected.", {
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
  expect_identical(unname(map_chr(res, class)), exp_classes)

  res_lst <- bis_dl(id, return_type = "lst")

  expect_true(is_bare_list(res_lst))
  # for some weird reason, this is the way to untibble that works.
  res_unlisted <- c(res[!map_lgl(res, is.list)],
                    unlist(res[map_lgl(res, is.list)], recursive = FALSE))
  expect_identical(names(res_lst), names(res_unlisted))
  expect_true(all(map2_lgl(res_lst, res_unlisted, identical)))
})

context("bis_lookup")
test_that("bis_lookup works as expected.", {
  name <- "Nokia"
  res1 <- bis_lookup(name, max_results = 1L)
  res5 <- bis_lookup(name, max_results = 5L)
  res51 <- bis_lookup(name, max_results = 5L, results_from = 1L)

  exp_cols <- c("name","business_id", "registration_date", "company_form")
  exp_classes <- c(rep("character", 2), "Date", "character")
  walk(list(res1, res5, res51), ~{
    expect_true("tbl_df" %in% class(.x))
    expect_identical(colnames(.x), exp_cols)
    expect_identical(unname(map_chr(.x, class)), exp_classes)
  })

  # max_results has an effect on the output
  expect_true(nrow(res1) == 1L)
  expect_true(nrow(res5) == 5L)
  expect_true(nrow(res51) == 5L)

  # results_from has an effect on the output
  expect_identical(slice(res5, 2:5), slice(res51, 1:4))
  expect_true(res5$business_id[1] != res51$business_id[5])

})

context("misc")
test_that("list_elem_to_tibble works as expected.", {
  e1 <- list(list(a = 0L, b_date = "1902-01-01", c = "2", language = "FI"),
             list(a = 0L, b_date = "1903-01-01", c = NULL, language = "SE"),
             list(a = 0L, b_date = "1904-01-01", c = NULL, language = NA_character_))
  res1 <- list_elem_to_tibble(e1)
  expect_true(is.tbl(res1))
  expect_true(all(c("a", "b_date", "c") %in% colnames(res1)))
  exp_class <- c("integer", "Date", "character")
  expect_true(all(map_chr(res1[c("a", "b_date", "c")], class) == exp_class))
  expect_true(nrow(res1) == 3L)
})

test_that("lang_filter works as expected.", {
  tbl1 <- tibble(a = c(0L, 1L, NA_integer_),
                 language = c("FI", "SE", "SE"))
  res1 <- lang_filter(tbl1, excl = c("SE", "FI"))
  res2 <- lang_filter(tbl1, excl = "FI")
  res3 <- lang_filter(tbl1, excl = NA_character_)
  res4 <- lang_filter(tbl1, excl = character(0))
  walk(list(res1, res2, res3, res4), ~{
    expect_true(is.tbl(.x))
    expect_true(all(c("a", "language") %in% colnames(.x)))
    exp_class <- c("integer", "character")
    expect_true(all(map_chr(.x[c("a", "language")], class) == exp_class))
  })
  expect_true(nrow(res1) == 0L)
  expect_true(nrow(res2) == 2L)
  expect_true(nrow(res3) == 3L)
  expect_true(nrow(res4) == 3L)
})


test_that("input validation works correctly", {
  id_incorrect <- list(NA, "1", "12345678", "1234567-a", c("0112038-9", "0112038-9"))
  walk(id_incorrect, ~expect_error(bis_dl(.x), regexp = "'id' must be"))

  excl_lang_incorrect <- list(11, list("Finnish"))
  walk(excl_lang_incorrect, ~expect_error(bis_dl("0112038-9", excl_langs = .x),
                                     regexp = "'excl_lang' must be"))
  ret_type_incorrect <- list(rnorm(1), "tibble", NA)
  walk(ret_type_incorrect, ~expect_error(bis_dl("0112038-9", return_type = .x),
                                         regexp = "'return_type' must be"))

  name_incorrect <- list(c("0112038-9", "0112038-9"), NA, 15)
  walk(name_incorrect, ~expect_error(bis_lookup(.x), regexp = "'name' must be"))

  max_results_incorrect <- list(0, list("Finnish"), NaN, c(17, 13))
  walk(max_results_incorrect, ~expect_error(bis_lookup("test", max_results = .x),
                                            regexp = "'max_results' must be"))

  results_from_incorrect <- list(-17, "tibble", NA, c(13, 16))
  walk(results_from_incorrect, ~expect_error(bis_lookup("test", results_from = .x),
                                             regexp = "'results_from' must be"))
})

test_that("bis_dl handles zero results correctly.", {
  expect_error(bis_dl("1111111-1"), regexp = "totalResults\":-1")
})

test_that("bis_lookup handles zero results correctly.", {
  expect_error(bis_lookup("1111111-1"), regexp = "totalResults\":-1")
})
