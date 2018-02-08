# -Tests for bis_dl

context("bis_dl")
test_that("bis_dl works correctly with correct input", {
  id <- "0112038-9"
  langs <- c("FI", "SE", "EN")
  res <- map(langs, ~bis_dl(id, lang = .x))
  exp_cols <- c("name", "business_id", "company_form",
                "registration_date", "business_line", "business_line_code")
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

test_that("bis_dl handles http errors correctly", {
  expect_error(bis_dl("1111111-1"), regexp = "Query returned with an error")
})
