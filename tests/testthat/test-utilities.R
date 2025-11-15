test_that("se computes standard error", {
  x <- 1:5
  expect_equal(se(x), stats::sd(x) / sqrt(length(x)))
})

test_that("se handles na.rm", {
  x <- c(1, 2, 3, NA, 4, 5)
  expect_equal(se(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
})

test_that("calc_error uses sd by default", {
  x <- 1:5
  expect_equal(calc_error(x), stats::sd(x))
})

test_that("calc_error uses se when specified", {
  x <- 1:5
  expect_equal(calc_error(x, fun.errorbar = "se"), se(x))
})

test_that("create_errorbarb allows zero width and height", {
  errorbar <- create_errorbarb(x = 0, y = 0, height = 0, width = 0, errorbar_tip_size = 1)
  expect_s3_class(errorbar, "data.frame")
  expect_equal(nrow(errorbar), 8)
})
