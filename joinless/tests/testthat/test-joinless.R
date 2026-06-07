#' @examples
#' df1 <- data.frame(id = 1:5, value = 1:5)
#' df2 <- data.frame(id = 3:7, value = 3:7)
#' joinless(df1, df2, x_vars = "id", y_vars = "id")
test_that("joinless works on simple data", {
  x <- data.frame(
    id    = 1:5,
    value = c(1, 2, 3, 4, 5)
  )

  y <- data.frame(
    id    = 3:7,
    value = c(3, 4, 99, 100, 101)
  )

  res <- joinless(x, y)

  # adjust these expectations to your real output:
  expect_false(is.null(res))
  expect_true(is.data.frame(res) || tibble::is_tibble(res))
  expect_gt(nrow(res), 0)
})


test_that("joinless respects ignore argument", {
  x <- data.frame(
    id    = 1:5,
    value = 1:5,
    extra = letters[1:5]
  )

  y <- data.frame(
    id    = 1:5,
    value = 1:5,
    extra = letters[1:5]
  )

  expect_no_error(res_all <- joinless(x, y))
  expect_no_error(res_ign <- joinless(x, y, ignore = "extra"))
  expect_no_error(joinless(x, y, ignore = "nonexistent_column"))

  expect_true(is.data.frame(res_all) || tibble::is_tibble(res_all))
  expect_true(is.data.frame(res_ign) || tibble::is_tibble(res_ign))
})


test_that("joinless handles missingness_tol argument", {
  x <- data.frame(id = 1:3, value = 1:3)
  y <- data.frame(id = 1:3, value = 1:3)

  expect_no_error(joinless(x, y, missingness_tol = 0))
  expect_no_error(joinless(x, y, missingness_tol = 0.1))
  expect_no_error(joinless(x, y, missingness_tol = 1))
})


test_that("joinless handles missing values within tolerance", {
  x <- data.frame(
    id    = 1:5,
    value = c(1, NA, 3, NA, 5)
  )

  y <- data.frame(
    id    = 1:5,
    value = c(1, 2, 3, 4, 5)
  )

  # should NOT error if tolerance is high enough
  expect_no_error(joinless(x, y, missingness_tol = 0.5))
})
