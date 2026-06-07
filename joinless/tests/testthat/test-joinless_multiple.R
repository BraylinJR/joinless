test_that("joinless_multiple rejects bad inputs", {
  x <- data.frame(id = 1:3, v = 1:3)
  y_good <- data.frame(id = 1:3, w = 1:3)

  expect_error(joinless::joinless_multiple("not df", ys = list(y_good)),
               "`x` must be a data.frame")

  expect_error(joinless::joinless_multiple(x, ys = "not list"),
               "`ys` must be a non-empty list of data.frames")

  expect_error(joinless::joinless_multiple(x, ys = list()),
               "`ys` must be a non-empty list of data.frames")

  expect_error(joinless::joinless_multiple(x, ys = list(y_good, "bad")),
               "All elements of `ys` must be data.frames")
})


test_that("joinless_multiple works on simple data and adds dataset column", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(id = 3:7, value = c(3, 4, 99, 100, 101))
  df3 <- data.frame(id = 5:9, value = c(5, 6, 7, 8, 9))

  res <- joinless::joinless_multiple(
    x = df1,
    ys = list(a = df2, b = df3)
  )

  expect_true(is.data.frame(res) || tibble::is_tibble(res))
  expect_true("dataset" %in% names(res))
  expect_setequal(unique(res$dataset), c("a", "b"))
})


test_that("joinless_multiple generates dataset names when ys is unnamed", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(id = 3:7, value = 3:7)
  df3 <- data.frame(id = 5:9, value = 5:9)

  res <- joinless::joinless_multiple(
    x  = df1,
    ys = list(df2, df3)
  )

  expect_true("dataset" %in% names(res))
  expect_setequal(unique(res$dataset), c("y1", "y2"))
})


test_that("joinless_multiple replaces empty names in ys", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(id = 3:7, value = 3:7)
  df3 <- data.frame(id = 5:9, value = 5:9)

  ys <- list(df2, df3)
  names(ys) <- c("", "b")

  res <- joinless::joinless_multiple(x = df1, ys = ys)

  expect_setequal(unique(res$dataset), c("y1", "b"))
})


test_that("joinless_multiple errors if dataset_names length mismatches ys", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(id = 3:7, value = 3:7)
  df3 <- data.frame(id = 5:9, value = 5:9)

  expect_error(
    joinless::joinless_multiple(
      x = df1,
      ys = list(df2, df3),
      dataset_names = c("solo_uno")
    ),
    "`dataset_names` must have the same length as `ys`"
  )
})


test_that("joinless_multiple filters y_vars per dataset and skips with warning", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(id = 3:7, value = 3:7)
  df3 <- data.frame(id_alt = 5:9, value = 5:9)  # no tiene "id"

  expect_warning(
    res <- joinless::joinless_multiple(
      x = df1,
      ys = list(a = df2, b = df3),
      x_vars = "id",
      y_vars = "id"
    ),
    "No y_vars found in dataset b; skipping"
  )

  expect_true(is.data.frame(res) || tibble::is_tibble(res))
  expect_setequal(unique(res$dataset), "a")
})


test_that("joinless_multiple returns empty df if all datasets skipped", {
  df1 <- data.frame(id = 1:5, value = 1:5)
  df2 <- data.frame(other = 1:5, value = 1:5)
  df3 <- data.frame(another = 1:5, value = 1:5)

  expect_warning(
    res <- joinless::joinless_multiple(
      x = df1,
      ys = list(a = df2, b = df3),
      x_vars = "id",
      y_vars = "id"
    )
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 0)  # porque devuelves data.frame() vacío
})
