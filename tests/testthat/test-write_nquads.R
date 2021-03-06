context("write nquads")


test_that("We can write nquads from a data frame", {
  tmp <- tempfile(fileext = ".nq")
  write_nquads(iris, tmp, prefix = "iris")
  expect_true(file.exists(tmp))

  write_nquads(iris, tmp, prefix = "iris", compress = TRUE)

})



test_that("We can write nquads from a list", {
  tmp <- tempfile(fileext = ".nq")
  x <- list(A1 = list(B = 1, C = 2), A2 = "bob")
  expect_warning(write_nquads(x, tmp))
  expect_true(file.exists(tmp))

  write_nquads(x, tmp, prefix = "x", compress = TRUE)

})

