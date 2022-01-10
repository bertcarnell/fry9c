test_that("get_fry9c_data works", {
  X <- get_fry9c_data(2018, 1, FALSE)
  expect_true(dim(X)[1] > 27)

  Y <- get_fry9c_data(2018, 2, FALSE)
  expect_true(dim(Y)[1] > 27)

  expect_output(Z <- get_fry9c_data(2018, 1, TRUE))
})
