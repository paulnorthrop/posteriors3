# Check that providing incorrect data to add_data() throws an error

d2 <- Binomial(size = c(5, 10))

df_data <- data.frame(data1 = c(1:2, NA))
test_that("add_data(): incorrect data frame throws an error", {
  testthat::expect_error(add_data(d2, df_data))
})

matrix_data <- matrix(c(1:2, NA, 1:3), nrow = 6, ncol = 1)
test_that("add_data(): incorrect data matrix throws an error", {
  testthat::expect_error(add_data(d2, matrix_data))
})

list_data <- list(1:2)
test_that("add_data(): incorrect data list throws an error", {
  testthat::expect_error(add_data(d2, list_data))
})

character_data <- c("a", "b")
test_that("add_data(): character data throws an error", {
  testthat::expect_error(add_data(d2, character_data))
})

numeric_data <- 1:3
test_that("add_data(): numeric data does not throw an error", {
  testthat::expect_no_error(add_data(d2, numeric_data))
})
