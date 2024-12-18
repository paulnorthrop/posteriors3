# Check that the likelihood object d has appropriate data as an attribute

# 1 Binomial distribution

d1 <- Binomial(size = 10)

data <- list(c(1:2, 1:3))
d_correct <- add_data(d1, data)
test_that("1 Binomial, using add_data() does not throw an error)", {
  testthat::expect_no_error(check_data(d_correct))
})

# 2 Binomial distributions

d2 <- Binomial(size = c(5, 10))

test_that("likelihood object with no data throws an error", {
  testthat::expect_error(check_data(d2))
})

attr(d2, "data") <- 1:10
test_that("likelihood object with data that is not a list throws an error", {
  testthat::expect_error(check_data(d2))
})

attr(d2, "data") <- list(data1 = 1:2)
test_that("likelihood object with list data of the wrong length throws an error", {
  testthat::expect_error(check_data(d2))
})

data <- list(1:2, 1:3)
d_correct <- add_data(d2, data)
test_that("2 Binomials, using add_data() does not throw an error)", {
  testthat::expect_no_error(check_data(d_correct))
})

## 3 Binomial distributions

d3 <- Binomial(size = c(5, 10, 20))

# No problems with the data

data <- list(1:2, 1:3, 1:6)
d_correct <- add_data(d3, data)
test_that("3 Binomials, using add_data() does not throw an error)", {
  testthat::expect_no_error(check_data(d_correct))
})

# Check for data that is outside the range of the support

data <- list(-1:2, 1:3, 1:6)
d3 <- add_data(d3, data)
test_that("3 Binomials, data below min(support) throws an error)", {
  testthat::expect_error(check_data(d3))
})

data <- list(1:6, 1:3, 1:6)
d3 <- add_data(d3, data)
test_that("3 Binomials, data below min(support) throws an error)", {
  testthat::expect_error(check_data(d3))
})

# Check for non-integer data in the Binomial case

data <- list(1:2, 1:3, c(1:6, 1.01))
d3 <- add_data(d3, data)
test_that("3 Binomials, data below min(support) throws an error)", {
  testthat::expect_error(check_data(d3))
})

# Check that the different ways of supplying data to add_data() are equivalent

## 2 Binomial distributions

d2 <- Binomial(size = c(5, 10))

list_data <- list(1:2, 1:3)
d2 <- add_data(d2, list_data)
test_that("2 Binomials, add_data(): adding a list returns the same list", {
  testthat::expect_equal(attr(d2, "data"), list_data)
})

df_data <- data.frame(data1 = c(1:2, NA), data2 = 1:3)
d_df <- add_data(d2, df_data)
# Use ignore_attr to ignore the attributes resulting from na.omit()
test_that("2 Binomials, add_data(): adding a data frame returns the correct list", {
  testthat::expect_equal(attr(d_df, "data"), list_data, ignore_attr = TRUE)
})

matrix_data <- matrix(c(1:2, NA, 1:3), nrow = 2, ncol = 3, byrow = TRUE)
rownames(matrix_data) <-  c("data1", "data2")
d_mat <- add_data(d2, matrix_data)
# Use ignore_attr to ignore the attributes resulting from na.omit()
test_that("2 Binomials, add_data(): adding a matrix returns the correct list", {
  testthat::expect_equal(attr(d_mat, "data"), list_data, ignore_attr = TRUE)
})

test_that("2 Binomials, add_data(): df and matrix inputs agree", {
  testthat::expect_equal(attr(d_df, "data"), attr(d_mat, "data"))
})

## 1 Binomial distribution

d1 <- Binomial(size = 10)

list_data <- list(1:2)
d1 <- add_data(d1, list_data)
test_that("1 Binomial, add_data(): adding a list returns the same list", {
  testthat::expect_equal(attr(d1, "data"), list_data)
})

df_data <- data.frame(data1 = c(NA, NA, 1, NA, 2, NA))
d_df <- add_data(d1, df_data)
# Use ignore_attr to ignore the attributes resulting from na.omit()
test_that("1 Binomial, add_data(): adding a data frame returns the correct list", {
  testthat::expect_equal(attr(d_df, "data"), list_data, ignore_attr = TRUE)
})

matrix_data <- matrix(c(NA, NA, 1, NA, 2, NA), nrow = 1, ncol = 6, byrow = TRUE)
rownames(matrix_data) <- "data1"
d_mat <- add_data(d1, matrix_data)
# Use ignore_attr to ignore the attributes resulting from na.omit()
test_that("1 Binomial, add_data(): adding a data frame returns the correct list", {
  testthat::expect_equal(attr(d_mat, "data"), list_data, ignore_attr = TRUE)
})

test_that("1 Binomial, add_data(): df and matrix inputs agree", {
  testthat::expect_equal(attr(d_df, "data"), attr(d_mat, "data"))
})
