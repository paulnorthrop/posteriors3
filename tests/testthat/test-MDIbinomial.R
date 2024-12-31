# Check the MDIbinomial() function and its methods

alpha <- 1
beta <- 1
P1 <- MDIbinomial(alpha = 1, beta = 1)
P01 <- MDIbinomial(alpha = 0:1, beta = 0:1)
P3 <- MDIbinomial(alpha = c(1, 2, 1), beta = c(1, 1, 2))

# Normalising constant for the alpha = beta = 1 case
const1 <- 0.617826919366
# Normalising constant for the (alpha=2, beta=1) and (alpha=1, beta=2) cases
const12 <- 0.493598989279
# Note: the alpha = beta = 0 case is U(0, 1)

# P1 (1 distribution) pdf and log_pdf
x_vals <- c(0, 0.5, 1)
pdf_val <- pdf(P1, x_vals)
manual_val <- const1 * c(1, 1 / 2 , 1)
test_that("MDIbinomial(1, 1): pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = FALSE)
})
log_pdf_val <- log_pdf(P1, x_vals)
log_manual_val <- log(c(const1, const1 / 2, const1))
test_that("MDIbinomial(1, 1): log_pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = FALSE)
})
# Check drop = FALSE
pdf_val <- pdf(P1, x_vals, drop = FALSE)
manual_val <- matrix(const1 * c(1, 1 / 2 , 1), nrow = 3, ncol = 1)
test_that("MDIbinomial(1, 1): pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = FALSE)
})

# P01 (2 distributions)
pdf_val <- pdf(P01, x_vals)
manual_val <- cbind(rep(1, 3), const1 * c(1, 1 / 2 , 1))
test_that("MDIbinomial(0:1, 0:1): pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = FALSE)
})
# Check that drop = FALSE change nothing
pdf_val <- pdf(P01, x_vals, drop = FALSE)
test_that("MDIbinomial(0:1, 0:1): pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = FALSE)
})

# P3 (3 distributions)
# Check elementwise = TRUE/FALSE
manual1 <- const1 * c(1, 1 / 2 , 1)
manual2 <- const12 * c(1, (1 / 2) ^ (3 / 2) , 1)
manual3 <- const12 * c(1, (1 / 2) ^ (3 / 2) , 1)
manual_val <- cbind(manual1, manual2, manual3)
pdf_val <- pdf(P3, x_vals, elementwise = FALSE)
test_that("MDIbinomial(c(1, 2, 1), c(1, 1, 2)): pdf, elementwise = TRUE", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = TRUE)
})
pdf_val <- pdf(P3, x_vals, elementwise = TRUE)
test_that("MDIbinomial(c(1, 2, 1), c(1, 1, 2)): pdf, elementwise = FALSE", {
  testthat::expect_equal(pdf_val, diag(manual_val), ignore_attr = TRUE)
})
pdf_val <- pdf(P3, x_vals)
test_that("MDIbinomial(c(1, 2, 1), c(1, 1, 2)): pdf, elementwise = NULL", {
  testthat::expect_equal(pdf_val, diag(manual_val), ignore_attr = TRUE)
})

# support
supp <- 0:1
test_that("MDIbinomial: support, 1 distribution", {
  testthat::expect_equal(support(P1), supp, ignore_attr = TRUE)
})
test_that("MDIbinomial: support, 2 distributions", {
  testthat::expect_equal(support(P01), rbind(supp, supp), ignore_attr = TRUE)
})

# is_discrete, is_continuous
test_that("MDIbinomial: is_continuous 1D", {
  testthat::expect_equal(is_continuous(P1), TRUE)
})
test_that("MDIbinomial: is_discrete, 1D", {
  testthat::expect_equal(is_discrete(P1), FALSE)
})
test_that("MDIbinomial: is_continuous, 2D", {
  testthat::expect_equal(is_continuous(P01), c(TRUE, TRUE))
})
test_that("MDIbinomial: is_discrete, 2D", {
  testthat::expect_equal(is_discrete(P01), c(FALSE, FALSE))
})

# Error/exception triggering
test_that("MDIbinomial: alpha negative", {
  testthat::expect_error(MDIbinomial(alpha = -1))
})
test_that("MDIbinomial: alpha negative", {
  testthat::expect_error(MDIbinomial(beta = -2))
})
