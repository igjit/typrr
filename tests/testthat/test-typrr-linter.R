context("typrr-linter.R")

source_expression <- function(line) {
  lintr::get_source_expressions(to_file(line))$expressions[[1]]
}

test_that("typrr_linter", {
  expect_null(typrr_linter()(source_expression("1 + 2")))
  expect_s3_class(typrr_linter()(source_expression("1 + TRUE")), "lint")
})
