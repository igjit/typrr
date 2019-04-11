context("lint.R")

test_that("source_expressions", {
  lines <- c("a <- 1", "a + 2")
  file <- tempfile()
  writeLines(lines, file)
  expressions <- source_expressions(file)
  expect_equal(purrr::map_chr(expressions, "lines"), lines)
})
