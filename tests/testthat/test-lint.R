context("lint.R")

to_file <- function (lines) {
  file <- tempfile()
  writeLines(lines, file)
  file
}

test_that("source_expressions", {
  lines <- c("a <- 1", "a + 2")
  expressions <- source_expressions(to_file(lines))
  expect_equal(purrr::map_chr(expressions, "lines"), lines)
})
