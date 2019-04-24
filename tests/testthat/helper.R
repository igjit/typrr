to_file <- function(lines) {
  file <- tempfile()
  writeLines(lines, file)
  file
}
