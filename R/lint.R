#' @importFrom purrr imap flatten

source_expressions <- function(filename) {
  lintr::get_source_expressions(filename)$expressions %>%
    head(-1)
}

extract_from_expressions <- function(expressions) {
  gamma <- type_env()
  vng <- variable_name_generator()

  imap(expressions, function(e, i) {
    expr <- parse(text = e$content)[[1]]
    c(E, t) %<-% extract1(gamma, expr, vng)
    map(E, ~ with_index(., i))
  }) %>% flatten
}
