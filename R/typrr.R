#' @importFrom zeallot %<-%
#' @importFrom purrr map detect

equation <- function(lhs, rhs) {
  structure(list(lhs = lhs, rhs = rhs), class = "equation")
}

empty_equation <- function() structure(list(), class = "equation")

print.equation <- function(x, ...) {
  if (length(x) != 0) {
    cat(as.character(x$lhs), "=", as.character(x$rhs))
  }
  cat("\n")
  invisible(x)
}

`%U%` <- function(eq1, eq2) {
  if (length(eq2) == 0) {
    eq1
  } else if (is(eq2, "equation")){
    c(eq1, list(eq2))
  } else {
    c(eq1, eq2)
  }
}

type_env <- function(parent = emptyenv()) {
  new.env(parent = parent)
}

variable_name_generator <- function(i = 1) {
  function() {
    ch <- letters[i]
    if (is.na(ch)) stop()
    i <<- i + 1
    paste0("'", ch)
  }
}

extract1 <- function(gamma, e, vng = variable_name_generator()) {
  if (typeof(e) %in% c("integer", "double", "logical", "character")) {
    list(empty_equation(), type_of(e))
  } else if (is.name(e)) {              # name
    list(empty_equation(), gamma[[as.character(e)]])
  } else if (as.character(e[[1]]) %in% c("+", "-", "*", "/")) { # e1 op e2
    e1 <- e[[2]]
    e2 <- e[[3]]
    c(E1, t1) %<-% extract1(gamma, e1, vng)
    c(E2, t2) %<-% extract1(gamma, e2, vng)
    E3 <- E1 %U% E2 %U% equation(t1, type("double")) %U% equation(t2, type("double"))
    list(E3, type("double"))
  } else if (as.character(e[[1]]) %in% c("<-", "=")) { # x <- e1
    x <- e[[2]]
    e1 <- e[[3]]
    c(E1, t1) %<-% extract1(gamma, e1, vng)
    gamma[[as.character(x)]] <- t1
    list(E1, t1)
  } else if (as.character(e[[1]]) == "function") { # function(x) e1
    x <- names(e[[2]])
    e1 <- e[[3]]
    alpha <- type_variable(vng())
    gamma[[as.character(x)]] <- alpha
    c(E, t0) %<-% extract1(gamma, e1, vng)
    list(E, function_type(alpha, t0))
  } else if (is.call(e)) {              # e1(e2)
    e1 <- e[[1]]
    e2 <- e[[2]]
    c(E1, t1) %<-% extract1(gamma, e1, vng)
    c(E2, t2) %<-% extract1(gamma, e2, vng)
    alpha <- type_variable(vng())
    E3 <- E1 %U% E2 %U% equation(t1, function_type(t2, alpha))
    list(E3, alpha)
  } else {
    stop("unknown expression: ", e)
  }
}

extract <- function(expressions) {
  E_all <- empty_equation()
  gamma <- type_env()
  vng <- variable_name_generator()
  for(e in expressions) {
    c(E, t) %<-% extract1(gamma, e, vng)
    E_all <- c(E_all, E)
  }
  list(E_all, t)
}

substitute_type1 <- function(var, val, ty) {
  if (is(ty, "type_variable") && eq(ty, var)) {
    val
  } else if (is(ty, "function_type")) {
    from <- substitute_type1(var, val, ty$from)
    to <- substitute_type1(var, val, ty$to)
    function_type(from, to)
  } else {
    ty
  }
}

substitute_type <- function(var, val, E) {
  lhs <- substitute_type1(var, val, E$lhs)
  rhs <- substitute_type1(var, val, E$rhs)
  equation(lhs, rhs)
}

unify <- function(equations) {
  if(length(equations) == 0) return(list())

  E1 <- equations[[1]]
  E_rest <- equations[-1]

  if (eq(E1$lhs, E1$rhs)) {               # τ = τ
    unify(E_rest)
  } else if (is(E1$lhs, "type_variable")) { # α = τ
    assigned <- map(E_rest, ~ substitute_type(E1$lhs, E1$rhs, .))
    S <- unify(assigned)
    S %U% E1
  } else if (is(E1$rhs, "type_variable")) { # τ = α
    assigned <- map(E_rest, ~ substitute_type(E1$rhs, E1$lhs, .))
    S <- unify(assigned)
    S %U% equation(E1$rhs, E1$lhs)
  } else if (is(E1$lhs, "function_type")) {
    E <- empty_equation() %U%
      equation(E1$lhs$from, E1$rhs$from) %U%
      equation(E1$lhs$to, E1$rhs$to) %U%
      E_rest
    unify(E)
  } else {
    stop("type error")
  }
}

PT <- function(expressions) {
  c(E, ty) %<-% extract(expressions)
  S <- unify(E)
  if (is(ty, "type_variable")) {
    detect(S, ~ .$lhs$name == ty$name)$rhs
  } else {
    ty
  }
}
