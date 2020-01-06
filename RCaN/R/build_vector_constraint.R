#' build_vector_constraint
#' internal function that convert a symbolic equation of constraint into a
#' matrix storing the coefficients of the constraint
#' @param eq_constraint a symbolic equation corresponding to a constraint
#' @param symbolic_enviro the symbolic environment
#' @return a sparse matrix storing the coefficient of the constraint (first
#' column stands for -intercept)
#' @importFrom Matrix Matrix
#' @importFrom symengine free_symbols
#' @importFrom symengine get_str

build_vector_constraint <- function(eq_constraint, symbolic_enviro) {
  coeff_const <-
    Matrix::Matrix(0, 1, length(symbolic_enviro$param), sparse = TRUE)
  colnames(coeff_const) <- as.character(symbolic_enviro$param)
  basic_constraint <- (eq_constraint)
  all_elements <- get_str(basic_constraint)
  myelem <- strsplit(gsub(" ", "", gsub(
    " +",
    ",+1*",
    gsub(" -", ",-1*", all_elements, fixed = TRUE),
    fixed = TRUE
  )), ",", fixed = TRUE)[[1]]
  param_used <- sort(as.character(free_symbols(basic_constraint)))
  if (length(myelem) > length(param_used)) {
    #there is an intercept
    param_used <- c("1", param_used)
  }
  coeff_const[, param_used] <-
    mapply(function(expr, par)
      eval(parse(text = gsub(
        paste("\\*?", par, "$", sep = ""), "", expr
      ))),
      myelem,
      ifelse(param_used != "1", param_used, ""))
  coeff_const
}
