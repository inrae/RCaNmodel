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

  #We know check that there is no division
  if(get_type(basic_constraint)=="Add"){
    denom <- lapply(as.list(get_args(basic_constraint)),
                    function(x){
                      if (!get_type(x) %in% c("RealDouble","Symbol")){
                        members <- as.list(get_args(x))
                        types <- sapply(members,get_type)
                        if ("Pow" %in% types)
                          return(members[[which(types == "Pow")]])
                        return(S(1))
                      } else{
                        return(S(1))
                      }
                    })

    denominator <- 1/ do.call('prod',denom)
    nb_elem <- length(as.list(get_args(basic_constraint)))
    numerator<- do.call(sum,
                        lapply(seq_len(nb_elem), function(x){
                          as.list(get_args(basic_constraint))[[x]]*denominator
                        }))
  } else if (get_type(basic_constraint) != "Mul"){
    numerator <- basic_constraint
    denominator <- S(1)
  } else {
    members <- as.list(get_args(basic_constraint))
    types <- sapply(members,get_type)
    if ("Pow" %in% types){
      numerator <- do.call(sum, members[-which(types=="Pow")])
      denominator <- members[which(types=="Pow")]
    } else {
      numerator <- basic_constraint
      denominator <- S(1)

    }
  }

  basic_constraint <- expand(numerator)




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
