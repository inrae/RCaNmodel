#' buildVectorConstraint
#' internal function that convert a symbolic equation of constraint into a
#' matrix storing the coefficients of the constraint
#' @param eq_constraint a symbolic equation corresponding to a constraint
#' @param symbolic_enviro the symbolic environment
#' @return a sparse matrix storing the coefficient of the constraint (first
#' column stands for -intercept)
#' @importFrom Matrix Matrix
#' @importFrom symengine free_symbols
#' @importFrom symengine get_str
#' @importFrom symengine get_type
#' @importFrom symengine get_args


buildVectorConstraint <-
  function(eq_constraint, symbolic_enviro) {
    coeff_const <-
      Matrix::Matrix(0, 1, length(symbolic_enviro$param), sparse = TRUE)
    colnames(coeff_const) <- as.character(symbolic_enviro$param)
    basic_constraint <- (eq_constraint)

    #We know check that there is no division
    if (get_type(basic_constraint) == "Add") {
      denom <- lapply(as.list(get_args(basic_constraint)),
                      function(x) {
                        if (!get_type(x) %in% c("RealDouble", "Symbol")) {
                          members <- as.list(get_args(x))
                          types <- sapply(members, get_type)
                          if ("Pow" %in% types)
                            return(members[[which(types == "Pow")]])
                          return(S(1))
                        } else{
                          return(S(1))
                        }
                      })

      denominator <- 1 / do.call("prod", denom)
      nb_elem <- length(as.list(get_args(basic_constraint)))
      numerator <- do.call(sum,
                           lapply(seq_len(nb_elem), function(x) {
                             as.list(get_args(basic_constraint))[[x]] *
                               denominator
                           }))
    } else if (get_type(basic_constraint) != "Mul") {
      numerator <- basic_constraint
      denominator <- S(1)
    } else {
      members <- as.list(get_args(basic_constraint))
      types <- sapply(members, get_type)
      if ("Pow" %in% types) {
        numerator <- do.call(sum, members[-which(types == "Pow")])
        denominator <- members[which(types == "Pow")]
      } else {
        numerator <- basic_constraint
        denominator <- S(1)

      }
    }

    basic_constraint <- expand(numerator)

    mycoeffs <- NULL

    if (get_type(basic_constraint) != "Add") {
      if (get_type(basic_constraint) == "Symbol") {
        mycoeffs <- 1
        names(mycoeffs) <- get_str(basic_constraint)
      } else if (get_type(basic_constraint) == "Mul") {
        mycoeffs <- as.numeric(as.list(get_args(basic_constraint))[[1]])
        names(mycoeffs) <- get_str(as.list(get_args(basic_constraint))[[2]])
      } else if (get_type(basic_constraint) == "NaN") {
        mycoeffs <- NA
        names(mycoeffs) <- "1"
      }else{
        mycoeffs <- as.numeric(basic_constraint)
        names(mycoeffs) <- "1"
      }
    } else {
      mycoeffs <- sapply(as.list(get_args(basic_constraint)), function(e) {
        if (get_type(e) %in% c("Integer", "RealDouble")) {
          return(c("1" = as.numeric(e)))
        } else if (get_type(e) == "Symbol") {
          val <- 1
          names(val) <- get_str(e)
          return(val)
        } else if (get_type(e) == "NaN") {
          return(c("1" = NA))
        } else {
          val <- as.list(get_args(e))[[1]]
          val <- ifelse(get_type(val) == "NaN",
                        NA,
                        as.numeric(val))
          names(val) <- get_str(as.list(get_args(e))[[2]])
          return(val)
        }
      })
    }
    coeff_const[, names(mycoeffs)] <- mycoeffs
    coeff_const
  }
