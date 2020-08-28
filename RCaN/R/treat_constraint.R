#' treat_constraint
#'
#' This is an internal function used to convert a string constraint into a
#' symbolic expression
#' @param myconstraint a string corresponding to a constraint
#' @param symbolic_enviro an environment that stores all symbolic computation
#' @param yr the year ranges in which the constraint apply
#' @param name_constr a string naming the constraint
#'
#' @return a matrix that correspond to the coefficient of the constraints, first
#' column corresponds to the -intercept and each line to a constraint (year)
#' @importFrom symengine expand



treat_constraint <- function(myconstraint,
                             symbolic_enviro,
                             yr = NULL,
                             name_constr = NULL) {
  years <- as.character(colnames(symbolic_enviro$Fmat))
  sign <-
    ifelse(length(grep("<=", myconstraint)) > 0, "<=", ifelse(length(grep(
      ">=", myconstraint
    )) > 0, ">=", "="))
  tmp <- strsplit(myconstraint, sign)[[1]]
  if (sign == "<=" | sign == "=") {
    left <- tmp[1]
    right <- tmp[2]
  } else if (sign == ">=") {
    left <- tmp[2]
    right <- tmp[1]
  } else{
    stop(paste("unrecognized sign in constraint:", myconstraint))
  }

  symbolic_constraint <-
      eval(parse(text = left), symbolic_enviro)  -
      eval(parse(text = right), symbolic_enviro)
  mat <-
    do.call(rbind,
            lapply(as.vector(symbolic_constraint), function(s)
              build_vector_constraint(s, symbolic_enviro)))
  if (is.null(yr)) {
    yr <- seq_len(nrow(mat))
  } else{
    yr <- years %in% as.character(eval(parse(text = yr)))
  }
  mat <- mat[yr, ]
  rownames(mat) <- paste(years[yr], name_constr, sep = " : ")
  mat <- mat[!is.na(Matrix::rowSums(mat)), ]
  if (nrow(mat) == 0)
    warning(paste("constraint", name_constr, "does not produce any constaints"))
  mat
}
