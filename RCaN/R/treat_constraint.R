#' treat_constraint
#'
#' This is an internal function used to convert a string constraint into a symbolic expression
#' @param myconstraint a string corresponding to a constraint
#' @param symbolic_enviro an environment that stores all symbolic computation
#' @param yr the year ranges in which the constraint apply
#' @param name_constr a string naming the constraint
#'
#' @return a matrix that correspond to the coefficient of the constraints, fist column corresponds to the -intercept and each line to a constraint (year)
#' @importFrom symengine expand


treat_constraint <- function(myconstraint,
                             symbolic_enviro,
                             yr = NULL,
                             name_constr = NULL) {
  years <- as.character(colnames(symbolic_enviro$Fmat))
  sign <-
    ifelse (length(grep("<=", myconstraint)) > 0, "<=", ifelse (length(grep(
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
  ###check for division
  left_numerator <- left
  left_denominator <- 1
  right_numerator <- right
  right_denominator <- 1
  if (length(grep("/", left)) > 0) {
    left_numerator <- strsplit(left, "/")[[1]][1]
    left_denominator <- strsplit(left, "/")[[1]][2]
  }
  if (length(grep("/", right)) > 0) {
    right_numerator <- strsplit(right, "/")[[1]][1]
    right_denominator <- strsplit(right, "/")[[1]][2]
  }

  symbolic_constraint <-
    symengine::expand(eval(parse(text = left_numerator),symbolic_enviro) * eval(parse(text =
                                                                        right_denominator),symbolic_enviro) - eval(parse(text = right_numerator),symbolic_enviro) * eval(parse(text =
                                                                                                                                                 left_denominator),symbolic_enviro))
  mat <-
    do.call(rbind, lapply(as.vector(symbolic_constraint), function(s) build_vector_constraint(s,symbolic_enviro)))
  if (is.null(yr)) {
    yr <- 1:nrow(mat)
  } else{
    yr <- years %in% as.character(eval(parse(text = yr)))
  }
  mat <- mat[yr, ]
  rownames(mat) <- paste(years[yr], name_constr, sep = " : ")
  mat
}
