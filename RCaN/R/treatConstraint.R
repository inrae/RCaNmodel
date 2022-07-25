#' treatConstraint
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
#' @importFrom symengine expand V
#' @importFrom stringr str_extract_all



treatConstraint <- function(myconstraint,
                             symbolic_enviro,
                             yr = NULL,
                             name_constr = NULL) {
  years <- as.character(colnames(symbolic_enviro$Fmat))
  ###first we checked if they are vector year indices and change them
  ###for corresponding vector indices
  #[year:year]
  matches <- str_extract_all(myconstraint, "\\[[:digit:]*:[:digit:]*\\]")
  if (length(matches[[1]]) > 0) {
    for (i in seq_along(matches[[1]])) {
      indices <- match(strsplit(matches[[1]][i], "\\[|\\]|:")[[1]][-1],
                       years)
      indices <- paste("[",
                       paste(indices, collapse = ":"),
                       "]",
                       sep = "")
      myconstraint <- gsub(matches[[1]][i],
                           indices,
                           myconstraint,
                           fixed = TRUE)
    }
  }

  #[c(year,year,year...)]
  matches <- str_extract_all(myconstraint,
                             "\\[c\\(([:digit:]*,)+[:digit:]*\\)\\]")
  if (length(matches[[1]]) > 0) {
    for (i in seq_along(matches[[1]])) {
      indices <- match(strsplit(matches[[1]][i], "\\[c\\(|\\,|\\)\\]")[[1]][-1],
                       years)
      indices <- paste("[c(",
                       paste(indices, collapse = ","),
                       ")]",
                       sep = "")
      myconstraint <- gsub(matches[[1]][i],
                           indices,
                           myconstraint,
                           fixed = TRUE)
    }
  }

  #[year]
  matches <- str_extract_all(myconstraint, "\\[[:digit:]*\\]")
  if (length(matches[[1]]) > 0) {
    for (i in seq_along(matches[[1]])) {
      indices <- match(strsplit(matches[[1]][i], "\\[|\\]")[[1]][-1],
                       years)
      indices <- paste("[",
                       indices,
                       "]",
                       sep = "")
      myconstraint <- gsub(matches[[1]][i],
                           indices,
                           myconstraint,
                           fixed = TRUE)
    }
  }


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

  symbolic_enviro$sum <- function(x, ..., na.rm = TRUE) {
    if (inherits(x, "VecBasic"))
      return(sum(x))
    base::sum(x, ..., na.rm = na.rm)
  }
  symbolic_enviro$mean <- function(x, ..., na.rm = TRUE) {
    if (inherits(x, "VecBasic"))
      return(mean(x))
    base::mean(x, ..., na.rm = na.rm)
  }

  symbolic_constraint <-
      eval(parse(text = left), symbolic_enviro)  -
      eval(parse(text = right), symbolic_enviro)
  if (length(symbolic_constraint) == 1) #this is not a vector but a single
                                        #constraint
    symbolic_constraint <- V(symbolic_constraint)  #conversion into VecBasic
  mat <-
    do.call(rbind,
            lapply(as.vector(symbolic_constraint), function(s)
              buildVectorConstraint(s, symbolic_enviro)))
  if (is.null(yr)) {
    yr <- seq_len(nrow(mat))
  } else if (is.na(yr)) {
    yr <- seq_len(nrow(mat))
  } else{
    yr <- years %in% as.character(eval(parse(text = yr)))
  }
  mat <- mat[yr, ,drop=FALSE]
  rownames(mat) <- paste(name_constr, years[yr], sep = " : ")
  mat <- mat[!is.na(Matrix::rowSums(mat)), , drop = FALSE]
  if (nrow(mat) == 0)
    warning(paste("constraint", name_constr, "does not produce any constaints"))
  mat
}
