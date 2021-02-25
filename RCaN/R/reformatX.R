#' reformatX
#' check that x parameter is in correct format and returns it as a list
#' @param x either a CaNmod or a named list
#' @return a list with A, b, C and v



reformatX <-
  function(x) {
    if (!class(x) %in% c("CaNmod", "list", "sampleCaNmod"))
      stop("x should either be a CaNmod object or a list")
    if (class(x) %in% c("CaNmod", "sampleCaNmod")){
      A <- as.matrix(x$A)
      b <- x$b
      C <-as.matrix(x$C)
      v <- x$v
    } else {
      if (is.null(names(x)))
        stop("x should be a named list")
      if (! all(c("A", "b") %in% names(x)))
        stop("x should at least contain a matrix A and a list b")
      if (! all(names(x) %in% c("A", "b", "C", "v")))
        stop("names of x should be A, b, C or v")
      A <- x$A
      b <- x$b
      C <- x$C
      v <- x$v
    }
    return(list(A = A,
                b = b,
                C = C,
                v = v))

  }
