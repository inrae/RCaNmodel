#' plotPolytope2D
#' polt the possible values of a multimensional convex polytope by A.x<=b and
#' C.x=v for 2 dimensions
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
#' @param params a vector of length 2 corresponding to the index of the
#' parameters
#' @return a ggplot
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_polygon
#'
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- plotPolytope2D(A,b)
#'
#' @export


plotPolytope2D <- function(A,
                           b,
                           C = NULL,
                           v = NULL,
                           params = c(1, 2)) {
  nbparam <- ncol(A)
  if (length(params) != 2)
    stop("only works for two params")
  if (class(params) == "character") {
    old_params <- params
    params <- match(params, colnames(A))
    if (length(which(is.na(params))) > 0)
      stop(paste("params", which(old_params[is.na(params)]), "not recognized"))
  }
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  bounds_param1 <- getBoundParam(A, b, params[1], C, v)
  seqx1 <- seq(bounds_param1[1], bounds_param1[2], length.out = 50)
  pb <- txtProgressBar(min = 0, max = 50, style = 3)
  polygon <- lapply(seqx1, function(x) {
    setTxtProgressBar(pb, which(seqx1 == x))
    cbind(rep(x, 2),
          getBoundParam(A, b,
                        params[2], rbind(C, ifelse((1:nbparam) ==
                                                     params[1], 1, 0
                        )), c(v, x)))
  })
  polygon <-
    as.data.frame(rbind(do.call("rbind", lapply(polygon, function(x)
      x[1, ])), do.call("rbind", lapply(rev(polygon), function(x)
        x[2, ]))))
  names(polygon) <- colnames(A)[params]
  ggplot(polygon,
         aes_string(x = colnames(A)[params[1]], y = colnames(A)[params[2]])) +
    geom_polygon()

}
