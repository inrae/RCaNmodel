#' plotPolytope2D
#' polt the possible values of a multimensional convex polytope by A.x<=b and
#' C.x=v or by a CaNmod object in 2D
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @param params a vector of length 2 corresponding to the index of the
#' parameters (default, two first)
#' @param progressBar should a progress bar be displayed (default TRUE)
#' @return a ggplot
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
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
#' X0 <- plotPolytope2D(list(A = A, b = b))
#'
#' @export


plotPolytope2D <- function(x,
                           params = c(1, 2),
                           progressBar = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v

  nbparam <- ncol(A)
  if (length(params) != 2)
    stop("only works for two params")
  if (inherits(params, "character")) {
    old_params <- params
    params <- match(params, colnames(A))
    if (length(which(is.na(params))) > 0)
      stop(paste("params", which(old_params[is.na(params)]), "not recognized"))
  }
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  bounds_param1 <- getBoundParam(list(A = A, b = b,
                                      C = C, v = v),
                                 params[1])
  seqx1 <- seq(bounds_param1[1], bounds_param1[2], length.out = 50)
  if (progressBar)
    pb <- txtProgressBar(min = 0, max = 50, style = 3)
  polygon <- lapply(seqx1, function(xi) {
    if (progressBar)
      setTxtProgressBar(pb, which(seqx1 == xi))
    cbind(rep(xi, 2),
          getBoundParam(list(A = A,
                             b = b,
                             C = rbind(C, ifelse((1:nbparam) ==
                                                   params[1], 1, 0
                                                 )),
                             v = c(v, xi)),
                        params[2]))
  })
  polygon <-
    as.data.frame(rbind(do.call("rbind", lapply(polygon, function(x)
      x[1, ])), do.call("rbind", lapply(rev(polygon), function(x)
        x[2, ]))))
  names(polygon) <- colnames(A)[params]
  ggplot(polygon,
         aes_string(x = as.name(colnames(A)[params[1]]),
                    y = as.name(colnames(A)[params[2]]))) +
    geom_polygon()

}
