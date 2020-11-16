#' plotPolytope2DCaNmod
#' polt the possible values of the polytope 2 dimensions
#' @param myCaNmod a CaNmod oject
#' @param params the names of the parameters
#' @return a ggplot
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_polygon
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' plotPolytope2DCaNmod(myCaNmod, c("OmniZooplankton[1988]", "F01[1988]"))
#'
#' @export


plotPolytope2DCaNmod <- function(myCaNmod,
                                 params) {
  if (class(myCaNmod) != "CaNmod")
    stop("myCaNmod should be a CaNmod object")
  if (length(params) != 2)
    stop("params should be of length 2")
  if (!all(params %in% colnames(myCaNmod$A)))
    stop("params not recognised")
  iparam <- match(params,
                  colnames(myCaNmod$A))
  A <- myCaNmod$A
  b <- myCaNmod$b
  C <- myCaNmod$C
  v <- myCaNmod$v
  plotPolytope2D(A,
                 b,
                 C,
                 v,
                 iparam)
}
