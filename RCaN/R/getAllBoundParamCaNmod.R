#' getAllBoundsParamCaNmod
#' compute bounds for all paramaters
#' @param myCaNmod a CaNmod oject
#'
#' @return a data frame with first column as names of params and 2 columns for
#' bounds
#' @export
#'
#' @importFrom cpgsR getAllBoundsParam
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' getAllBoundsParamCaNmod(myCaNmod)

getAllBoundsParamCaNmod <- function(myCaNmod) {
  getAllBoundsParam(as.matrix(myCaNmod$A),
                    myCaNmod$b,
                    as.matrix(myCaNmod$C),
                    myCaNmod$v)
}
