#' getBoundParamCaNmod
#' compute bounds for the paramater p
#' @param myCaNmod a CaNmod oject
#' @param p the paramter to be analysed
#'
#' @return a vector with lower bound and upper bound
#' @export
#'
#' @importFrom cpgsR getBoundParam
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx", package = "RCaN"))
#' getBoundParamCaNmod(myCaNmod,1)
getBoundParamCaNmod <-
  function(myCaNmod, p) {
    getBoundParam(as.matrix(myCaNmod$A), myCaNmod$b, p, as.matrix(myCaNmod$C), myCaNmod$v)
  }
