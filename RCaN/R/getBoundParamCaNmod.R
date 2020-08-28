#' getBoundParamCaNmod
#' compute bounds for the paramater p
#' @param myCaNmod a CaNmod oject
#' @param p the paramter to be analysed
#'
#' @return a vector with lower bound and upper bound
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' # with the index
#' getBoundParamCaNmod(myCaNmod,1)
#' # with the name
#' getBoundParamCaNmod(myCaNmod,"F01[1988]")
getBoundParamCaNmod <-
  function(myCaNmod, p) {
    if (class(p) == "character")
      p <- which(colnames(myCaNmod$A) == p)
    getBoundParam(as.matrix(myCaNmod$A),
                  myCaNmod$b,
                  p,
                  as.matrix(myCaNmod$C),
                  myCaNmod$v)
  }
