
#' checkPolytopeStatusCaNmod
#' check whether the polytope is well defined or not
#' @param myCaNmod a CaNmod oject
#'
#' @return  print a message to tell if the polygon is ok or not
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' checkPolytopeStatusCaNmod(myCaNmod)
#'
#' #we artificially add incompatible constraints (negative flow)
#' myCaNmod$A <- rbind(myCaNmod$A,c(1,rep(0,ncol(myCaNmod$A)-1)))
#' rownames(myCaNmod$A)[nrow(myCaNmod$A)]<-"neg_flow"
#' myCaNmod$b <- c(myCaNmod$b,-1)
#' checkPolytopeStatusCaNmod(myCaNmod)



checkPolytopeStatusCaNmod <- function(myCaNmod) {
  checkPolytopeStatus(as.matrix(myCaNmod$A),
                      myCaNmod$b,
                      as.matrix(myCaNmod$C),
                      myCaNmod$v)
}
