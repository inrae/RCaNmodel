
#' findingIncompatibleConstrCaNmod
#' compute bounds for all paramaters
#' @param myCaNmod a CaNmod oject
#'
#' @return a list of vector. To be fitted, constraints corresponding to the
#' first elements of each vector of the list should be relaxed.
#' Those constraints appear to be incompatible with following constraints in the
#'  Vector. For example, list(c(A,B),c(D,E, F)) means that A and B should be
#'  relaxed and that
#' A is incompatible with B and that D is incompatible with both E and F
#' @export
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' #we artificially add incompatible constraints (negative flow)
#' myCaNmod$A <- rbind(myCaNmod$A,c(1,rep(0,ncol(myCaNmod$A)-1)))
#' rownames(myCaNmod$A)[nrow(myCaNmod$A)]<-"neg_flow"
#' myCaNmod$b <- c(myCaNmod$b,-1)
#' findingIncompatibleConstrCaNmod(myCaNmod)



findingIncompatibleConstrCaNmod <- function(myCaNmod) {
  findingIncompatibleConstr(as.matrix(myCaNmod$A),
                                 myCaNmod$b,
                                 as.matrix(myCaNmod$C),
                                 myCaNmod$v)
}
