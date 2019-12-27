
#' fitmyCaNmod
#' fit the CaNmod model
#' @param myCaNmod a CaNmod object with following elements
#' @param N the number of samples required
#'
#' @return a list with F and B the matrices of biomass and flows
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx", package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#'
#' @importFrom lpSolveAPI get.constr.value
#' @importFrom lpSolveAPI set.objfn
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI get.primal.solution
#' @importFrom cpgsR defineLPMod
#'
fitmyCaNmod<-function(myCaNmod,N){
  lp_model<-defineLPMod(myCaNmod$A,myCaNmod$b,myCaNmod$C,myCaNmod$v)
  ncontr <- length(get.constr.value(lp_model))
  set.objfn(lp_model, rep(1,ncol(myCaNmod$A)))
  lp.control(lp_model, sense = "max")
  solve.lpExtPtr(lp_model)
  x0 <-
    get.primal.solution(lp_model, orig = TRUE)[(ncontr + 1):(ncontr + ncol(myCaNmod$A))]
  res <- fitCaN(N, as.matrix(myCaNmod$A), myCaNmod$b, as.matrix(myCaNmod$C), myCaNmod$v, as.matrix(myCaNmod$L), myCaNmod$M, x0)
  names(res)<-c("F","B")
  print(dim(res$F))
  print(dim(myCaNmod$A))
  colnames(res$F)<-colnames(myCaNmod$A)
  print("there")
  colnames(res$B)<-rownames(myCaNmod$L)
  print("nowhere")
  res
}
