#' ROI_solve
#' calls \code{\link[ROI]{ROI_solve}} from ROI except for lpsolve
#'
#' @param x an OP object from ROI
#' @param solver the solver to use
#' @param control a list of control parameter
#'
#' @return same as \code{\link[ROI]{ROI_solve}}
#'
#' @importFrom lpSolveAPI read.lp
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI get.primal.solution
#' @importFrom lpSolveAPI dim.lpExtPtr
#' @importFrom lpSolveAPI get.objective
#' @importFrom lpSolveAPI delete.lp
#' @importFrom lpSolveAPI set.objfn
#' @importFrom ROI ROI_plugin_canonicalize_solution
#' @importFrom stats terms


ROI_solve <-
  function(x, solver, control = list()) {
    if (solver != "lpsolve"){
      res <- ROI::ROI_solve(x, solver, control)
    } else {
      lp_model <- read.lp(x$lp_model, type = "mps")
      ob <- as.vector(terms(objective(x))$L[1,])
      dims <- dim.lpExtPtr(lp_model)
      set.objfn(lp_model, ob)
      sense <- ifelse(x$maximum, "max", "min")
      lp.control(lp_model, sense = sense)
      #valide_names <- names(lp.control(lp_model))
      do.call(lp.control,
              c(lp_model, control))
      conv <- solve.lpExtPtr(lp_model)
      x0 <-
        get.primal.solution(lp_model,
                            orig = TRUE)[(dims[1] + 1):(dims[1] +
                                                          dims[2])]
      optimum <- get.objective(lp_model)
      res <- ROI_plugin_canonicalize_solution( solution = x0,
                                               optimum  = optimum,
                                               status   = conv,
                                               solver   = solver)
      delete.lp(lp_model)
    }
    return(res)
  }
