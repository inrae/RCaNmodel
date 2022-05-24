#' ROI_solve
#' calls \code{\link[ROI]{ROI_solve}} from ROI except for lpsolve
#'
#' @param x an OP object from ROI
#' @param solver the solver to use
#' @param control a list of control parameter
#'
#' @return same as \code{\link[ROI]{ROI_solve}}
#'
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI get.primal.solution
#' @importFrom lpSolveAPI dim.lpExtPtr
#' @importFrom lpSolveAPI get.objective
#' @importFrom ROI ROI_plugin_canonicalize_solution
#'


ROI_solve <-
  function(x, solver, control = list()) {
    if (solver != "lpsolve"){
      res <- ROI::ROI_solve(x, solver, control)
    } else {
      lp_model <- x$lp_model
      dims <- dim.lpExtPtr(lp_model)
      valide_names <- names(lp.control(lp_model))
      do.call(lp.control,
              c(lp_model, control[names(control) %in% valide_names]))
      conv <- solve.lpExtPtr(lp_model)
      x0 <-
        get.primal.solution(lp_model,
                            orig = TRUE)[(dims[1] + 1):(dims[1] +
                                                          dims[2])]
      if (any(x0 == 1.0e30)) { #this is the infinite bound of lpsolve
        conv <- 3
        x0[which(x0 == 1.0e30)] <- Inf
      }
      if (any(x0 == - 1.0e30)){ #this is the infinite bound of lpsolve
        conv <- 3
        x0[which(x0 == - 1.0e30)] <- Inf
      }
      optimum <- get.objective(lp_model)
      res <- ROI_plugin_canonicalize_solution( solution = x0,
                                               optimum  = optimum,
                                               status   = conv,
                                               solver   = solver)
    }
    return(res)
  }
