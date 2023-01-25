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
#'


ROI_solve <-
  function(x, solver, control = list()) {
    if (!solver %in% c("cbc", "lpsolve"))
      stop("solver should be one of cbc or lpsolve")
    if (solver != "lpsolve"){
      res <- ROI::ROI_solve(x, solver, control)
      if (res$status$msg$symbol == "unbounded"){
        res$status$msg$code <- 3
      } else if (res$status$msg$symbol == "Numerical instability"){
        res$status$msg$code <- 5
      } else if (res$status$msg$symbol== "infeasible"){
        res$status$msg$code <- 2
      }
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
      if (any(x0 == 1e30)) { #this is the infinite bound of lpsolve
        conv <- 3
        x0[which(x0 == 1e30)] <- Inf
      }
      if (any(x0 == - 1e30)){ #this is the infinite bound of lpsolve
        conv <- 3
        x0[which(x0 == - 1e30)] <- -Inf
      }
      status <- list(code = as.integer(conv != 0),
                     msg = list(solver = "lpsolve",
                                code = conv,
                                message = "",
                                roi_code = as.integer(conv != 0)))
      optimum <- get.objective(lp_model)
      res <- list( solution = x0,
                   objval = optimum,
                   status = status)

    }
    return(res)
  }
