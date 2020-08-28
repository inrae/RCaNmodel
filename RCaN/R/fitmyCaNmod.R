
#' fitmyCaNmod
#'
#' fit the CaNmod model
#' @param myCaNmod a CaNmod object with following elements
#' @param N the number of samples required
#' @param nchain the number of mcmc chains
#' @param ncore number of cores to use
#' @param thin thinning interval
#' @return a \code{\link[coda]{mcmc.list}}
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#'
#' @importFrom lpSolveAPI get.constr.value
#' @importFrom lpSolveAPI set.objfn
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI get.primal.solution
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom doRNG registerDoRNG
#' @importFrom coda mcmc
#' @importFrom coda mcmc.list
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom foreach %do%
#' @importFrom doRNG "%dorng%"
#' @importFrom stats runif
fitmyCaNmod <- function(myCaNmod,
                        N,
                        nchain = 1,
                        ncore = 1,
                        thin = 1) {
  ncore <- min(min(detectCores() - 1, ncore), nchain)
  `%myinfix%` <- `%do%`

  if (ncore > 1) {
    cl <- makeCluster(ncore)
    clusterEvalQ(cl, {
      library(RCaN)
    })
    clusterEvalQ(cl, {
      library(lpSolveAPI)
    })
    clusterEvalQ(cl, {
      library(coda)
    })
    clusterEvalQ(cl, {
      library(doRNG)
    })
    clusterEvalQ(cl, {
      library(stats)
    })
    clusterExport(cl, c("myCaNmod", "N"), envir = environment())
    registerDoParallel(cl)
    registerDoRNG(seed = 123)
    `%myinfix%` <- `%dopar%`
  }
  res <- foreach(i = 1:nchain) %myinfix% {
    find_init <- FALSE
    nbiter <- 0
    while (nbiter < 100 & !find_init) {
      lp_model <- defineLPMod(myCaNmod$A, myCaNmod$b, myCaNmod$C, myCaNmod$v)
      ncontr <- length(get.constr.value(lp_model))
      set.objfn(lp_model, runif(ncol(myCaNmod$A)))
      lp.control(lp_model, sense = "min")
      conv <- solve.lpExtPtr(lp_model)
      x0 <-
        get.primal.solution(lp_model,
                            orig = TRUE)[(ncontr + 1):(ncontr +
                                                         ncol(myCaNmod$A))]
      if (conv == 0)
        find_init <- TRUE
      nbiter < nbiter + 1
    }
    if (!find_init)
      stop("unable to find any suitable solutions after 100 tries")
    res <-
      fitCaN(
        N,
        thin = thin,
        as.matrix(myCaNmod$A),
        myCaNmod$b,
        as.matrix(myCaNmod$C),
        myCaNmod$v,
        as.matrix(myCaNmod$L),
        x0
      )
    names(res) <- c("F", "B")
    res$F <- res$F[, -seq_len(length(myCaNmod$species))]
    #we remove the first column which corresponds to initial biomasses
    colnames(res$F) <- colnames(myCaNmod$A)[-seq_len(length(myCaNmod$species))]
    colnames(res$B) <- rownames(myCaNmod$L)
    mcmc(cbind(res$F, res$B), 1, nrow(res$F), 1)
  }

  if (ncore > 1) {
    stopCluster(cl)
    stopImplicitCluster()
  }
  mcmc.list(res)
}
