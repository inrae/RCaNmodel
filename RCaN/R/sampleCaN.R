#' sampleCaN
#'
#' sample the polytope corresponding to the CaNmod model
#' @param myCaNmod a CaNmod object with following elements
#' @param N the number of samples required
#' @param nchain the number of mcmc chains
#' @param ncore number of cores to use
#' @param thin thinning interval
#' @param method one of gibbs (default) or hitandrun
#' @param lastF should flow for last year be simulated (default = FALSE)
#' @return a sampleCaNmod object which contains two elements
#' \itemize{
#'  \item{"CaNmod"}{the CaNmod object descring the model}
#'  \item{"mcmc"}{\code{\link[coda]{mcmc.list}}}
#' }
#'
#' @export
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#'
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom coda mcmc
#' @importFrom coda mcmc.list
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom foreach %do%
#' @importFrom stats runif
sampleCaN <- function(myCaNmod,
                      N,
                      nchain = 1,
                      ncore = 1,
                      thin = 1,
                      method="gibbs",
                      lastF = FALSE) {
  if (! method %in% c("gibbs","hitandrun"))
    stop("method should be either gibbs or hitandrun")
  ncore <- min(min(detectCores() - 1, ncore), nchain)
  `%myinfix%` <- `%do%`

  if (ncore > 1) {
    cl <- makeCluster(ncore)
    clusterEvalQ(cl, {
      library(RCaN)
    })
    clusterEvalQ(cl, {
      library(ROI)
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
    `%myinfix%` <- `%dopar%`
  }
  i <- NULL
  res <- foreach(i = 1:nchain) %myinfix% {
    find_init <- FALSE
    nbiter <- 0
    while (nbiter < 100 & !find_init) {
      lp_model <- defineLPMod(myCaNmod$A, myCaNmod$b, myCaNmod$C, myCaNmod$v,
                              maximum = FALSE,
                              ob = runif(ncol(myCaNmod$A)))
      res <- ROI_solve(lp_model, solver = "lpsolve",
                       list(presolve = c("rows",
                                         "lindep",
                                         "rowdominate",
                                         "mergerows"),
                                      scaling = c("extreme",
                                                  "equilibrate",
                                                  "integers")))
      if (requireNamespace("ROI.plugin.clp", quietly = TRUE)
          & res$status$code == 5){
        res <- ROI_solve(lp_model, solver = "clp", control = list(amount = 0))
      }
      x0 <- res$solution

      if (res$status$code == 0)
        find_init <- TRUE
      nbiter <- nbiter + 1
    }
    if (!find_init)
      stop("unable to find any suitable solutions after 100 tries")
    file.remove(lp_model$lp_model)

    res <-
      sampleCaNCPP(
        N,
        thin = thin,
        as.matrix(myCaNmod$A),
        myCaNmod$b,
        as.matrix(myCaNmod$C),
        myCaNmod$v,
        as.matrix(myCaNmod$L),
        x0,
        method == "gibbs",
        i,
        i
      )
    names(res) <- c("F", "B")
    res$F <- res$F[, -seq_len(length(myCaNmod$species))]
    #we remove the first column which corresponds to initial biomasses
    colnames(res$F) <- colnames(myCaNmod$A)[-seq_len(length(myCaNmod$species))]

    if (!lastF) {#we removed last time step
      lastid <- which(colnames(res$F) %in% paste(myCaNmod$fluxes_def$Flux,
                                                 "[",
                                                 max(myCaNmod$series$Year),
                                                 "]",
                                                 sep = ""))
      if (length(lastid) > 0)
        res$F <- res$F[, - lastid]
    }
    colnames(res$B) <- rownames(myCaNmod$L)
    mcmc(cbind(res$F, res$B), 1, nrow(res$F), 1)
  }

  if (ncore > 1) {
    stopCluster(cl)
    stopImplicitCluster()
  }
  sampleCaNmod <- list(CaNmod = myCaNmod,
                       mcmc = mcmc.list(res))
  class(sampleCaNmod) <- "sampleCaNmod"
  return(sampleCaNmod)

}
