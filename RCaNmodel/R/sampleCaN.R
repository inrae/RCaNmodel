#' sampleCaN
#'
#' sample the polytope corresponding to the CaNmod model
#' @param myCaNmod a CaNmod or a sampleCaNmod. In this latter case, adapative
#' phase and discarding phases won't be necessary
#' @param N the number of samples required
#' @param nchain the number of mcmc chains
#' @param ncore number of cores to use
#' @param thin thinning interval
#' @param method one of gibbs (default) or hitandrun
#' @param lastF should flow for last year be simulated (default = FALSE)
#' @return a sampleCaNmod object which contains three elements
#' \itemize{
#'  \item{"CaNmod"}{the CaNmod object descring the model}
#'  \item{"mcmc"}{\code{\link[coda]{mcmc.list}}}
#'  \item{"covMat"}{the estimate of the covariance matrix in the first chain
#'  which can be used to samples new iterations without adaptation nor
#'  discarding phase}
#'  \item{"N"}{reminder of the set up of the run}
#'  \item{"thin"}{reminder of the set up of the run}
#'  \item{"nchain"}{reminder of the set up of the run}
#'  \item{"method"}{reminder of the set up of the run}
#' }
#'
#' @export
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel"))
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
#' @importFrom stats runif
sampleCaN <- function(myCaNmod,
                      N,
                      nchain = 1,
                      ncore = 1,
                      thin = 1,
                      method = "gibbs",
                      lastF = FALSE) {
  if (inherits(myCaNmod, "sampleCaNmod")){
    covMat <- myCaNmod$covMat
    myCaNmod <- myCaNmod$CaNmod
  } else{
    covMat <- NULL
  }
  if (! method %in% c("gibbs","hitandrun"))
    stop("method should be either gibbs or hitandrun")
  ncore <- min(min(detectCores() - 1, ncore), nchain)
  `%myinfix%` <- `%do%`

  if (ncore > 1) {
    cl <- makeCluster(ncore)
    clusterEvalQ(cl, {
      library(RCaNmodel)
    })
    clusterEvalQ(cl, {
      library(ROI)
    })
    clusterEvalQ(cl, {
      library(coda)
    })
    clusterEvalQ(cl, {
      library(stats)
    })
    clusterExport(cl, c("myCaNmod", "N", "covMat", "thin", "method"),
                  envir = environment())
    registerDoParallel(cl)
    `%myinfix%` <- `%dopar%`
  }
  i <- NULL
  res <- foreach(i = 1:nchain) %myinfix% {
    x0 <- findInitPoint(myCaNmod$A, myCaNmod$b, myCaNmod$C, myCaNmod$v)
    if (any(is.nan(x0)))
      stop("unable to find any suitable solutions after 100 tries")
    res <-
      sampleCaNCPP(
        N,
        A = as.matrix(myCaNmod$A),
        b = myCaNmod$b,
        C = as.matrix(myCaNmod$C),
        v = myCaNmod$v,
        L = as.matrix(myCaNmod$L),
        x0 = x0,
        thin = thin,
        gibbs = (method == "gibbs"),
        seed = i,
        stream = i,
        covMat = covMat
      )
    names(res) <- c("F", "B", "covMat")
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
    list(samples = mcmc(cbind(res$F, res$B), 1, nrow(res$F), 1),
         covMat = res$covMat)
  }

  if (ncore > 1) {
    stopCluster(cl)
    stopImplicitCluster()
  }
  sampleCaNmod <- list(CaNmod = myCaNmod,
                       mcmc = mcmc.list(lapply(res, function(r) r$samples)),
                       covMat  = res[[1]]$covMat,
                       N = N,
                       thin = thin,
                       nchain = nchain,
                       method = method)
  class(sampleCaNmod) <- "sampleCaNmod"
  return(sampleCaNmod)

}
