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
#' @param keepCovMat save or not the covariance matrix (saving can gain time
#' if new samples should be run but is very memory consuming - default FALSE)
#' @return a sampleCaNmod object which contains three elements
#' \itemize{
#'  \item{"CaNmod"}{the CaNmod object descring the model}
#'  \item{"mcmc"}{\code{\link[coda]{mcmc.list}}}
#'  \item{"x0"}{average starting point}
#'  \item{"covMat"}{average estimated covariance matrix that can be used
#'  for other run, if kept}
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
                      lastF = FALSE,
                      keepCovMat = FALSE) {
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
    cl <- makeCluster(ncore, outfile = "")
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
  
  writeLines("##Initializing")
  #we removed parameters that are fixed because of A or almost
  bounds <- getAllBoundsParam(list(A = as.matrix(myCaNmod$A),
                                   b = myCaNmod$b),
                              progressBar = TRUE)
  
  #if some are fixed, we add them to C
  fixed <- which(abs(bounds[, 2] - bounds[, 3]) <= 0)
  if (length(fixed) > 0){
    myCaNmod$v <- c(myCaNmod$v, rowMeans(bounds[fixed, 2:3, drop = FALSE]))
    newC <- Matrix(0, length(fixed), ncol(myCaNmod$A))
    newC[cbind(seq_len(length(fixed)), fixed)] <- 1
    myCaNmod$C <- rbind(myCaNmod$C, newC)
  }
  
  #now we restrict to the degenerate subspace
  solequality <- findInitPoint(as.matrix(myCaNmod$A),
                               myCaNmod$b,
                               as.matrix(myCaNmod$C),
                               myCaNmod$v,
                               progressBar = TRUE)
  if (nrow(myCaNmod$C) > 0){
    subspace <- degenerateSubSpace(as.matrix(myCaNmod$A),
                                   myCaNmod$b,
                                   as.matrix(myCaNmod$C),
                                   myCaNmod$v,
                                   solequality)
    A2 <- subspace$A2
    b2 <- subspace$b2
    Nt <- subspace$Nt
    colnames(A2) <- paste("param", seq_len(ncol(A2)))
    
  } else { #no equality constraints, everything remains the same
    A2 <-myCaNmod$A
    b2 <- myCaNmod$b - myCaNmod$A %*% solequality
    Nt <- diag(ncol(myCaNmod$A))
  }
  
  #now we presolve the model to simply the polytope
  presolved <- presolveLPMod(A2,
                             b2,
                             lower = rep(-Inf, ncol(A2)),
                             upper = rep(Inf, ncol(A2)),
                             sense = "min")
  A3 <- presolved$A
  b3 <- presolved$b
  fixed <- presolved$fixed
  
  #to avoid computations problems, we slightly shift b3
  b3 <- b3 + 1e-7
  #to avoid computations problems, we slightly shift b3
  b3 <- b3 + 1e-7
  #we add the bounds as constraints
  nonnull <- which(is.finite(presolved$lower))
  if (length(nonnull) > 0){
    bounds <- matrix(0, length(nonnull), ncol(A3))
    bounds[cbind(seq_len(length(nonnull)),
                 nonnull)] <- - 1
    A3 <- rbind(A3, bounds)
    b3 <- c(b3, -presolved$lower[nonnull])
  }
  
  nonnull <- which(is.finite(presolved$upper))
  if (length(nonnull) > 0){
    bounds <- matrix(0, length(nonnull), ncol(A3))
    bounds[cbind(seq_len(length(nonnull)),
                 nonnull)] <- 1
    A3 <- rbind(A3, bounds)
    b3 <- c(b3, presolved$upper[nonnull])
  }
  
  
  if (ncore > 1) {
    clusterExport(cl, c("A2", "b2", "Nt",
                        "solequality", "A3",
                        "b3", "fixed", "keepCovMat"),
                  envir = environment())
  }
  
  writeLines("##Sampling")
  
  res <- foreach(i = 1:nchain) %myinfix% {
    writeLines(paste("###Start chain",i))
    x0 <- rep(0, ncol(A3))
    if (any(is.nan(x0)))
      stop("unable to find any suitable solutions after 100 tries")
    writeLines(paste("###Start cpgs chain",i))
    
    res <-
      cpgs(N, A3, b3, x0, thin, method == "gibbs", i, i, covMat)
    #now we turn back result into original format
    if (length(fixed) > 0){
      res$X <- cbind(res$X, 
                     matrix(rep(fixed, nrow(res$X)),
                            nrow(res$X),
                            length(fixed),
                            byrow = TRUE))
      colnames(res$X) <- c(colnames(A3), names(fixed))  
      res$X <- res$X[, colnames(A2)]
      
    } else {
      colnames(res$X) <- colnames(A2)  
    }
    
    res$X <- t(Nt %*% t(res$X)) + 
      matrix(rep(solequality, nrow(res$X)),
             nrow(res$X),
             byrow = TRUE)
    colnames(res$X) <- colnames(myCaNmod$A)
    names(res) <- c("F", "covMat")
    res$B <- t(apply(res$F, 1, function(x) as.matrix(myCaNmod$L) %*% x))
    
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
    writeLines(paste("###End chain",i))
    covmat <- NULL
    if (keepCovMat)
      covmat <- res$covMat
    list(samples = mcmc(cbind(res$F, res$B), 1, nrow(res$F), 1),
         covMat = covmat, x0 = x0)
  }
  
  if (ncore > 1) {
    stopCluster(cl)
    stopImplicitCluster()
  }
  covmat <- NULL
  if (keepCovMat)
    covmat <- Reduce('+',
                     lapply(res, function(x) x$covMat)) /
    length(res)
  sampleCaNmod <- list(CaNmod = myCaNmod,
                       mcmc = mcmc.list(lapply(res, function(r) r$samples)),
                       covMat  = covmat,
                       x0  = rowMeans(do.call(cbind,
                                              lapply(res, function(x) x$x0))),
                       N = N,
                       thin = thin,
                       nchain = nchain,
                       method = method)
  class(sampleCaNmod) <- "sampleCaNmod"
  return(sampleCaNmod)
  
}
