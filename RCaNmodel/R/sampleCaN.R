#' sampleCaN
#'
#' sample the polytope corresponding to the CaNmod model
#' @param myCaNmod a CaNmod or a sampleCaNmod. In this latter case, adapative
#' phase and discarding phases won't be necessary
#' @param N the number of samples required
#' @param nchain the number of mcmc chains
#' @param ncore number of cores to use
#' @param thin thinning interval
#' @param method 1 of gibbs (default), 2 for hit and run, 3 for chrr
#' @param lastF should flow for last year be simulated (default = FALSE)
#' @return a sampleCaNmod object which contains three elements
#' \itemize{
#'  \item{"CaNmod"}{the CaNmod object descring the model}
#'  \item{"mcmc"}{\code{\link[coda]{mcmc.list}}}
#'  \item{"covMat"}{the estimate of the covariance matrix in the first chain
#'  which can be used to samples new iterations without adaptation nor
#'  discarding phase}
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
#' @importFrom Matrix Matrix
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom coda mcmc
#' @importFrom coda mcmc.list
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom stats runif
#' @importFrom lpSolveAPI get.rhs
sampleCaN <- function(myCaNmod,
                      N,
                      nchain = 1,
                      ncore = 1,
                      thin = 1,
                      method = 1,
                      lastF = FALSE) {
  if (inherits(myCaNmod, "sampleCaNmod")){
    covMat <- myCaNmod$covMat
    myCaNmod <- myCaNmod$CaNmod
  } else{
    covMat <- NULL
  }
  if (! method %in% seq_len(3))
    stop("method should be in 1:3")
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
  print("##Initializing")
  #we removed parameters that are fixed because of A or almost
  bounds <- getAllBoundsParam(list(A = as.matrix(myCaNmod$A),
                                   b = myCaNmod$b),
                                   progressBar = TRUE)
  
  #if some are fixed, we add them to C
  fixed <- which(abs(bounds[, 2] - bounds[, 3]) < 1e-5)
  if (length(fixed) > 0){
    myCaNmod$v <- c(myCaNmod$v, rowMeans(bounds[fixed, 2:3, drop = FALSE]))
    newC <- Matrix(0, length(fixed), ncol(myCaNmod$A))
    newC[cbind(seq_len(length(fixed)), fixed)] <- 1
    myCaNmod$C <- rbind(myCaNmod$C, newC)
  }
  
  #now we restric to the degenerate subspace
  if (nrow(myCaNmod$C) > 0){
    lpmod <- defineLPMod(myCaNmod$A,
                         myCaNmod$b, 
                         myCaNmod$C, 
                         myCaNmod$v,
                         maximum = FALSE)
    solequality <- ROI_solve(lpmod, 
                             solver = "lpsolve",
                             control = list(presolve = c("rows",
                                                         "lindep",
                                                         "rowdominate",
                                                         "mergerows")))$solution
    subspace <- degenerateSubSpace(as.matrix(myCaNmod$A),
                                   myCaNmod$b,
                                   as.matrix(myCaNmod$C),
                                   myCaNmod$v,
                                   solequality)
    A2 <- subspace$A2
    b2 <- subspace$b2
    Nt <- subspace$Nt
    
  } else { #no equality constraints, everything remains the same
    A2 <-myCaNmod$A
    b2 <- myCaNmod$b
    Nt <- diag(ncol(myCaNmod$A))
    solequality <- rep(0, ncol(myCaNmod$A))
  }
  
  #now we presolve the model to simply the polytope
  
  
  print("##Sampling")
  
  res <- foreach(i = 1:nchain) %myinfix% {

      # lp_model <- defineLPMod(myCaNmod$A, myCaNmod$b, myCaNmod$C, myCaNmod$v,
      #                         maximum = FALSE,
      #                         ob = runif(ncol(myCaNmod$A)))
      presolved <- presolveLPMod(A2,
                                 b2,
                                 lower = rep(-Inf, ncol(A2)),
                                 upper = rep(Inf, ncol(A2)),
                                 sense = "min")
      A3 <- presolved$A
      b3 <- presolved$b
      
      
      x0 <- chebyCenter(A3, 
                        b3,
                        lower = presolved$lower,
                        upper = presolved$upper)
      
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
      

    res <- cpgs(N,A3,b3,x0,thin,method,i,i,covMat)
    
    #now we turn back result into original format
    res$X <- cbind(res$X, 
                   matrix(rep(presolved$fixed, nrow(res$X)),
                          nrow(res$X),
                          length(presolved$fixed),
                          byrow = TRUE))
    colnames(res$X) <- c(colnames(A3), names(presolved$fixed))
    res$X <- res$X[, sort(colnames(res$X))]
    
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
    list(samples = mcmc(cbind(res$F, res$B), 1, nrow(res$F), 1),
         covMat = res$covMat)
  }

  if (ncore > 1) {
    stopCluster(cl)
    stopImplicitCluster()
  }
  sampleCaNmod <- list(CaNmod = myCaNmod,
                       mcmc = mcmc.list(lapply(res, function(r) r$samples)),
                       covMat  = res[[1]]$covMat)
  class(sampleCaNmod) <- "sampleCaNmod"
  return(sampleCaNmod)

}
