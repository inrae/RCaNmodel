#' generateDerivedSymbolicObjects
#'
#' This is an internal function that builds derived symbolic objects
#' required for the computation of the model
#' @param param namesof param
#' @param env the environment to be updated
#' @param before whether to generate vector NA 1:T-1
#' @param after whether to generate vector 2:T NA
#' @param delta whether to generate Yt+1-Yt
#' @param beforedelta whether to generate Yt-Yt-1
#' @param ratio whether to generate Yt+1/Yt
#' @param beforeratio whether to generate Yt/Yt-1
#'
#' @return nothing but update the environment
#' @importFrom symengine Vector
#' @importFrom symengine S
#' @importFrom symengine V
#' @importFrom dplyr pull

generateDerivedSymbolicObjects <-
  function(param,
           env,
           before = FALSE,
           after = FALSE,
           delta = FALSE,
           beforedelta = FALSE,
           ratio = FALSE,
           beforeratio = FALSE) {
    obj <- get(param, envir = env)
    if (before) {
      if (exists(paste0("Before",param), envir = env))
        stop(paste("object", paste0("Before",param), "already exists"))
      assign(paste0("Before",param),
             rev(c(rev(obj[-length(obj)]), NaN)),
             envir = env)
    }
    if (after) {
      if (exists(paste0("After",param), envir = env))
        stop(paste("object", paste0("After",param), "already exists"))
      assign(paste0("After",param),
             c(obj[-1], NaN),
             envir = env)
    }
    if (delta) {
      if (exists(paste0("Delta", param), envir = env))
        stop(paste("object", paste0("Delta", param), "already exists"))
      assign(paste0("Delta", param),
             c(obj[-1] - obj[-length(obj)], NaN),
             envir = env)
    }
    if (ratio) {
      if (exists(paste0("Ratio", param), envir = env))
        stop(paste("object", paste0("Ratio", param), "already exists"))
      assign(paste0("Ratio", param),
             c(obj[-1] / obj[-length(obj)], NaN),
             envir = env)
    }


    if (beforedelta) {
      if (exists(paste0("Before", "Delta", param), envir = env))
        stop(paste("object", paste0("Before","Delta", param), "already exists"))
      assign(paste0("Before","Delta", param),
             rev(c(rev(obj[-1] - obj[-length(obj)]), NaN)),
             envir = env)
    }
    if (beforeratio) {
      if (exists(paste0("Before", "Ratio", param), envir = env))
        stop(paste("object", paste0("Before", "Ratio", param), "already exists"))
      assign(paste0("Before", "Ratio", param),
             rev(c(rev(obj[-1] / obj[-length(obj)]), NaN)),
             envir = env)
    }


  }
