#' generateDerivedSymbolicObjects
#'
#' This is an internal function that builds derived symbolic objects
#' required for the computation of the model
#' @param param namesof param
#' @param env the environment to be updated
#' @param M whether to generate vector 2:T NA
#' @param P whether to generate vector NA 1:t-1
#' @param delta whether to generate Yt+1-Yt
#' @param deltaM whether to generate Yt-Yt-1
#' @param ratio whether to generate Yt+1/Yt
#' @param ratioM whether to generate Yt/Yt-1
#'
#' @return nothing but update the environment
#' @importFrom symengine Vector
#' @importFrom symengine S
#' @importFrom symengine V
#' @importFrom dplyr pull

generateDerivedSymbolicObjects <-
  function(param,
           env,
           M = FALSE,
           P = FALSE,
           delta = FALSE,
           deltaM = FALSE,
           ratio = FALSE,
           ratioM = FALSE) {
    obj <- get(param, envir = env)
    if (M) {
      if (exists(paste0(param, "M"), envir = env))
        stop(paste("object", paste0(param, "M"), "already exists"))
      assign(paste0(param, "M"),
             rev(c(rev(obj[-length(obj)]), NaN)),
             envir = env)
    }
    if (P) {
      if (exists(paste0(param, "P"), envir = env))
        stop(paste("object", paste0(param, "P"), "already exists"))
      assign(paste0(param, "P"),
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


    if (deltaM) {
      if (exists(paste0("Delta", param, "M"), envir = env))
        stop(paste("object", paste0("Delta", param, "M"), "already exists"))
      assign(paste0("Delta", param, "M"),
             rev(c(rev(obj[-1] - obj[-length(obj)]), NaN)),
             envir = env)
    }
    if (ratioM) {
      if (exists(paste0("Ratio", param, "M"), envir = env))
        stop(paste("object", paste0("Ratio", param, "M"), "already exists"))
      assign(paste0("Ratio", param, "M"),
             rev(c(rev(obj[-1] / obj[-length(obj)]), NaN)),
             envir = env)
    }


  }
