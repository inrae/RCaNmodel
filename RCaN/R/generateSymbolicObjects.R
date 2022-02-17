#' generateSymbolicObjects
#'
#' This is an internal function that builds all the required symbolic objects
#' required for the computation of the model
#' @param fluxes_def fluxes_def
#' @param species names of the species
#' @param ntstep number of the time step
#' @param H the H matrix of (I-H).B+N.F
#' @param N the N matrix of (I-H).B+N.F
#' @param series the names of the series
#'
#' @return en environment storing all symbolic elements including among others
#' \itemize{
#'  \item{"IE_H"}{identity- H the matrix of the equation (Bt+1=(I-H).B+N.F)}
#'  \item{"n"}{the N matrix of the equation (Bt+1=(I-H).B+N.F)}
#'  \item{"B_0"}{initial biomasses}
#'  \item{"species"}{a vector of biomasse per time step for each species}
#'  \item{"flow"}{a vector of biomasse per time step for each flow}
#' }
#' @importFrom symengine Vector
#' @importFrom symengine S
#' @importFrom symengine V
#' @importFrom dplyr pull

generateSymbolicObjects <-
  function(fluxes_def, species, ntstep, H, N, series) {
    flow <- fluxes_def$Flux
    years <- series$Year
    nbspec <- length(species)
    Ie <- diag(nbspec) #diagonal_matrix
    IE_H <- symengine::Matrix(Ie - H)
    n <- symengine::Matrix(N)
    for (s in species)
      assign(paste(s, years[1], sep = "_"),
             S(paste(s, years[1], sep = "_"))) #initial biomass
    assign(paste("B_", years[1], sep = ""),
           eval(parse(text = paste(
             "Vector(", paste(species, years[1], sep = "_", collapse = ","), ")"
           ))))  #symbolic vector B_0 (all biomasses for time step 0)
    for (f in flow)
      assign(paste(f, years[1], sep = "_"),
             S(paste(f, years[1], sep = "_"))) #symbolic flow for time step 0
    assign(paste("F_", years[1], sep = ""),
      eval(parse(text = paste(
        "Vector(", paste(flow, years[1], sep = "_", collapse = ","), ")"
      )))) #symbolic vector F_0 (all fluxes for time step 0)
    list_F <- list(eval(parse(text = paste("F", years[1], sep = "_"))))
    list_B <- list(eval(parse(text = paste("B", years[1], sep = "_"))))
    for (t in years[-1]) {
      for (f in flow) {
        assign(paste(f, t, sep = "_"),
               S(paste(f, t, sep = "_"))) #symbolic fluxes for time step t

      }
      assign(paste("F", t, sep = "_"), eval(parse(text = paste(
        "Vector(", paste(flow, t, sep = "_", collapse = ","), ")"
      )))) #vector of fluxes for time step t
      assign(paste("B", t, sep = "_"), (IE_H %*% eval(parse(
        text = paste("B", t - 1, sep = "_")
      )))[, 1,drop=FALSE] + (n %*% eval(parse(
        text = paste("F", t - 1, sep = "_")
      )))[, 1]) # biomass at time t+1 is B_t+1=(Ie-H)%*%B_t+N%*%F_t
      list_F <- c(list_F, eval(parse(text = paste("F", t, sep = "_"))))
      list_B <- c(list_B, eval(parse(text = paste("B", t, sep = "_"))))
    }


    assign("Fmat", do.call("cbind", list_F))
    colnames(Fmat) <- years
    assign("param",
           c(eval(parse(text = paste("B_", years[1], sep = ""))),
             do.call("c", list_F))) #vector of parameters
                                    #on which we will sample
    assign("Bmat", do.call("cbind", list_B))
    colnames(Bmat) <- years
    param <- c(V(1), param) #we add an intercept


    for (sp in species) {
      isp <- which(sp == species)
      inflow <- which(fluxes_def$To == sp)
      outflow <- which(fluxes_def$From == sp)
      intemp <- rep(0, length(years))
      outtemp <- rep(0, length(years))
      for (i in inflow)
        intemp <- intemp + Fmat[i, ]
      for (i in outflow)
        outtemp <- outtemp + Fmat[i, ]
      assign(paste0("Inflows", sp),
             intemp)
      assign(paste0("Outflows", sp),
             outtemp)
      assign(paste0("Ratio", sp),
             c(Bmat[isp, -1] / Bmat[isp, -length(years)], NaN))
      assign(paste0("RatioM", sp),
             rev(c(rev(Bmat[isp, -1] / Bmat[isp, -length(years)]), NaN)))
      assign(paste0("Delta", sp),
             c(Bmat[isp, -1] - Bmat[isp, -length(years)], NaN))
      assign(paste0("DeltaM", sp),
             rev(c(rev(Bmat[isp, -1] - Bmat[isp, -length(years)]), NaN)))
    }


    for (f in flow) {
      ifl <- which(flow == f)
      assign(paste0("Ratio", f),
             c(Fmat[ifl, -1] / Fmat[ifl, -length(years)], NaN))
      assign(paste0("RatioM", f),
             rev(c(rev(Fmat[ifl, -1] / Fmat[ifl, -length(years)]), NaN)))
      assign(paste0("Delta", f),
             c(Fmat[ifl, -1] - Fmat[ifl, -length(years)], NaN))
      assign(paste0("DeltaM", f),
             rev(c(rev(Fmat[ifl, -1] - Fmat[ifl, -length(years)]), NaN)))
    }






    for (is in 1:nbspec) {
      assign(species[is], Bmat[is, ]) #vectors of biomass named by species name
    }

    for (f in seq_len(length(flow))) {
      assign(flow[f], Fmat[f, ]) #vectors of flow named by flow name
    }

    for (s in names(series)[-1]) {
      ser <- pull(series, s)
      ser[is.na(ser)] <- NaN
      assign(s, Vector(ser))
    }
    rm(list = c(
      "H",
      "N",
      "flow",
      "species",
      "series",
      "f",
      "s",
      "t",
      "is"
    ))
    return(environment())
  }
