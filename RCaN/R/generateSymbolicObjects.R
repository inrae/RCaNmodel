#' generateSymbolicObjects
#'
#' This is an internal function that builds all the required symbolic objects
#' required for the computation of the model
#' @param components the components data frame
#' @param fluxes_def fluxes_def
#' @param ntstep number of the time step
#' @param series the names of the series
#' @param aliases table of alias (default = NULL)
#' @param dynamics_equation a string that specifies the dynamics equation (NULL
#' if trophic model)
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
  function(components,
           fluxes_def,
           ntstep,
           series,
           aliases,
           dynamics_equation = NULL) {
    species <- components$Component[which(components$Inside == 1)]
    flow <- fluxes_def$Flux
    years <- series$Year
    nbspec <- length(species)
    Ie <- diag(nbspec) #diagonal_matrix

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

    matrices <- createDynamics(dynamics_equation, components, fluxes_def)
    H <- matrices$H
    N <- matrices$N
    Nend <- matrices$Nend

    IE_H <- symengine::Matrix(Ie - H)
    n <- symengine::Matrix(N)
    nend <- symengine::Matrix(Nend)

    #loop over time step
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

      #biomass at the end of the time step
      assign(paste("BEnd", t - 1, sep = "_"), (IE_H %*% eval(parse(
        text = paste("B", t - 1, sep = "_")
      )))[, 1,drop=FALSE] + (nend %*% eval(parse(
        text = paste("F", t - 1, sep = "_")
      )))[, 1]) # biomass at time t+1 is B_t+1=(Ie-H)%*%B_t+N%*%F_t

      list_F <- c(list_F, eval(parse(text = paste("F", t, sep = "_"))))
      list_B <- c(list_B, eval(parse(text = paste("B", t, sep = "_"))))
      if ((t - 1) == years[1]){
        list_BEnd <- eval(parse(text = paste("BEnd", t - 1, sep = "_")))
      } else {
        list_BEnd <- c(list_BEnd,
                       eval(parse(text = paste("BEnd", t - 1, sep = "_"))))
      }
    }


    assign("Fmat", do.call("cbind", list_F))
    colnames(Fmat) <- years
    assign("param",
           c(eval(parse(text = paste("B_", years[1], sep = ""))),
             do.call("c", list_F))) #vector of parameters
                                    #on which we will sample
    assign("Bmat", do.call("cbind", list_B))
    colnames(Bmat) <- years

    assign("BmatEnd", do.call("cbind", list_BEnd))
    colnames(BmatEnd) <- years

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
      generateDerivedSymbolicObjects(paste0("Inflows", sp),
                                     environment(),
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
      generateDerivedSymbolicObjects(paste0("Outflows", sp),
                                     environment(),
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
    }


    for (sp in species) {
      for (p in names(fluxes_def)[-(1:3)]){
        isp <- which(sp == species)
        inflow <- which(fluxes_def$To == sp & fluxes_def[, p])
        outflow <- which(fluxes_def$From == sp & fluxes_def[, p])
        intemp <- rep(0, length(years))
        outtemp <- rep(0, length(years))
        for (i in inflow)
          intemp <- intemp + Fmat[i, ]
        for (i in outflow)
          outtemp <- outtemp + Fmat[i, ]
        assign(paste0(p, "Inflows", sp),
               intemp)
        generateDerivedSymbolicObjects(paste0(p, "Inflows", sp),
                                       environment(),
                                       M = TRUE,
                                       P = TRUE,
                                       ratio = TRUE,
                                       ratioM = TRUE,
                                       delta = TRUE,
                                       deltaM = TRUE)

        assign(paste0(p, "Outflows", sp),
               outtemp)
        generateDerivedSymbolicObjects(paste0(p, "Outflows", sp),
                                       environment(),
                                       M = TRUE,
                                       P = TRUE,
                                       ratio = TRUE,
                                       ratioM = TRUE,
                                       delta = TRUE,
                                       deltaM = TRUE)
      }
    }









    for (is in seq_len(nbspec)) {
      assign(species[is], Bmat[is, ]) #vectors of biomass named by species name

      #vectors of biomass named by species name at end of tstep
      #we add a nan since the vector has no element for last time step
      assign(paste0(species[is],
                    "End"),
             c(BmatEnd[is, ], NaN))
      generateDerivedSymbolicObjects(species[is],
                                     environment(),
                                     M = TRUE,
                                     P = TRUE,
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
      generateDerivedSymbolicObjects(paste0(species[is], "End"),
                                     environment(),
                                     M = TRUE,
                                     P = TRUE,
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
    }

    for (f in seq_len(length(flow))) {
      assign(flow[f], Fmat[f, ]) #vectors of flow named by flow name
      generateDerivedSymbolicObjects(flow[f],
                                     environment(),
                                     M = TRUE,
                                     P = TRUE,
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
    }

    for (s in names(series)[-1]) {
      ser <- pull(series, s)
      ser[is.na(ser)] <- NaN
      assign(s, Vector(ser))
      generateDerivedSymbolicObjects(s,
                                     environment(),
                                     M = TRUE,
                                     P = TRUE,
                                     ratio = TRUE,
                                     ratioM = TRUE,
                                     delta = TRUE,
                                     deltaM = TRUE)
    }
    if (!is.null(aliases)){
      for (i in seq_len(nrow(aliases))){
        assign(aliases[i, 1],
               aliases[i, 2])
        generateDerivedSymbolicObjects(aliases[i, 1],
                                       environment(),
                                       M = TRUE,
                                       P = TRUE,
                                       ratio = TRUE,
                                       ratioM = TRUE,
                                       delta = TRUE,
                                       deltaM = TRUE)

      }
    }
    rm(list = c(
      "H",
      "flow",
      "species",
      "series",
      "f",
      "s",
      "t",
      "isp",
      "sp",
      "inflow",
      "outflow",
      "intemp",
      "outtemp",
      "years",
      "ser",
      "i",
      "n",
      "nbspec"

    ))
    return(environment())
  }
