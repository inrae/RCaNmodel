#' generate_symbolic_objects
#'
#' This is an internal function that builds all the required symbolic objects
#' required for the computation of the model
#' @param flow names of the flow
#' @param species names of the species
#' @param ntstep number of the time step
#' @param H the H matrix of (I-H).B+N.F
#' @param N the N matrix of (I-H).B+N.F
#' @param B0 initial biomasses
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

generate_symbolic_objects <-
  function(flow, species, ntstep, H, N, B0, series) {
    years <- series$Year
    nbspec <- length(species)
    Ie <- diag(nbspec) #diagonal_matrix
    IE_H <- symengine::Matrix(Ie - H)
    n <- symengine::Matrix(N)
    assign(paste("B_", years[1], sep = ""), Vector(B0))
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
      )))[, 1] + (n %*% eval(parse(
        text = paste("F", t - 1, sep = "_")
      )))[, 1]) # biomass at time t+1 is B_t+1=(Ie-H)%*%B_t+N%*%F_t
      list_F <- c(list_F, eval(parse(text = paste("F", t, sep = "_"))))
      list_B <- c(list_B, eval(parse(text = paste("B", t, sep = "_"))))
    }

    assign("Fmat", do.call("cbind", list_F))
    colnames(Fmat) <- years
    assign("param",
           do.call("c", list_F)) #vector of flows on which we will sample
    assign("Bmat", do.call("cbind", list_B))
    colnames(Bmat) <- years
    param <- c(V(1), param) #we add an intercept

    for (is in 1:nbspec) {
      assign(species[is], Bmat[is, ]) #vectors of biomass named by species name
    }

    for (f in seq_len(length(flow))) {
      assign(flow[f], Fmat[f, ]) #vectors of flow named by flow name
    }

    for (s in names(series)[-1]) {
      ser <- series[, s]
      ser[is.na(ser)] <- NaN
      assign(s, Vector(ser))
    }
    rm(list = c(
      "H",
      "N",
      "flow",
      "species",
      "ntstep",
      "series",
      "B0",
      "f",
      "s",
      "t",
      "is"
    ))
    return(environment())
  }
