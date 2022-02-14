#' createDynamics
#'
#' builds H and N matrix that relate biomass at time step t+1 to fluxes and
#' biomasses at time t in the case of non standard RCaN model. This function
#' is not called by user but by buildCaN.
#' @param dynamics_equation a string that specifies the dynamics equation
#' @param components the components data frame
#' @param fluxes the fluxes data frame
#'
#' @return a list with H and N matrices
#' @importFrom symengine expand
#' @importFrom symengine S
#' @importFrom stringr str_replace_all
#' @importFrom stats setNames
#'
createDynamics <- function(dynamics_equation,
                           components,
                           fluxes) {
  species <- components$Component[which(components_param$Inside == 1)]
  nbfluxes <- nrow(fluxes)

  H <- diag(1,
            nrow=length(species))
  nbspecies <- length(species)
  N <- matrix(0, nbspecies, nbfluxes)

  rownames(N) <- species
  colnames(N) <- fluxes$Flux
  colnames(H) <- rownames(H) <- species


  #create symbolic variables
  for (sp in species)
    assign(sp,
           S(sp))
  for (f in fluxes$Flux)
    assign(f,
           S(f))

  prop <- names(components)
  prop <- prop[!prop %in% (c("Component", "Inside"))]

  for (p in prop){
    parameters <- components_param[, p]
    parameters <- setNames(parameters, components$Component)
    assign(p, parameters)
  }

  #build matrix
  for (sp in species){
    #contribution of component
    equation <- gsub("Component",
                     sp,
                     dynamics_equation[1])
    for (p in prop){
      equation <- str_replace_all(equation,
                                  paste0("(\\b|\\(|\\))",
                                         p,
                                         "(\\b|\\(|\\))"),
                                  function (m) gsub(p,
                                                    paste0(p,"['", sp, "']"),
                                                    m))
    }


    #contribution of inflows
    if (grepl("Inflow", dynamics_equation[2])){
      dynamics <- dynamics_equation[2]
      dynamics <- gsub("sum", "", dynamics)
      dynamics <- str_replace_all(dynamics,
                                  "Inflow",
                                  fluxes$Flux[fluxes$To == sp])

      for (id in seq_len(length(dynamics))){
        for (p in prop){
          dynamics[id] <- str_replace_all(dynamics[id],
                                          paste0("(\\b|\\(|\\))",
                                                 p,
                                                 "(\\b|\\(|\\))"),
                                          function (m) gsub(p,
                                                            paste0(p,
                                                                   "['",
                                                                   sp, "']"),
                                                            m))
          source <- fluxes$From[fluxes$To == sp][id]
          dynamics[id] <- str_replace_all(dynamics[id],
                                          paste0("(\\b|\\(|\\))",
                                                 p,
                                                 "_source(\\b|\\(|\\))"),
                                          function (m) gsub(paste0(p,
                                                                   "_source"),
                                                            paste0(
                                                              p,
                                                              "['",
                                                              source,
                                                              "']"),
                                                            m)
          )
        }
      }
      dynamics <- paste(dynamics, collapse ="+")
      equation <- paste(equation, dynamics, sep = "+")
    }

    #contribution of outflows
    if (grepl("Outflow", dynamics_equation[3])){
      dynamics <- dynamics_equation[3]
      dynamics <- gsub("sum", "", dynamics)
      dynamics <- str_replace_all(dynamics,
                                  "Outflow",
                                  fluxes$Flux[fluxes$From == sp])

      for (id in seq_len(length(dynamics))){
        for (p in prop){
          dynamics[id] <- str_replace_all(dynamics[id],
                                          paste0("(\\b|\\(|\\))",
                                                 p,
                                                 "(\\b|\\(|\\))"),
                                          paste0(p, "['", sp, "']"))
          sink <- fluxes$To[fluxes$From == sp][id]
          dynamics[id] <- str_replace_all(dynamics[id],
                                          paste0("(\\b|\\(|\\))",
                                                 p,
                                                 "_sink(\\b|\\(|\\))"),
                                          function (m) gsub(p,
                                                            paste0(
                                                              p,
                                                              "_sink['",
                                                              sink,
                                                              "']"),
                                                            m))
        }
      }
      dynamics <- paste(dynamics, collapse ="+")
      equation <- paste(equation, dynamics, sep = "+")
    }

    equation <- expand(eval(parse(text = equation)))

    mycoeffs <- sapply(as.list(get_args(equation)), function(e) {
      if (get_type(e) == "RealDouble") {
        return(c("1" = as.numeric(e)))
      } else if (get_type(e) == "Symbol") {
        val <- 1
        names(val) <- get_str(e)
        return(val)
      } else if (get_type(e) == "NaN") {
        return(c("1" = NA))
      } else {
        val <- as.list(get_args(e))[[1]]
        val <- ifelse(get_type(val) == "NaN",
                      NA,
                      as.numeric(val))
        names(val) <- get_str(as.list(get_args(e))[[2]])
        return(val)
      }
    })
  H[sp, match(names(mycoeffs),
              colnames(H),
              nomatch = 0)] <- mycoeffs[match(names(mycoeffs),
                                              colnames(H),
                                              nomatch = 0)]
  N[sp, match(names(mycoeffs),
              colnames(N),
              nomatch = 0)] <- mycoeffs[match(names(mycoeffs),
                                              colnames(N),
                                              nomatch = 0)]
  }
  H <- diag(nbspecies) - H #since Bt+1=(I-H)*Bt

  return(list(H = H,
              N = N))
}
