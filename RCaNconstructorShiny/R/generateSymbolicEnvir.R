#' generateSymbolicEnvir
#'
#' @param network the network object
#'
#' @return an environment with symbolic environment
#' @importFrom symengine S Vector
#' @export
#'

generateSymbolicEnvir <- function(network){
  components <- network$components
  fluxes_def <- network$fluxes
  series <- network$observations
  aliases <- network$aliases
  species <- components$Component[which(network$components$Inside == 1)]
  flow <- fluxes_def$Flux
  years <- series$Year
  nbspec <- length(species)

  generateDerivedSymbolicObjects <- RCaNmodel:::generateDerivedSymbolicObjects

  genVector <- function(param)
    Vector(lapply(years,
                  function(y) S(paste0(param,
                  y))))

  ## alias for inflows and outflows
  for (sp in species) {
    assign(paste0("Inflows", sp),
           genVector(paste0("Inflows", sp)))
    assign(paste0("Outflows", sp),
           genVector(paste0("Outflows", sp)))
    generateDerivedSymbolicObjects(paste0("Inflows", sp),
                                   environment(),
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
    generateDerivedSymbolicObjects(paste0("Outflows", sp),
                                   environment(),
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
  }

  ## alias for trophic inflows and trophic outflows
  for (sp in species) {
    for (p in names(fluxes_def)[-(1:3)]){
      assign(paste0(p, "Inflows", sp),
             genVector(paste0(p, "Inflows", sp)))
      generateDerivedSymbolicObjects(paste0(p, "Inflows", sp),
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)

      assign(paste0(p, "Outflows", sp),
             genVector(paste0(p, "Outflows", sp)))
      generateDerivedSymbolicObjects(paste0(p, "Outflows", sp),
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
    }
  }

  ## alias for all flows
  assign("Allflows", genVector("Allflows"))
  generateDerivedSymbolicObjects("Allflows",
                                 environment(),
                                 before = TRUE,
                                 after = TRUE,
                                 ratio = TRUE,
                                 beforeratio = TRUE,
                                 delta = TRUE,
                                 beforedelta = TRUE)
  for (p in names(fluxes_def)[-(1:3)]){
    assign(paste0(p, "Allflows"), genVector(paste0(p, "Allflows")))
    generateDerivedSymbolicObjects(paste0(p, "Allflows"),
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)


  }



  for (is in seq_len(nbspec)) {
    assign(species[is],
           genVector(species[is])) #vectors of biomass named by species name

    #vectors of biomass named by species name at end of tstep
    #we add a NaN since the vector has no element for last time step
    assign(paste0(species[is],
                  "End"),
           genVector(paste0(species[is],
                            "End")))

    generateDerivedSymbolicObjects(species[is],
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
    generateDerivedSymbolicObjects(paste0(species[is], "End"),
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
  }

  for (f in seq_len(length(flow))) {
    assign(flow[f],
           genVector(flow[f])) #vectors of flow named by flow name
    generateDerivedSymbolicObjects(flow[f],
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
  }

  # this will be useful to have time varying parameters: they come either
  # from the component sheet or can be overwritten by the time series sheet
  for (sp in components$Component){
    for (p in names(components)[-1]){
      assign(paste0(sp, p),
             genVector(paste0(sp, p))
      )
    }
  }

  for (s in names(series)[-1]) {
    ser <- pull(series, s)
    ser[is.na(ser)] <- NaN
    assign(s, ser)
    generateDerivedSymbolicObjects(s,
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
  }
  for (i in seq_len(nrow(aliases))){
    assign(aliases$Alias[i],
           genVector(aliases$Alias[i]))
    generateDerivedSymbolicObjects(aliases$Alias[i],
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)

  }
  rm("network", "i", "nbspec", "genVector", "s", "f", "is")
  return(environment())

}
