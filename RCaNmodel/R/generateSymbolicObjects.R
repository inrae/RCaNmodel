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
#' @param stanza_species a vector with the names of multistazas component
#' @param stanza a list with the parameters of the stanzas
#' @param fluxes_stanza a list with the flow names and the subflows for stanzas
#'
#' @return en environment storing all symbolic elements 
#' 
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
           dynamics_equation = NULL,
           stanza_species,
           stanza,
           fluxes_stanza) {
    
    ########## Initialisation ----    
    
    species <- components$Component[which(components$Inside == 1)]
    
    componentsall <- components
    if (length(stanza) > 0)
      componentsall <- dplyr::bind_rows(componentsall,
                                        do.call(bind_rows, stanza))
    speciesall <- componentsall$Component[which(componentsall$Inside  == 1)]
    
    flow <- fluxes_def$Flux
    nbfluxes <- length(flow)
    years <- series$Year
    
    groups <- speciesall[!speciesall %in% stanza_species]
    nbgroups <- length(groups)
    Ie <- diag(nbgroups) #diagonal_matrix
    
    
    fluxes_to <- match(fluxes_def$To, groups)
    fluxes_from <- match(fluxes_def$From, groups)
    is_trophic_flux <- fluxes_def$Trophic == 1
    
    ########## Stages transition ----    
    previousgroups <- list()
    for (s in species){
      if (s %in% stanza_species){
        gcomponents <- stanza[[s]]$Component
        for (igs in seq_len(length(gcomponents))){
          gs <- gcomponents[igs]
          if (igs == 1){
            previousgroups[gs] <- paste0("Recruitment", gs)
          } else if (igs == length(gs)){
            previousgroups[gs] <- c(gs, gcomponents[igs-1])
          } else {
            previousgroups[gs] <- gcomponents[igs-1]
          }
        }
      } else {
        previousgroups[s] <- s
      }
    }
    
    
    ########## Construction of matrices Fmat and Bmat ----    
    Bmat <- symengine::Matrix(0, length(groups), nrow(series))
    Bmat[, 1] <- symengine::Vector(paste(groups, years[1], sep = "_"))
    
    Fmat <- symengine::Matrix(0, nrow(fluxes_def), nrow(series))
    for (ifl in seq_len(length(flow))){
      Fmat[ifl, ] <- symengine::Vector(paste(flow[ifl], years, sep = "_"))
    }
    
    Bend <- symengine::Matrix(0, length(groups), nrow(series))
    
    rownames(Fmat) <- flow
    rownames(Bmat) <- rownames(Bend) <- groups
    colnames(Bmat) <- colnames(Bend) <- colnames(Fmat) <- years
    
    #################Creation of standard flow aliases ----
    
    ## alias for inflows and outflows
    for (sp in componentsall$Component) {
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
    
    ## alias for trophic/param inflows and trophic/param outflows
    for (sp in speciesall) {
      for (p in names(fluxes_def)[-(1:3)]){
        spgroups <- sp
        if (sp %in% stanza_species)
          spgroups <- stanza[[sp]]$Component
        inflow <- which(fluxes_def$To %in% spgroups & fluxes_def[, p])
        outflow <- which(fluxes_def$From %in% spgroups & fluxes_def[, p])
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
                                       before = TRUE,
                                       after = TRUE,
                                       ratio = TRUE,
                                       beforeratio = TRUE,
                                       delta = TRUE,
                                       beforedelta = TRUE)
        
        assign(paste0(p, "Outflows", sp),
               outtemp)
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
    allflow <- rep(0, length(years))
    for (i in seq_len(nrow(Fmat))){
      allflow <- allflow + Fmat[i, ]
    }
    assign("Allflows", allflow)
    generateDerivedSymbolicObjects("Allflows",
                                   environment(),
                                   before = TRUE,
                                   after = TRUE,
                                   ratio = TRUE,
                                   beforeratio = TRUE,
                                   delta = TRUE,
                                   beforedelta = TRUE)
    for (p in names(fluxes_def)[-(1:3)]){
      allflow <- rep(0, length(years))
      for (i in which(fluxes_def[, p] == 1)){
        allflow <- allflow + Fmat[i, ]
      }
      assign(paste0(p, "Allflows"), allflow)
      generateDerivedSymbolicObjects(paste0(p, "Allflows"),
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
      
      
    }
    
    for (f in seq_len(length(flow))) {
      assign(flow[f], Fmat[f, ]) #vectors of flow named by flow name
      generateDerivedSymbolicObjects(flow[f],
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
    }
    
    for (f in names(fluxes_stanza)){
      assign(f, eval(parse(text = paste(fluxes_stanza[[f]], collapse = "+"))))
      generateDerivedSymbolicObjects(f,
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
    }
    
    ######## Creation of components parameters symbolics ----
    # this will be useful to have time varying parameters: they come either
    # from the component sheet or can be overwritten by the time series sheet
    for (sp in components$Component){
      for (param in names(components)[-1]){
        assign(paste0(sp, param),
               rep(components[components$Component == sp, param],
                   nrow(series)))
        if (sp %in% stanza_species){
          for (sp2 in stanza[[sp]]$Component){
            assign(paste0(sp2, param),
                   rep(components[components$Component == sp, param],
                       nrow(series)))
          }
        }
      }
    }
    
    #for stanza group if a parameter is defined at the group level, we overwrite
    #the general parameter
    for (sp in stanza_species){
      for (param in names(stanza[[sp]])[-1]){
        for (isp2 in seq_len(nrow(stanza[[sp]]))){
          if (!is.na(stanza[[sp]][isp2,param])){
            assign(paste0(stanza[[sp]][isp2, "Component"], param),
                   rep(dplyr::pull(stanza[[sp]][isp2, param]),
                       nrow(series)))
          }
        }
      }
    }
    
    #series can overwrite parameters values
    for (s in names(series)[-1]) {
      ser <- dplyr::pull(series, s)
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
    
    ######## Dynamic computation ----
    #loop over time step
    for (t in years) {
      i <- which(years == t)
      
      ##### computation of Bmat based on Bend and group transition -----
      if (i > 1){
        for (ig in seq_len(nrow(Bmat))){
          g <- groups[ig]
          or <- which(groups %in% previousgroups[g])
          if (length(or) == 0) {
            #no origin, so biomass comes from recruitment
            Bmat[ig ,i] <- eval(parse(text = paste0("Recruitment", g)))[i]
          } else { 
            Bmat[ig ,i] <- sum(Bend[or, i-1])
          }
        }
      }
      
      #computation of temporary matrices
      H <- diag(1, nbgroups)
      N <- matrix(0, nbgroups, nrow(fluxes_def))
      
      rownames(N) <- groups
      colnames(N) <- paste(fluxes_def$Flux, t, sep = "_")
      colnames(H) <- rownames(H) <- groups
      
      ##### Create Dynamics: H and N matrices at t-----
      if (is.null(dynamics_equation)) {
        ###### Case for standard trophic model ----
        otherLosses <- sapply(groups, function(s){
          get(paste0(s, "OtherLosses"))[i]
        })
        assimE <- mapply(function(tro, s){
          if (!tro) {
            return (1)
          } else {
            get(paste0(s, "AssimilationE"))[i]
          }
        }, fluxes_def$Trophic, fluxes_def$To)
        digestib <- mapply(function(tro, s){
          if (!tro) {
            return (1)
          } else {
            get(paste0(s, "Digestibility"))[i]
          }
        }, is_trophic_flux, fluxes_def$From)
        
        
        
        H <- diag(1 - exp(-otherLosses),
                  nrow=nbgroups)
        N[cbind(fluxes_from, seq_len(nbfluxes))] <- -1 #this is an outgoing flow
        N[na.omit(cbind(fluxes_to, seq_len(nbfluxes)))] <-
          na.omit(
            N[cbind(fluxes_to, seq_len(nbfluxes))] + ifelse(
              is_trophic_flux,
              assimE *
                digestib,
              1
            )
          ) #if it is not a trophic flow, we do not take into account assimilation
        # and digestibility
        N <-
          sweep(N, 1, STATS = diag(H) /
                  (otherLosses), "*")
      } else {
        ###### Case for generic model ----
        #create symbolic variables
        Component = S("Component")
        prop <- names(componentsall)
        prop <- prop[!prop %in% (c("Component", "Inside"))]
        
        
        
        #build matrix
        for (sp in groups){
          for (p in prop){
            assign(p, get(paste0(sp, p))[i])
          }
          #contribution of component
          equation <- eval(parse(text = dynamics_equation[1]))
          
          
          #contribution of inflows
          if (grepl("Inflow", dynamics_equation[2])){
            Inflow <- Fmat[which(fluxes_def$To == sp), i]
            
            #we create symbolic object for properties of flow
            for (fprop in names(fluxes_def)[-(1:3)])
              assign(fprop, as.logical(fluxes_def[fluxes_def$To == sp, fprop]))
            
            #we also get properties of the source of the flow
            #useful for example for Digestibility_source
            for (p in prop){
              assign(paste0(p, "_source"),
                     sapply(fluxes_def$From[fluxes_def$To == sp],
                            function(so) get(paste0(so, p))[i]))
            }
            dynamics <- dynamics_equation[2]
            equation <- equation + eval(parse(text = dynamics))
          }
          
          #contribution of outflows
          if (grepl("Outflow", dynamics_equation[3])){
            Outflow <- Fmat[which(fluxes_def$From == sp), i]
            
            #we create symbolic object for properties of flow
            for (fprop in names(fluxes_def)[-(1:3)])
              assign(fprop, as.logical(fluxes_def[fluxes_def$From == sp,
                                                  fprop]))
            
            #useful if the contribution depends on the destination of the flow
            for (p in prop){
              assign(paste0(p, "_sink"),
                     sapply(fluxes_def$To[fluxes_def$Fom == sp],
                            function(si) get(paste0(si, p))[i]))
            }
            dynamics <- dynamics_equation[3]
            equation <- equation + eval(parse(text = dynamics))
          }
          
          equation <- symengine::expand(equation)
          
          if (get_type(equation) != "Add") {
            if (get_type(equation) == "Symbol") {
              mycoeffs <- 1
              names(mycoeffs) <- get_str(equation)
            } else if (get_type(equation) == "Mul") {
              mycoeffs <- as.numeric(as.list(get_args(equation))[[1]])
              names(mycoeffs) <- get_str(as.list(get_args(equation))[[2]])
            } else if (get_type(equation) == "NaN") {
              mycoeffs <- NA
              names(mycoeffs) <- "1"
            }else{
              mycoeffs <- as.numeric(equation)
              names(mycoeffs) <- "1"
            }
          } else {
            
            mycoeffs <- sapply(as.list(get_args(equation)), function(e) {
              if (get_type(e) %in% c("RealDouble", "Symbol")) {
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
          }
          
          names(mycoeffs)[names(mycoeffs) == "Component"] <- sp
          H[sp, match(names(mycoeffs),
                      colnames(H),
                      nomatch = 0)] <- mycoeffs[names(mycoeffs) %in% colnames(H)]
          
          
          
          N[sp, match(names(mycoeffs),
                      colnames(N),
                      nomatch = 0)] <- mycoeffs[names(mycoeffs) %in% colnames(N)]
          
        }
        
        
        H <- diag(nbgroups) - H #since Bt+1=(I-H)*Bt
      }
      Nend <- N  #matrix to compute biomasses at the end of time step
      if ("End" %in% names(fluxes_def)[-(1:3)]){
        Nend[, fluxes_def$End == 1, drop = FALSE] <- 0
      }
      
      ##### computation of Bend -----
      
      
      
      
      IE_H <- symengine::Matrix(Ie - H)
      
      # Biomass at the end of the time step
      Bend[, i] <- IE_H %*% Bmat[, i,drop=FALSE] + 
        (symengine::Matrix(N) %*% Fmat[, i, drop = FALSE])
    }
    
    ##### Finalizing stuffs -----
    rownames(Fmat) <- flow
    rownames(Bmat) <- rownames(Bend) <- groups
    colnames(Bmat) <- colnames(Bend) <- colnames(Fmat) <- years
    
    param <- c(Bmat[, 1], as.vector(Fmat))
    param <- c(V(1), param) #we add an intercept
    
    
    
    for (is in seq_len(length(groups))) {
      assign(groups[is], Bmat[is, ]) #vectors of biomass named by species name
      
      #vectors of biomass named by species name at end of tstep
      #we add a NaN since the vector has no element for last time step
      assign(paste0(groups[is],
                    "End"),
             c(Bend[is, ], NaN))
      generateDerivedSymbolicObjects(groups[is],
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
      generateDerivedSymbolicObjects(paste0(groups[is], "End"),
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
    }
    #we assign aliases for biomass of stanza component
    for (s in stanza_species){
      assign(s, eval(parse(text=paste(stanza[[s]]$Component,
                                      collapse = "+")))) #vectors of biomass named by species name
      
      #vectors of biomass named by species name at end of tstep
      #we add a NaN since the vector has no element for last time step
      assign(paste0(s,
                    "End"),
             eval(parse(text=paste(paste0(stanza[[s]]$Component,"End"),
                                   sep = "+"))))
      generateDerivedSymbolicObjects(s,
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
      generateDerivedSymbolicObjects(paste0(s, "End"),
                                     environment(),
                                     before = TRUE,
                                     after = TRUE,
                                     ratio = TRUE,
                                     beforeratio = TRUE,
                                     delta = TRUE,
                                     beforedelta = TRUE)
    }
    
    
    
    
    if (!is.null(aliases)){
      for (i in seq_len(nrow(aliases))){
        assign(aliases[i, 1],
               eval(parse(text = aliases[i, 2])))
        generateDerivedSymbolicObjects(aliases[i, 1],
                                       environment(),
                                       before = TRUE,
                                       after = TRUE,
                                       ratio = TRUE,
                                       beforeratio = TRUE,
                                       delta = TRUE,
                                       beforedelta = TRUE)
        
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
      "sp",
      "inflow",
      "outflow",
      "intemp",
      "outtemp",
      "years",
      "ser",
      "i",
      "N",
      "nbgroups",
      "groups",
      "previousgroups"
      
    ))
    return(environment())
  }
