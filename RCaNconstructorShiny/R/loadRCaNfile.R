#' loadRCaNfile
#'
#' @param datapath the path to the model
#' @param modelname the name of the model
#'
#' @return a list with the slots of the network
#' @export
#'
loadRCaNfile <- function(datapath, modelname){
  filenewnetwork <- list()
  shinyCatch({
    orig <- datapath
    newname <- paste(modelname[seq_len(length(modelname) - 1)],
                     collapse = '.')
    
    
    load_comp <- readxl::read_excel(orig,
                                    sheet = "Components & input parameter") %>%
      select(any_of(c("Component",
                      "Inside",
                      "AssimilationE",
                      "Digestibility",
                      "OtherLosses",
                      "Inertia",
                      "Satiation",
                      "RefugeBiomass",
                      "X",
                      "Y",
                      "x",
                      "y"))) %>%
      mutate("Inside" = as.integer(.data[["Inside"]]),
             "id" = .data[["Component"]])
    
    
    if (!"x" %in% names(load_comp)){
      if ("X" %in% names(load_comp)){
        load_comp$x <- round(-400 + 800 * (load_comp$X- min(load_comp$X)) /
                               (max(load_comp$X) - min(load_comp$Y)))
      } else {
        load_comp$x <- as.numeric(NA)
      }
    }
    if (!"y" %in% names(load_comp)){
      if ("Y" %in% names(load_comp)){
        load_comp$y <- round(- 400 + 800 * (load_comp$Y- min(load_comp$Y)) /
                               (max(load_comp$Y) - min(load_comp$Y)))
      } else {
        load_comp$y <- as.numeric(NA)
      }
    }
    load_comp <- load_comp %>%
      dplyr::select(!dplyr::any_of(c("X", "Y")))
    
    load_flux <- readxl::read_excel(orig,
                                    sheet = "Fluxes") %>%
      select(any_of(c("Flux",
                      "From",
                      "To",
                      "Trophic"))) %>%
      mutate("Trophic" = as.integer(.data[["Trophic"]]),
             "id" = .data[["Flux"]],
             "from" = load_comp$Component[match(.data[["From"]],
                                                load_comp$Component)],
             "to" = load_comp$Component[match(.data[["To"]],
                                              load_comp$Component)])
    
    
    load_obs <- readxl::read_excel(orig,
                                   sheet = "Input time-series")
    
    series <- character()
    if (nrow(load_obs) > 0){
      series <- setdiff(names(load_obs), "Year")
    }
    filenewnetwork$components <- load_comp
    filenewnetwork$observations <- load_obs
    filenewnetwork$fluxes <- load_flux
    
    load_aliases <- createEmptyAliases()
    load_metaobs <- createEmptyMetaObs()
    
    
    filenewnetwork$dictionary <-
      generateDictionary(load_comp,
                         load_flux,
                         load_obs,
                         load_metaobs,
                         load_aliases)
    
    if ("Aliases" %in% readxl::excel_sheets(orig)){
      load_aliases <- readxl::read_excel(orig,
                                         sheet = "Aliases") %>%
        mutate(id = .data[["Alias"]],
               idconstraint = sapply(.data[["Formula"]],
                                     convertConstr2idConstr,
                                     filenewnetwork$dictionary))
      if (!"Comment" %in% names(load_aliases)){
        load_aliases$Comment <- character(nrow(load_aliases))
      }
    }
    
    
    if ("MetaObs" %in% readxl::excel_sheets(orig)){
      load_metaobs <- readxl::read_excel(orig,
                                         sheet = "Observation MetaInfo") %>%
        mutate(id = .data[["Observation"]])
      if (!"Comment" %in% names(load_metaobs)){
        load_metaobs$Comment <- character(nrow(load_metaobs))
      }
    } else {
      load_metaobs <- tibble(
        id = setdiff(names(load_obs), "Year"),
        Observation = setdiff(names(load_obs), "Year"),
        Comment = character(ncol(load_obs) - 1))
    }
    
    
    filenewnetwork$dictionary <-
      generateDictionary(load_comp,
                         load_flux,
                         load_obs,
                         load_metaobs,
                         load_aliases)
    filenewnetwork$aliases <- load_aliases
    filenewnetwork$metaobs <- load_metaobs
    
    
    load_constr <- readxl::read_excel(orig,
                                      sheet = "Constraints")
    load_constr$idconstraint = sapply(load_constr$Constraint,
                                      convertConstr2idConstr,
                                      filenewnetwork$dictionary)
    
    filenewnetwork$constraints <- load_constr
    
    
    filenewnetwork$model <- newname
    filenewnetwork$envir <- generateSymbolicEnvir(filenewnetwork)
    
    if (nrow(filenewnetwork$constraints) > 0){
      filenewnetwork$constraints$valid <- TRUE
      filenewnetwork$constraints$validity_comments <- character(nrow(filenewnetwork$constraints))
    }
    
    if (nrow(filenewnetwork$aliases) > 0){
      filenewnetwork$aliases$valid <- TRUE
      filenewnetwork$aliases$validity_comments <- character(nrow(filenewnetwork$aliases))
    }
    
    
    
    
    if ("TimeLines" %in% readxl::excel_sheets(orig)){
      tryCatch({
        filenewnetwork$timeline <<- readxl::read_excel(orig,
                                                       "TimeLines") %>%
          dplyr::select(all_of(c("Date", "Task", "Annotation")))
      },
      error = function(e){
        filenetwork$timeline <<- createEmptyTimeLine()
      }
      )
    } else {
      filenetwork$timeline <<- createEmptyTimeLine()
    }
    
  })
  return(filenetwork)
}