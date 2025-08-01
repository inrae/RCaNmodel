#' generateDictionary
#'
#' @param components the components
#' @param fluxes the fluxes
#' @param observations the observations
#' @param metaobs meta information of observation
#' @param constraints constaints
#' @param aliases facultative aliases
#'
#' @return a dictionary
#' @export
#'
generateDictionary <- function(components,
                               fluxes,
                               observations,
                               metaobs,
                               constraints,
                               aliases = NULL){
  if (!all(sort(setdiff(names(observations), "Year")) ==
          sort(metaobs$Observation)))
    showNotification("metaobs and observations are not consistent",
                     type = "error")
  dictionary <- c(components$Component,
                  fluxes$Flux,
                  metaobs$Observation,
                  constraints$Id,
                  aliases$Alias,
                  "AllFlows",
                  paste0("Outflows",
                         components$Component[which(components$Inside == 1)]),
                  paste0("Inflows",
                         components$Component[which(components$Inside == 1)]))



  names(dictionary) <- c(components$id,
                         fluxes$id,
                         metaobs$id,
                         constraints$id,
                         aliases$id,
                         "AllFlows",
                         paste0("Outflows",
                                components$id[which(components$Inside == 1)]),
                         paste0("Inflows",
                                components$id[which(components$Inside == 1)]))
  f <- fluxes %>%
    dplyr::select(!dplyr::any_of(c("id",
                                  "Flux",
                                  "From",
                                  "To",
                                  "from",
                                  "to")))
  if (ncol(f) > 0){

    newnames <-
      sapply(names(f),
             function(p)
               c(paste0(p,
                        "Outflows",
                        components$Component[which(components$Inside == 1)]),
                 paste0(p,
                        "Inflows",
                        components$Component[which(components$Inside == 1)])))
    names(newnames) <-
      sapply(names(f),
             function(p)
               c(paste0(p,
                        "Outflows",
                        components$id[which(components$Inside == 1)]),
                 paste0(p,
                        "Inflows",
                        components$id[which(components$Inside == 1)])))
    dictionary <- c(dictionary, newnames)

  }
  dictionary

}
