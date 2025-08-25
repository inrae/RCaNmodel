#' writeTimeLine
#'
#' @param type the slot that have beenn modified 
#' @param new a tibble of new value
#' @param old a tibble of old value
#'
#' @return a string describing the change
#' @export
#' @importFrom tibble tibble
#' @examples
#' writeTimeLine("aliases", data.frame(Alias = "toto", Formula = 33), NULL)
#' 

writeTimeLine <- function(type, new, old){
  timeline <- ""
  if (!is.null(new))
    new <- tibble::tibble(new)
  if (!is.null(old))
    old <- tibble::tibble(old)
  if (type == "components"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added component", new$Component)
    } else if ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted component", old$Component)
    } else {
      
      if (new$Component != old$Component)
        timeline <- paste("component",
                          old$Component,
                          "renamed to",
                          new$Component)
      for (var in c("Inside", "AssimilationE", "Digestibility", "OtherLosses",
                    "Inertia", "Satiation", "RefugeBiomass")){
        if (!ident_tol(new[, var], old[, var]))
          timeline <- paste0(
            timeline,
            paste("component",
                  new$Component,
                  var,
                  "changed",
                  old[, var],
                  "->",
                  new[, var]),
            collapse = ", ")
        
      }
    }
    
  }
  
  
  
  
  
  if (type == "fluxes"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added flux", new$Flux)
    } else if ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted flux", old$Flux)
    } else {
      
      if (new$Flux != old$Flux)
        timeline <- paste("flux",
                          old$Flux,
                          "renamed to",
                          new$Flux)
      for (var in c("From", "To", "Trophic")){
        if (!ident_tol(new[, var], old[, var]))
          timeline <- paste0(
            timeline,
            paste("flux",
                  new$Flux,
                  var,
                  "changed",
                  old[, var],
                  "->",
                  new[, var]),
            collapse = ", ")
        
      }
    }
  }
  
  
  
  
  
  
  if (type == "aliases"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added alias", new$Alias)
    } else if ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted alias", old$Alias)
    } else {
      
      if (new$Alias != old$Alias)
        timeline <- paste("alias",
                          old$Alias,
                          "renamed to",
                          new$Alias)
      
      for (var in c("Formula")){
        if (!ident_tol(new[, var], old[, var]))
          timeline <- paste0(
            timeline,
            paste("alias",
                  new$Alias,
                  var,
                  "changed",
                  old[, var],
                  "->",
                  new[, var]),
            collapse = ", ")
        
      }
    }
  }
  
  if (type == "constraints"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added constraints", new$Id)
    } else if ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted constraints", old$Id)
    } else {
      if (new$Id != old$Id)
        timeline <- paste("constraint",
                          old$Id,
                          "renamed to",
                          new$Id)
      
      for (var in c("Constraint", "Active", "Time-range")){
        if (!ident_tol(new[, var], old[, var]))
          timeline <- paste0(
            timeline,
            paste("constraint",
                  new$Id,
                  var,
                  "changed",
                  old[, var],
                  "->",
                  new[, var]),
            collapse = ", ")
        
      }
    }
  }
  
  if (type == "observations"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added Year", new$Year)
    } else if  ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted Year", old$Year)
    } else {
      for (col in setdiff(intersect(names(new),
                                    names(old)),
                          "Year")){
        if (!ident_tol(new[, col], old[,col]))
          {
          timeline <- paste0(
            timeline,
            paste("val of series",
                  col,
                  "changed",
                  old[, col],
                  "->",
                  new[, col]),
            collapse = ", ")}
      }
    }
  }
  
  if (type == "metaobs"){
    if ((!is.null(new)) & is.null(old)){
      timeline <- paste("added series", new$Observation)
    } else if  ((!is.null(old)) & is.null(new)) {
      timeline <- paste("deleted series", old$Observation)
    } else {
      if (new$Observation != old$Observation)
        timeline <- paste("observation",
                          old$Observation,
                          "renamed to",
                          new$Observation)
      for (var in c("Comment")){
        if (!ident_tol(new[, var], old[, var]))
          timeline <- paste0(
            timeline,
            paste("series",
                  new$Observation,
                  var,
                  "changed",
                  old[, var],
                  "->",
                  new[, var]),
            collapse = ", ")
        
      }
    }
    
  }
  
  if (timeline != ""){
    return (tibble::tibble(Task = timeline))
  } else {
    return (NULL)
  }
}