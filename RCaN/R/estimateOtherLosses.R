#' estimateOtherLosses
#' Estimate other losses based on various allometric relationships
#' @param weight wet weight in gram
#' @param method one of "gillooly", "yodzis", "makarieva"
#' @param taxon one of ("unicell", "plant", "multicellular invertebrate", "fish",
#' "amphibian", "reptile", "bird or mammal") for gillooly, one of ("endotherm",
#' "vertebrate ecotherm", "invertebrate") for yodzis, one of ("prokaryote",
#' "protozoa", "insect", "aquatic invertebrate", "copepod or krill",
#' "peracarid", "decapod", "cephalopod", "gelatinous invertebrate", "ectothermic
#'  vertebrate", "amphibian", "fish", "reptile", "endothermic vertebrate",
#'  "bird", "mammal", "cyanobacteria", "eukaryotic microalgae", "eukaryotic
#'  macroalgae", "green leaves", "tree saplings", "seedlings")
#' @param temperature temperature in °C (required for Gillooly and Makarieva)
#' @param d fraction of days in which a species grow (1 for 365 days / year).
#' used in gillooly and makarieva
#'
#' @return other losses as explained in supplementary material in
#' \insertCite{planque2022;textual}{RCaN}
#'
#'
#' @details \insertCite{planque2022;textual}{RCaN} proposed several
#' methods based on allometric relationships to estimate otherlosses. Method
#' "gillooly" is based on \insertCite{gillooly2001;textual}{RCaN}, "yodzis"
#' is based on \insertCite{yodzis1992;textual}{RCaN} and "makarieva" on
#' \insertCite{makarieva2008;textual}{RCaN}
#'
#' @importFrom Rdpack reprompt
#' @references
#'     \insertAllCited{}
#' @export
#'
#' @examples
#' estimateOtherLosses(240, "yodzis", taxon = "vertebrate ecotherm")
#' estimateOtherLosses(240, "gillooly", "fish", 13, 1)
#' estimateOtherLosses(240, "makarieva", "fish", 13, 1)
estimateOtherLosses <- function(weight,
                                method,
                                taxon,
                                temperature = NULL,
                                d = 1){
  if (!method %in% c("gillooly", "yodzis", "makarieva") | length(method) > 1)
    stop("method should be unique and one of 'gillooly', 'yodzis', 'makarieva'")
  if (method %in% c("gillooly", "makarieva") & is.null(temperature))
    stop("temperature should be provided for this method")


  ######Gillooly
  if (method == "gillooly"){
    if (!taxon %in% c("unicell", "plant", "multicellular invertebrate", "fish",
                      "amphibian", "reptile", "bird or mammal"))
      stop('taxon should be one of "unicell", "plant",
      "multicellular invertebrate", "fish", "amphibian", "reptile",
           "bird or mammal" with this method')
    reg <- switch(taxon,
                  "unicell" = c(8.79, 25.8),
                  "plant" = c(7.61, 21.37),
                  "multicellular invertebrate" = c(9.15, 27.62),
                  "fish" = c(5.02, 14.47),
                  "amphibian" = c(5.76, 16.68),
                  "reptile" = c(8.78, 26.85),
                  "bird or mammal" = c(9.10, 29.49) )
    TK <- temperature + 273.15
    losses_J_min <- (exp(reg[2])*exp((-reg[1]/TK) * 1000) *
                       weight ^ 0.75)
    losses_J_year <- losses_J_min * 24 * 60 * 365 * d
    losses_kg_year <- losses_J_year / 7000000
    bmr <- losses_kg_year / (weight * 1e-3) # conversion from g to kg
    fmr <- 3 * bmr
  }

  #yodsis
  if (method == "yodzis"){
    if (!taxon %in% c("endotherm", "vertebrate ecotherm", "invertebrate"))
      stop('taxon should be one of "endotherm", "vertebrate ecotherm",
      "inverebrate" with this method')
    a <- switch(taxon,
                "endotherm" = 54.9,
                "vertebrate ecotherm" = 2.3,
                "invertebrate" = .5)
    fmr <- a*(weight * 1e-3) ^ -0.25

  }

  # makarieva
  if (method == "makarieva"){
    if (! taxon %in% c("prokaryote",  "protozoa", "insect",
                       "aquatic invertebrate", "copepod or krill", "peracarid",
                       "decapod", "cephalopod", "gelatinous invertebrate",
                       "ectothermic vertebrate", "amphibian", "fish",
                       "reptile", "endothermic vertebrate", "bird", "mammal",
                       "cyanobacteria", "eukaryotic microalgae",
                       "eukaryotic macroalgae", "green leaves", "tree saplings",
                       "seedlings"))
      stop('taxon should be one of "prokaryote",  "protozoa", "insect",
                       "aquatic invertebrate", "copepod or krill", "peracarid",
                       "decapod", "cephalopod", "gelatinous invertebrate",
                       "ectothermic vertebrate", "amphibian", "fish",
                       "reptile", "endothermic vertebrate", "bird", "mammal",
                       "cyanobacteria", "eukaryotic microalgae",
                       "eukaryotic macroalgae", "green leaves", "tree saplings",
                       "seedlings" with this method')
    Tref <- switch(taxon,
                   "prokaryote" = 31,
                   "protozoa" = 20,
                   "insect" = 25,
                   "aquatic invertebrate" = 11,
                   "copepod or krill" = 13,
                   "peracarid" = 12,
                   "decapod" = 13,
                   "cephalopod" = 5,
                   "gelatinous invertebrate" = 5,
                   "ectothermic vertebrate" = 19,
                   "amphibian" = 19,
                   "fish" = 14,
                   "reptile" = 26,
                   "endothermic vertebrate" = 38,
                   "bird" = 39,
                   "mammal" = 37,
                   "cyanobacteria" = 26,
                   "eukaryotic microalgae" = 15,
                   "eukaryotic macroalgae" = 8,
                   "green leaves" = 25,
                   "tree saplings" = 24,
                   "seedlings" = 24)
    q <- switch(taxon,
                "prokaryote" = 4.6,
                "protozoa" = 7.5,
                "insect" = 2.9,
                "aquatic invertebrate" = 1.3,
                "copepod or krill" = 3.0,
                "peracarid" = 1.4,
                "decapod" = 0.56,
                "cephalopod" = 0.78,
                "gelatinous invertebrate" = 0.78,
                "ectothermic vertebrate" = 0.36,
                "amphibian" = 0.39,
                "fish" = 0.38,
                "reptile" = 0.3,
                "endothermic vertebrate" = 5.5,
                "bird" = 8.7,
                "mammal" = 4.4,
                "cyanobacteria" = 3.7,
                "eukaryotic microalgae" = 8.8,
                "eukaryotic macroalgae" = 2.1,
                "green leaves" = 1.2,
                "tree saplings" = 0.51,
                "seedlings" = 3.6)
    adjusted_q_w_kg <- q * 2^((temperature - Tref) /10)
    adjusted_J_kg_an <- adjusted_q_w_kg * 3600 * 24 * 365 * d
    bmr <- adjusted_J_kg_an/(7000*1000)
    fmr <- 3 * bmr
  }

  return (fmr)
}
