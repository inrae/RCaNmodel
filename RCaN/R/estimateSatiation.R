#' estimateSatiation
#' Estimate satiation based on allometric relationship
#' @param weight wet weight in gram
#' @param taxon one of ("endotherm", #' "vertebrate ecotherm", "invertebrate")
#' @param assimilation_efficiency assimilation efficiency of the species
#' (unitless)
#' @param digestibility average digestibility correction factor of prey
#' (unitless)
#'
#' @return satiation as explained in supplementary material in
#' \insertCite{planque2022;textual}{RCaN}
#'
#' @details Following \insertCite{planque2022;textual}{RCaN}, this function
#' estimates satiation based on an allometric relationship formulated by
#' \insertCite{yodzis1992;textual}{RCaN}
#'
#' @importFrom Rdpack reprompt
#' @references
#'     \insertAllCited{}
#' @export
#'
#' @examples
#' estimateSatiation(1e-4, taxon = "invertebrate", 1, 0.9, 0.3)
estimateSatiation <- function(weight,
                              taxon,
                              assimilation_efficiency = 1,
                              digestibility = 1){
  if (!taxon %in% c("endotherm", "vertebrate ecotherm", "invertebrate"))
    stop('taxon should be one of "endotherm", "vertebrate ecotherm",
      "inverebrate" with this method')
  a <- switch(taxon,
              "endotherm" = 89.2,
              "vertebrate ecotherm" = 8.9,
              "invertebrate" = 9.7)
  fractional <- switch(taxon,
                       "endotherm" = 1,
                       "vertebrate ecotherm" = 0.2,
                       "invertebrate" = 0.3)
  sigma <- (fractional * a * (weight * 1e-3) ^ -0.25) /
    (assimilation_efficiency * digestibility)


  return (sigma)
}
