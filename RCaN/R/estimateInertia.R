#' estimateInertia
#' Estimate inertia based on allometric relationships
#' @param weight wet weight in gram
#' @param method one of "savage" or "yodzis"
#' @param taxon one of ("endotherm", #' "vertebrate ecotherm", "invertebrate")
#' for yodzis and one of ("unicellular eukaryote","multicellular eukaryote",
#' "vertebrate") for savaage
#' @param temperature temperature in °C (if savage)
#' @param d fraction of days in which a species grow (1 for 365 days / year).
#' used in savage
#' @param fractional Fractional properties (unitless), generally in the order
#' of 0.1 (default value)
#'
#' @return satiation as explained in supplementary material in
#' \insertCite{planque2022;textual}{RCaN}
#'
#' @details Following \insertCite{planque2022;textual}{RCaN}, this function
#' estimates satiation based on an allometric relationship formulated by
#' \insertCite{yodzis1992;textual}{RCaN} or
#' \insertCite{savage2004;textual}{RCaN}
#'
#' @importFrom Rdpack reprompt
#' @references
#'     \insertAllCited{}
#' @export
#'
#' @examples
#' estimateInertia(1e-4, "yodzis", taxon = "invertebrate", factional = 0.1)
estimateInertia <- function(weight,
                            method,
                            taxon,
                            temperature = NULL,
                            d = 1,
                            fractional = 0.1){
  if (!method %in% c("savage", "yodzis") | length(method) > 1)
    stop("method should be unique and one of 'savage' or 'yodzis'")
  if (method == "temperature" & is.null(temperature))
    stop("temperature should be provided for this method")


  ######Savage
  if (method == "savage"){
    if (!taxon %in% c("unicellular eukaryote",
                      "multicellular eukaryote",
                      "vertebrate"))
      stop('taxon should be one of "unicellular eukaryote",
           "multicellular eukaryote" or "vertebrate" with this method')
    reg <- switch(taxon,
                  "unicellular eukaryote" = c(20.04, 0.54),
                  "multicellular eukaryote" = c(32.39, 0.84),
                  "vertebrate" = c(12.57, 0.35))
    k <- 8.617E-05
    Tk <- temperature + 273.15
    daily_growth <- exp(reg[1]) * exp(-reg[2] / (k * Tk)) *
      (weight * 1e6) ^ -0.25 #weight in micrograms
    rho <- daily_growth * d * 365
  }

  #yodsis
  if (method == "yodzis"){
    if (!taxon %in% c("endotherm", "vertebrate ecotherm", "invertebrate"))
      stop('taxon should be one of "endotherm", "vertebrate ecotherm",
      "inverebrate" with this method')
    a <- switch(taxon,
                "endotherm" = 34.3,
                "vertebrate ecotherm" = 6.6,
                "invertebrate" = 9.2)
    rho <- a* fractional  * (weight * 1e-3) ^ -0.25

  }

  return (rho)
}
