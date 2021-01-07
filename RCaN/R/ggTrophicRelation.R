#' ggTrophicRelation
#'
#' plots fluxes from prey to predators
#' @param myFitCaNmod result sent by \link{fitmyCaNmod}
#' @param species the name (or a vector of name) of the species of interest
#' by default, all species
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' #with one series#'
#' #with 2 series
#' ggTrophicRelation(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang !! sym
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 theme
#' @export
#'
ggTrophicRelation <- function(myFitCaNmod,
                     species = NULL) {
  if (is.null(species))
    species <- myFitCaNmod$CaNmod$species
  if (!all(species %in% myFitCaNmod$CaNmod$species))
    stop("some species are not recognized")
  species <- factor(species, levels = species)
  myCaNmodFit_long <- as.data.frame(as.matrix(myFitCaNmod$mcmc)) %>%
    mutate("Sample_id" = 1:nrow(as.matrix(myFitCaNmod$mcmc))) %>%
    pivot_longer(col = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    rename("prey" = "Var")
  trophic_flows <- myFitCaNmod$CaNmod$fluxes_def$Flux[
    myFitCaNmod$CaNmod$fluxes_def$Trophic == 1]
  fluxes <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% trophic_flows) %>%
    rename("Flux" = "Var") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    left_join(myFitCaNmod$CaNmod$fluxes_def) %>%
    rename("predator" = !!sym("To"),
           "prey" = !!sym("From")) %>%
    filter(!!sym("prey") %in% species &
             !!sym("predator") %in% species) %>%
    group_by(!!sym("Sample_id"),
             !!sym("Year"),
             !!sym("predator"),
             !!sym("prey")) %>%
    summarize("consumption" = sum(!!sym("value")))
  biomass <- biomass %>%
    left_join(fluxes)

  biomass$predator <- factor(biomass$predator,
                            levels = species)
  biomass$prey <- factor(biomass$prey,
                             levels = species)



  g <- ggplot(na.omit(biomass),
              aes_string(x = "b", y = "consumption")) +
    geom_point(size=.1, alpha = 0.5) +
    stat_smooth(method = "gam", colour = "chocolate4") +
    facet_grid(predator ~  prey,
               scales = "free") +
    xlab('Biomass prey') +
    ylab('Flux to predator') +
    theme(legend.position = "none") +
    theme_bw()
  return(g)
}


