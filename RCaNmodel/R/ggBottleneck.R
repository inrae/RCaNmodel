#' ggBottleneck
#' display whether species were closed to satiation or if their predators
#' were closed to inertia
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the species to plot (if null, default, all species)
#' @param years years to be plotted (default all)
#' @param frac fraction of points to be plot (default all)
#' @return a ggplot
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom rlang !! sym
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @importFrom dplyr n
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 stat_density_2d
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 scale_fill_viridis_d
#'
#' @details
#' the idea of this plot is to show whether a species has enough food and is or
#' limiting in its predator diet. As such, the x-axis displays how far the
#' species is from satiation (1 satiation, 0 no food) and the y-axis displays
#' how far the sum of trophics flow towards its predator is from the flows that
#' would have occured if all predators had achieved satiation (1 = all predators
#' are at satiation). Each point stands for a time step in one iteration of
#' MCMC samples.
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggBottleneck(res)
#' @export


ggBottleneck <- function(mysampleCaNmod,
                         species = NULL,
                         years = NULL,
                         frac = 1){
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work.
         Please install it",
         call. = FALSE)
  }
  if (is.null(years))
    years <- mysampleCaNmod$CaNmod$series$Year
  if (is.null(species))
    species <- mysampleCaNmod$CaNmod$species
  if (!all(species %in% mysampleCaNmod$CaNmod$species))
    stop("some species are not recognized")
  species <- factor(species, levels = species)
  
  
  myCaNmodFit_long <- as.data.frame(as.matrix(mysampleCaNmod$mcmc)) %>%
    mutate("Sample_id" = 1:nrow(as.matrix(mysampleCaNmod$mcmc))) %>%
    slice(round(seq(1, nrow(.), length.out = round(frac * nrow(.))))) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  
  #table with biomass at time t and t+1
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    rename("predator" = "Var") %>%
    mutate("next_year" = !!sym("Year") + 1)
  
  
  #limit to trophic flows
  trophic_flows <- mysampleCaNmod$CaNmod$fluxes_def$Flux[
    mysampleCaNmod$CaNmod$fluxes_def$Trophic == 1]
  
  
  #table use to see species satiation
  fluxes_to <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% trophic_flows) %>%
    rename("Flux" = "Var") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    left_join(mysampleCaNmod$CaNmod$fluxes_def) %>%
    rename("predator" = !!sym("To"),
           "prey" = !!sym("From")) %>%
    group_by(!!sym("Sample_id"),
             !!sym("Year"),
             !!sym("predator")) %>%
    summarize("consumption" = sum(!!sym("value"))) %>%
    left_join(biomass)
  
  fluxes_to$predator <- factor(fluxes_to$predator,
                               levels = species)
  
  
  
  
  
  
  
  #table with satiation parameters
  Satiation <- mysampleCaNmod$CaNmod$components_param %>%
    filter(!!sym("Component") %in% species) %>%
    filter(!!sym("Inside") == 1) %>%
    mutate("predator" = factor(!!sym("Component"),
                               levels=species),
           "intercept" = 0 ) %>%
    select(!!sym("predator"),
           !!sym("Satiation"),
           !!sym("intercept"))
  
  #table with for each species an indicator of satiation of predators
  fluxes_from <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% trophic_flows) %>%
    rename("Flux" = "Var") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    left_join(mysampleCaNmod$CaNmod$fluxes_def) %>%
    rename("predator" = !!sym("To"),
           "prey" = !!sym("From")) %>%
    group_by(!!sym("predator"),
             !!sym("Year"),
             !!sym("Sample_id")) %>%
    summarize(sum_in_flux_from = sum(!!sym("value"))) %>%
    left_join(biomass) %>%
    inner_join(Satiation %>%
                 mutate("predator" = as.character(!!sym("predator")))) %>%
    mutate(sum_max_in_flux = !!sym("b") * !!sym("Satiation")) %>%
    left_join(mysampleCaNmod$CaNmod$fluxes_def[
      mysampleCaNmod$CaNmod$fluxes_def$Trophic == 1, ],
      by = c("predator" = "To")) %>%
    group_by(!!sym("From"),
             !!sym("Year"),
             !!sym("Sample_id")) %>%
    summarize(satiation_std_pred =
                sum(!!sym("sum_in_flux_from")) /
                sum(!!sym("sum_max_in_flux"))) %>%
    rename("species" = !!sym("From"))
  
  
  
  satiation_tab <- merge(fluxes_to, Satiation) %>%
    mutate(satiation_std = !!sym("consumption") /
             (!!sym("b")*!!sym("Satiation"))) %>%
    rename("species" = !!sym("predator")) %>%
    select(-!!sym("consumption"),
           -!!sym("b"),
           -!!sym("Satiation"),
           -!!sym("intercept")) %>%
    left_join(fluxes_from)
  
  
  ggplot(satiation_tab,
         aes(x = !!sym("satiation_std"),
             y = !!sym("satiation_std_pred"))) +
    geom_point(shape=".",col="grey") +
    #geom_density_2d_filled(alpha = .5) +
    geom_density_2d_filled(contour_var = "ndensity",
                           alpha = .5,
                           colour = NA,
                           breaks = seq(0.1, 1.0, length.out = 10)) +
    scale_fill_viridis_d() +
    facet_wrap(~ species, ncol = ceiling(length(species)^0.5),
               scales = "free") +
    guides(colour = "none", alpha = "none", fill = "none") +
    xlim(0,1)+ylim(0,1) + theme_bw()+
    xlab("") + ylab("")
}
