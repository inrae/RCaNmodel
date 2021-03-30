#' ggBottleneck
#' display whether species were closed to satiation or if their predators
#' were closed to inertia
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the species to plot (if null, default, all species)
#' @return plots in a grid.arrange
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom rlang !! sym
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr sample_n
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 stat_density_2d
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom gridExtra grid.arrange
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
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggBottleneck(res)
#' @export


ggBottleneck <- function(mysampleCaNmod, species = NULL){
  if (is.null(species))
    species <- mysampleCaNmod$CaNmod$species
  if (!all(species %in% mysampleCaNmod$CaNmod$species))
    stop("some species are not recognized")
  species <- factor(species, levels = species)


  myCaNmodFit_long <- as.data.frame(as.matrix(mysampleCaNmod$mcmc)) %>%
    mutate("Sample_id" = 1:nrow(as.matrix(mysampleCaNmod$mcmc))) %>%
    sample_n(min(1000,
                 nrow(as.matrix(mysampleCaNmod$mcmc))), replace = FALSE) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                   values_to = 'value')

  #table with biomass at time t and t+1
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
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
    left_join(mysampleCaNmod$CaNmod$fluxes_def) %>%
    rename("predator" = !!sym("To"),
           "prey" = !!sym("From")) %>%
    group_by(!!sym("predator"),
             !!sym("Year"),
             !!sym("Sample_id")) %>%
    summarize(sum_in_flux_from = sum(!!sym("value"))) %>%
    left_join(biomass) %>%
    left_join(Satiation %>%
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

  list_plot <- lapply(species,function(s){
    sub_data <- satiation_tab %>%
      filter(species == s)
    if (nrow(na.omit(sub_data)) > 0){
      ggplot(sub_data,
             aes_string(x = "satiation_std", y = "satiation_std_pred")) +
        geom_point(shape=".",col="grey") +
        #geom_density_2d_filled(alpha = .5) +
        stat_density_2d(geom = "polygon", contour = TRUE,
                        aes(fill = after_stat(!!sym("level"))), colour = NA,
                        bins = 10, alpha = .5)+
        scale_fill_viridis_c()+
        guides(colour = FALSE, alpha = FALSE, fill = FALSE) +
        ggtitle(s) + xlim(0,1)+ylim(0,1) + theme_bw()+
        xlab("") + ylab("")
    } else{
      return(ggplot()+ggtitle(s))
    }
  })


  grid.arrange(grobs = list_plot,
               nrow=ceiling(sqrt(length(list_plot))),
               bottom = "standardised satiation",
               left = "standardised satiation of predators")
}
