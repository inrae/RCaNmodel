#' ggSatiatInertia
#' display whether species were closed to satiation or inertia bounds
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the species to plot (if null, default, all species)
#' @param years years to be plotted (default all)
#' @param frac fraction of points to be plot (default all)
#' @return a ggplot
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom rlang !! sym
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr sample_n
#' @importFrom dplyr left_join
#' @importFrom dplyr slice
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_density_2d_filled
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 scale_fill_viridis_d
#'
#' @details
#' for each species, a point stands for a time step and an iteration of the
#' MCMC. The x axis stands for the inertia constraint: a value close to 1
#' indicates that the growth (resp -1) was constrained by the max inertia (resp
#' min) bound. A value of 0 indicates no growth. Y-axis stands for satiation,
#' a value of 0 indicates that the species has not eaten while a value of 1
#' indicates that the species was close to satiation.
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggSatiatInertia(res)
#' @export




ggSatiatInertia <- function(mysampleCaNmod,
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
    sample_n(min(1000, nrow(as.matrix(mysampleCaNmod$mcmc))), replace = FALSE) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    rename("predator" = "Var") %>%
    mutate("next_year" = !!sym("Year") + 1)



  trophic_flows <- mysampleCaNmod$CaNmod$fluxes_def$Flux[
    mysampleCaNmod$CaNmod$fluxes_def$Trophic == 1]
  fluxes <- myCaNmodFit_long %>%
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

  fluxes$predator <- factor(fluxes$predator,
                            levels = species)



  Satiation <- mysampleCaNmod$CaNmod$components_param %>%
    filter(!!sym("Component") %in% species) %>%
    mutate("predator" = factor(!!sym("Component"),
                               levels=species),
           "intercept" = 0 ) %>%
    select(!!sym("predator"),
           !!sym("Satiation"),
           !!sym("intercept"))

  satiation_tab <- merge(fluxes, Satiation) %>%
    mutate(satiation_std = !!sym("consumption") /
             (!!sym("b") * !!sym("Satiation"))) %>%
    rename("species" = !!sym("predator")) %>%
    select(-!!sym("consumption"),
           -!!sym("b"),
           -!!sym("Satiation"),
           -!!sym("intercept"))

  Inertia <- mysampleCaNmod$CaNmod$components_param %>%
    filter(!!sym("Component") %in% species) %>%
    mutate(inertia_low = exp(-!!sym("Inertia")),
           inertia_high = exp(!!sym("Inertia"))) %>%
    mutate("species" = factor(!!sym("Component"),
                              levels = species)) %>%
    select(!!sym("species"),
           !!sym("inertia_low"),
           !!sym("inertia_high"),
           !!sym("Inertia"))



  biomass <- biomass %>%
    left_join(select(biomass, -!!sym("next_year")),
            by = c("next_year" = "Year",
                   "Sample_id" = "Sample_id",
                   "predator" = "predator"),
            suffix = c("_curr", "_next")) %>%
    mutate("growth" = !!sym("b_next")/!!sym("b_curr")) %>%
    rename("species" = !!sym("predator"))



  immigrants <- myCaNmodFit_long %>%
    filter(! (!!sym("Var") %in% trophic_flows)) %>%
    rename("Flux" = "Var") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    left_join(mysampleCaNmod$CaNmod$fluxes_def) %>%
    group_by(!!sym("Year"), !!sym("To")) %>%
    summarize(immig = sum(!!sym("value"))) %>%
    filter(!is.na(!!sym("To"))) %>%
    rename("species" = !!sym("To"))


  inertia_tab <- merge(biomass, Inertia) %>%
    left_join(immigrants) %>%
    mutate(immig = ifelse(is.na(!!sym("immig")),
                          0,
                          !!sym("immig"))) %>%
    mutate(inertia_high = !!sym("inertia_high") +
             (1-exp(-!!sym("Inertia")))/!!sym("Inertia") *
             !!sym("immig") / !!sym("b_curr")) %>%
    select(-!!sym("Inertia"), -!!sym("immig")) %>%
    mutate(growth_std = ifelse(!!sym("growth") > 1,
                               1 / (!!sym("inertia_high") - 1) * !!sym("growth")
                               - 1 / (!!sym("inertia_high") - 1),
                               1 / (1 - !!sym("inertia_low")) * !!sym("growth") -
                                 1 /(1 - !!sym("inertia_low")) )) %>%
    select(!!sym("species"),
           !!sym("Sample_id"),
           !!sym("Year"),
           !!sym("growth_std"))


  full_tab <- merge(inertia_tab, satiation_tab) %>%
    slice(round(seq(1, nrow(.), length.out = round(frac * nrow(.)))))

    ggplot(full_tab, aes(y = !!sym("growth_std"), x = !!sym("satiation_std"))) +
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
      geom_hline(aes(yintercept = 0), lty = 2) +
      ylim(-1,1)+xlim(0,1) + theme_bw()

}
