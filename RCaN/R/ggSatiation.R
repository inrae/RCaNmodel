#' ggSatiation
#' plots incoming trophic fluxes versus biomass
#' @param myFitCaNmod result sent by \link{fitmyCaNmod}
#' @param species the name (or a vector of name) of the species of interest
#' by default, all species
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' ggSatiation(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr sample_n
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_abline
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
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme
#' @export
#'
ggSatiation <- function(myFitCaNmod,
                     species = NULL) {
  if (is.null(species))
    species <- myFitCaNmod$CaNmod$species
  if (!all(species %in% myFitCaNmod$CaNmod$species))
    stop("some species are not recognized")
  species <- factor(species, levels = species)
  myCaNmodFit_long <- as.data.frame(as.matrix(myFitCaNmod$mcmc)) %>%
    mutate("Sample_id" = 1:nrow(as.matrix(myFitCaNmod$mcmc))) %>%
    sample_n(min(1000, nrow(as.matrix(myFitCaNmod$mcmc))), replace = FALSE) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    rename("predator" = "Var")
  trophic_flows <- myFitCaNmod$CaNmod$fluxes_def$Flux[
    myFitCaNmod$CaNmod$fluxes_def$Trophic == 1]
  fluxes <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% trophic_flows) %>%
    rename("Flux" = "Var") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    left_join(myFitCaNmod$CaNmod$fluxes_def) %>%
    rename("predator" = !!sym("To"),
           "prey" = !!sym("From")) %>%
    group_by(!!sym("Sample_id"),
             !!sym("Year"),
             !!sym("predator")) %>%
    summarize("consumption" = sum(!!sym("value"))) %>%
    left_join(biomass)

  fluxes$predator <- factor(fluxes$predator,
                            levels = species)


  Satiation <- myFitCaNmod$CaNmod$components_param %>%
    filter(!!sym("Component") %in% species) %>%
    mutate("predator" = factor(!!sym("Component"),
                               levels=species),
           "intercept" = 0 ) %>%
    select(!!sym("predator"),
           !!sym("Satiation"),
           !!sym("intercept"))

  g <- ggplot(fluxes,
              aes_string(x = "b", y = "consumption")) +
    geom_point(size=.1, alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "firebrick3", linetype = "dashed") +
    geom_abline(data = Satiation,
                aes_string(slope = "Satiation",
                           intercept = "intercept"),
                colour = "firebrick3",
                linetype = "dashed") +
    facet_wrap(~predator,
               ncol = ceiling(length(species)^0.5),
               scales = "free") +
    xlab('Predator biomass') +
    ylab('Total flux to predator') +
    theme(legend.position = "none") +
    theme_bw()+
    ggtitle('Satiation')
  return(g)
}


