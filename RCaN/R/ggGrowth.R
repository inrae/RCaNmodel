#' ggGrowth
#'
#' plots biomass growth as a function of biomass
#' provides a distribution over iterations and years
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
#' ggGrowth(res)
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
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme
#' @export
#'
ggGrowth <- function(myFitCaNmod,
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
    mutate("next_year" = !!sym("Year") + 1)
  biomass <- biomass %>%
    left_join(select(biomass, -!!sym("next_year")),
              by = c("next_year" = "Year",
                     "Sample_id" = "Sample_id",
                     "Var" = "Var"),
              suffix = c("_curr", "_next")) %>%
    mutate("growth" = b_next/b_curr) %>%
    rename("species" = !!sym("Var"))
  Inertia <- myFitCaNmod$CaNmod$components_param %>%
    filter(!!sym("Component") %in% species) %>%
    mutate(inertia_low = exp(-!!sym("Inertia")),
           inertia_high = exp(!!sym("Inertia"))) %>%
    mutate("species" = factor(!!sym("Component"),
                            levels = species))

  g <- ggplot(na.omit(biomass),
              aes_string(x = "b_curr", y = "growth", col = "species")) +
    geom_point(size=.1, alpha = 0.5) +
    stat_smooth(method = "gam", colour = "chocolate4") +
    geom_hline(yintercept = 1, colour = 'black', linetype = "dashed") +
    geom_hline(data = Inertia, aes_string(yintercept = "inertia_low"),
               colour = "firebrick3", linetype = "dashed") +
    geom_hline(data = Inertia, aes_string(yintercept = "inertia_high"),
               colour = "firebrick3", linetype = "dashed") +
    facet_wrap(~ species, ncol = ceiling(length(species)^0.5),
               scales = "free") +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10')  +
    ggtitle('Growth (B(t+1)/B(t)) vs. Biomass')+
    xlab("biomass") +
    ylab("growth") +
    theme_bw() +
    theme(legend.position = "none")
  return(g)
}


