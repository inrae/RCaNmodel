#' ggPairsBiomass
#' plots pairs plot of biomass among species and provided Kendall correlation
#' tau
#' @param myFitCaNmod result sent by \link{fitmyCaNmod}
#' @param species the name (or a vector of name) of the species of interest
#' by default, all species
#' @param logscale should biomass be log10 transformed (default yes)
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' ggPairsBiomass(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr sample_n
#' @importFrom GGally ggpairs
#' @importFrom GGally wrap
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
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme
#' @export
#'
ggPairsBiomass <- function(myFitCaNmod,
                     species = NULL,
                     logscale = TRUE) {
  if (!is.logical(logscale))
    stop("logscale should be a logical")
  if (is.null(species))
    species <- myFitCaNmod$CaNmod$species
  if (!all(species %in% myFitCaNmod$CaNmod$species))
    stop("some species are not recognized")

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
    rename("species" = "Var")
  if (logscale)
    biomass$b <- log10(biomass$b)

  biomass$species <- factor(biomass$species,
                            levels = species)
  biomass <- pivot_wider(biomass,
                         names_from = !!sym("species"),
                         values_from = !!sym("b"))
  g <- ggpairs(biomass,
               columns = 3:ncol(biomass),
               lower = list(continuous = wrap("smooth",
                                              method = "loess",
                                              size = 0.1,
                                              alpha = 0.5)),
               upper = list(continuous = wrap("cor",
                                              method = "kendall"))) +
    ggtitle(ifelse(logscale,
                   'Species pair-plot (log10 scale)',
                   'Species pair-plot')) +
    theme_bw()
  return(g)
}


