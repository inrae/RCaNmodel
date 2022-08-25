#' ggSatiation
#' plots incoming trophic fluxes (consumption) versus biomass, alongside the
#' satiation constraints (red dashed line)
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the name (or a vector of name) of the species of interest
#' by default, all species
#' @param years years to be plotted (default all)
#' @param frac fraction of points to be plot (default all)
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggSatiation(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_density_2d_filled
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_abline
#' @importFrom dplyr left_join
#' @importFrom dplyr slice
#' @importFrom dplyr n
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
ggSatiation <- function(mysampleCaNmod,
                     species = NULL,
                     years = NULL,
                     frac = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
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
    slice(seq(1, n(),by = round(n() / (frac * n())))) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    rename("predator" = "Var")
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

  g <- ggplot(fluxes,
              aes_string(x = "b", y = "consumption")) +
    geom_point(size=.1, alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "firebrick3", linetype = "dashed") +
    geom_abline(data = Satiation,
                aes_string(slope = "Satiation",
                           intercept = "intercept"),
                colour = "firebrick3",
                linetype = "dashed") +
    geom_density_2d_filled(contour_var = "ndensity",
                           alpha = .5,
                           colour = NA,
                           breaks = seq(0.1, 1.0, length.out = 10)) +
    scale_fill_viridis_d() +
    guides(colour = FALSE, alpha = FALSE, fill = FALSE) +
    facet_wrap(~predator,
               ncol = ceiling(length(species)^0.5),
               scales = "free") +
    xlab('Biomass') +
    ylab('Consumption') +
    theme(legend.position = "none") +
    theme_bw()+
    ggtitle('Satiation')
  return(g)
}


