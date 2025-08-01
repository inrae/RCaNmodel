#' ggTrophicRelation
#' plots fluxes from prey to predators
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
#' ggTrophicRelation(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 stat_density_2d
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang !! sym .data
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 theme
#' @export
#'
ggTrophicRelation <- function(mysampleCaNmod,
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
    mutate("Sample_id" = seq_len(nrow(as.matrix(mysampleCaNmod$mcmc)))) %>%
    slice(round(seq(1, length(.data[["Sample_id"]]), length.out = round(frac * length(.data[["Sample_id"]]))))) 
  
  if (nrow(myCaNmodFit_long) > 1000)
    myCaNmodFit_long <- myCaNmodFit_long %>%
    sample_n(1000, replace = FALSE)
  
  myCaNmodFit_long <- myCaNmodFit_long %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    rename("prey" = "Var") %>%
    filter(!!sym("Year") %in% years)
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
  biomass <- biomass %>%
    na.omit()
  
  
  
  g <- ggplot(na.omit(biomass),
              aes(x = !!sym("b"),
                  y = !!sym("consumption"))) +
    geom_point(size=.1, alpha = 0.5) +
    stat_smooth(method = "gam", colour = "chocolate4") +
    geom_density_2d_filled(contour_var = "ndensity",
                           alpha = .5,
                           colour = NA,
                           breaks = seq(0.1, 1.0, length.out = 10)) +
    scale_fill_viridis_d() +
    facet_grid(predator ~  prey,
               scales = "free") +
    guides(colour = "none", alpha = "none", fill = "none") +
    xlab('Biomass prey') +
    ylab('Flux to predator') +
    theme(legend.position = "none") +
    theme_bw()
  return(g)
}


