#' ggTopDownBottomUp
#' display a diagram to detect bottom up control
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species a named list with for each species of interest (names), a
#' vector of predators to consider (if null, default, all species and all
#' predators)
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
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom dplyr any_of
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 scale_linetype_discrete
#' @importFrom ggplot2 facet_wrap
#' @importFrom stats cor
#'
#' @details
#' the idea of this plot is to show whether a species is controlled by its
#' predators
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggTopDownBottomUp(res)
#' @export




ggTopDownBottomUp <- function(mysampleCaNmod,
                              species = NULL,
                              years = NULL,
                              frac = 1){
  CanMod <- mysampleCaNmod$CaNmod
  if (is.null(species)){
      sp <- CanMod$species #all species
      species <- lapply(sp, function(s){
        to <- unique(CanMod$fluxes_def$To[CanMod$fluxes_def$From == s])
        sort(unique(to))
        })
      names(species) <- sp
      species <- species[lengths(species) > 0]
  }
  if (is.null(years))
    years <- mysampleCaNmod$CaNmod$series$Year
  combinations <- do.call('rbind.data.frame',
                          lapply(seq_along(species), function(i){
                            data.frame(From = rep(names(species)[i],
                                                length(species[[i]])),
                                       To = species[[i]])
                            })
  )
  # Data wrangling ----------------------------------------------------------
  # Transform the mcmc output into a 4 col vector with simulation id, year, variable and value
  mcmc <- mysampleCaNmod$mcmc
  myCaNmodFit_long <- as.data.frame(as.matrix(mcmc)) %>%
    mutate(Sample_id = 1:dim(as.matrix(mcmc))[1]) %>%
    slice(seq(1, n(),by = round(n() / (frac * n())))) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  # dataset for the bottom-up correlation (feeding-growth)
  fluxTo <- myCaNmodFit_long %>%
    rename("Flux" = !!sym("Var")) %>%
    inner_join(CanMod$fluxes_def) %>%
    filter(!!sym("To") %in% names(species)) %>%
    group_by(!!sym("To"),
             !!sym("Year"),
             !!sym("Sample_id")) %>%
    summarize(feeding = sum(!!sym("value"))) %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    rename("Species" = !!sym("To"))

  # dataset for the top-down correlation (predation-growth)
  fluxFrom <- myCaNmodFit_long %>%
    rename("Flux" = !!sym("Var")) %>%
    inner_join(CanMod$fluxes_def) %>%
    inner_join(combinations) %>%
    group_by(!!sym("From"),
             !!sym("Year"),
             !!sym("Sample_id")) %>%
    summarize(predation = sum(!!sym("value"))) %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    rename("Species" = !!sym("From"))

  biomass <- myCaNmodFit_long %>%
    rename("Species" = !!sym("Var")) %>%
    dplyr::filter(!!sym("Species") %in% names(species)) %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    mutate("next_year" = !!sym("Year") + 1)


  biomass <- biomass %>%
    inner_join(select(biomass, -!!sym("next_year")),
              by = c("next_year" = "Year",
                     "Sample_id" = "Sample_id",
                     "Species" = "Species"),
              suffix = c("_curr", "_next")) %>%
    mutate("growth" = !!sym("value_next")/!!sym("value_curr"))

  #full_table
  cortable <- biomass %>%
    inner_join(fluxFrom) %>%
    inner_join(fluxTo) %>%
    mutate(feeding = !!sym("feeding") / !!sym("value_curr"),
           predation = !!sym("predation") / !!sym("value_curr")) %>%
    group_by(!!sym("Species"),
             !!sym("Sample_id")) %>%
    filter(!!sym("Year") %in% years) %>%
    summarize(R1 = cor(!!sym("feeding"), !!sym("growth")),
              R2 = cor(!!sym("predation"), !!sym("growth"))) %>%
    pivot_longer(cols = any_of(c("R1","R2")),
                 names_to="type",
                 values_to="correlation")

  ggplot(data = cortable) +
    geom_vline(xintercept = c(-0.5,0.5), size = 1, alpha = 0.25) +
    geom_density(aes_string(x = "correlation",
                            fill = "type",
                            linetype = 'type'),
                 alpha=.5) +
    scale_fill_discrete("", labels = c("Growth-Feeding", "Growth-Predation")) +
    scale_linetype_discrete("",
                            labels = c("Growth-Feeding", "Growth-Predation")) +
    xlim(-1,1) +
    xlab("Correlation") +
    ylab("Density") +
    facet_wrap(~ Species,
               scales = "free") +
    theme_bw()
}
