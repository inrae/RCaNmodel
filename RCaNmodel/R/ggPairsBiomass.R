#' ggPairsBiomass
#' plots pairs plot of biomass among species and provided Kendall correlation
#' tau
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the name (or a vector of name) of the species of interest
#' by default, all species
#' @param years years to be plotted (default all)
#' @param frac fraction of points to be plot (default all)
#' @param logscale should biomass be log10 transformed (default yes)
#' @param ... other arguments sent to
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggPairsBiomass(res)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom GGally ggpairs
#' @importFrom GGally wrap
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 geom_hline
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr slice
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang !! sym .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme
#' @export
#'
ggPairsBiomass <- function(mysampleCaNmod,
                     species = NULL,
                     years = NULL,
                     frac = 1,
                     logscale = TRUE,
                     ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package GGally needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.null(years))
    years <- mysampleCaNmod$CaNmod$series$Year
  if (!is.logical(logscale))
    stop("logscale should be a logical")
  if (is.null(species))
    species <- mysampleCaNmod$CaNmod$species
  if (!all(species %in% mysampleCaNmod$CaNmod$species))
    stop("some species are not recognized")

  myCaNmodFit_long <- as.data.frame(as.matrix(mysampleCaNmod$mcmc)) %>%
    mutate("Sample_id" = 1:nrow(as.matrix(mysampleCaNmod$mcmc))) %>%
    slice(round(seq(1, length(.data[["Sample_id"]]), length.out = round(frac * length(.data[["Sample_id"]]))))) %>%
    pivot_longer(cols = -!!sym("Sample_id"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = 'value')
  biomass <- myCaNmodFit_long %>%
    filter(!!sym("Var") %in% species) %>%
    rename("b" = "value") %>%
    mutate("Year" = as.numeric(!!sym("Year"))) %>%
    filter(!!sym("Year") %in% years) %>%
    rename("species" = "Var")
  if (logscale)
    biomass$b <- log10(biomass$b)

  biomass$species <- factor(biomass$species,
                            levels = species)
  biomass <- pivot_wider(biomass,
                         names_from = !!sym("species"),
                         values_from = !!sym("b")) 
  nbiom <- nrow(biomass)
  biomass <- biomass %>%
    slice(round(seq(1, nbiom, length.out = round(frac * nbiom))))
  g <- ggpairs(biomass,
               columns = 3:ncol(biomass),
               lower = list(continuous = wrap("smooth",
                                              method = "loess",
                                              size = 0.1,
                                              alpha = 0.5)),
               upper = list(continuous = wrap("cor",
                                              method = "kendall")),
               ...) +
    ggtitle(ifelse(logscale,
                   'Species pair-plot (log10 scale)',
                   'Species pair-plot')) +
    theme_bw()
  return(g)
}


