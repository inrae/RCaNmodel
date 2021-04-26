
#' ggDiet
#' provides a ggplot of diet, barplot provides an average diet while violin plot
#' provides a distribution over iterations and years
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param species the name (or a vector of name) of the species of interest
#' @param years years to be plotted (default all)
#' @param barplot if TRUE a barplot (default), else a violin plot
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#' ggDiet(res,"OmniZooplankton")
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr sample_n
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
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
#' @export
#'
ggDiet <- function(mysampleCaNmod,
                   species,
                   years = NULL,
                   barplot = TRUE) {
  if (is.null(years))
    years <- mysampleCaNmod$CaNmod$series$Year
  if (!all(species %in% mysampleCaNmod$CaNmod$species))
    stop("some species are not recognized")
  if (!is.logical(barplot))
    stop("barplot should be logical")
  fluxes <- mysampleCaNmod$CaNmod$fluxes_def %>%
    filter(!!sym("To") %in% species & !!sym("Trophic"))
  mat_res <- as.matrix(mysampleCaNmod$mcmc)
  mat_res <- mat_res[, grep(paste("^(",
                                  paste(fluxes$Flux, collapse = "|"),
                                  ")\\[",
                                  sep = ""),
                            colnames(mat_res)),
                     drop = FALSE]
  mat_res <- as.data.frame(mat_res) %>%
    mutate(iter =  1:nrow(mat_res)) %>%
    sample_n(min(1000, nrow(mat_res)), replace = FALSE) %>%
    pivot_longer(cols = - !!sym("iter"),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = "val") %>%
    left_join(fluxes, by = c("Var" = "Flux")) %>%
    filter(!!sym("Year") %in% years)
  mat_res <- mat_res %>%
    select(-!!sym("Var"))%>%
    group_by(!!sym("To"), !!sym("iter"), !!sym("Year")) %>%
    mutate("prop_in_diet" = !!sym("val")/sum(!!sym("val"))) %>%
    rename("prey" = !!sym("From"), "predator" = !!sym("To"))
  if (barplot)
    mat_res <- mat_res %>%
    group_by(!!sym("prey"), !!sym("predator")) %>%
    summarize("prop_in_diet" = mean(!!sym("prop_in_diet")))
  mat_res$predator <- factor(as.character(mat_res$predator),
                             levels = species)


  if (barplot){
    g <- ggplot(data = mat_res, aes_string(x = "predator",
                                    y = "prop_in_diet",
                                    fill = "prey")) +
      geom_bar(position="stack", stat="identity",width=.98,color='black') +
      theme(legend.position = "bottom")
    } else{
    g <- ggplot(data = mat_res, aes_string(x = "prey",
                                    y = "prop_in_diet")) +
      geom_violin(trim = TRUE, scale = "width", fill = "darkcyan") +
      theme_bw() +
      facet_wrap(~predator, ncol = ceiling(length(species)^0.5)) +
      xlab('Prey') +
      ylab('% in diet')
  }
  return(g)
}


