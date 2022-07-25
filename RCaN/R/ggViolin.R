#' ggViolin
#' provides a violin plot of the distribution of flux or biomass
#' for specific years
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param param the name (or a vector of name) of a parameter (either a flux or
#' a biomass)
#' @param years years to be plotted (default all)
#' @param logscale flag to indicate to use a log scale (default TRUE)
#' @param xlab default Component Flux
#' @param ylab default Distribution
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#' #with 2 series
#' ggViolin(res,c("F01","HerbZooplankton"), year=1988, TRUE)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_bw
#' @importFrom magrittr %>%
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @export
#'
ggViolin <- function(mysampleCaNmod,
                     param,
                     years = NULL,
                     logscale=TRUE,
                     xlab = "Component/Flux",
                     ylab = "Distribution") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.null(years))
    years <- mysampleCaNmod$CaNmod$series$Year
  if (! inherits(mysampleCaNmod, "sampleCaNmod"))
    stop("you should provide a sampleCaNmod object")
  mat_res <- as.matrix(mysampleCaNmod$mcmc)
  mat_res <- mat_res[, colnames(mat_res) %in% paste0(rep(param,
                                                         each = length(years)),
                                                    "[",
                                                    years,
                                                    "]"),
                     drop = FALSE]
  mat_res <- as.data.frame(mat_res) %>%
    pivot_longer(cols = everything(),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = "val")
  mat_res$Var <- factor(mat_res$Var,
                        levels = param)
  if (nrow(mat_res) < 1)
    stop("no data found")
  g <- ggplot(mat_res, aes_string(x = "Var", y = "val")) +
    geom_violin(trim=TRUE,scale="width",aes_string(fill = "Var"))+
    theme_bw()
  if (logscale)
    g <- g + scale_y_continuous(trans='log10')
  g <- g +  xlab(xlab) +
    ylab(ylab) +
    theme(legend.position = "none")
  return(g)
}
