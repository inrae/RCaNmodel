
#' ggViolin
#'
#' provide a violin plot for specific years
#' @param myFitCaNmod result sent by \link{fitmyCaNmod}
#' @param param the name (or a vector of name) of a parameter (either a flow or
#' a biomass)
#' @param year the year
#' @param logscale flag to indicate to use a log scale (default TRUE)
#' @param xlab default Component Flux
#' @param ylab default Biomass
#' @return a ggplot
#' @details distribution of fluxes or biomass for a specific year
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' #with one series#'
#' #with 2 series
#' ggViolin(res,c("F01","HerbZooplankton"), year=1988, TRUE)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom tidyr pivot_longer

#' @export
#'
ggViolin <- function(myFitCaNmod,
                     param,
                     year,
                     logscale=TRUE,
                     xlab = "Component/Flux",
                     ylab = "Biomass") {
  if (length(year) != 1)
    stop("a single year should be provided")
  if (class(myFitCaNmod) != "fitCaNmod")
    stop("you should provide a fitCaNmod object")
  mat_res <- as.matrix(myFitCaNmod$mcmc)
  mat_res <- mat_res[, colnames(mat_res) %in% paste(param,
                                                    "[",
                                                    year,
                                                    "]",
                                                    sep = ""),
                     drop = FALSE]
  mat_res <- as.data.frame(mat_res) %>%
    pivot_longer(cols = everything(),
                 names_to = c("Var","Year"),
                 names_pattern = "(.*)\\[(.*)\\]",
                 values_to = "val")
  if (nrow(mat_res) < 1)
    stop("no data found")
  g <- ggplot(mat_res, aes(x = Var, y = val)) +
    geom_violin(trim=TRUE,scale="width",aes(fill = Var))+
    theme_bw()
  if (logscale)
    g <- g + scale_y_continuous(trans='log10')
  g <- g +  xlab(xlab) +
    ylab(ylab) +
    theme(legend.position = "none")
  return(g)
}
