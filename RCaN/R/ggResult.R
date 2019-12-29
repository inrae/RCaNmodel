
#' ggResult
#'
#' provide a ggplot of the fit
#' @param mcmc_res result sent by \link{fitmyCaNmod}
#' @param myCaNmod a CaNmod object
#' @param param the name of a parameter (either a flow or a biomass)
#' @param colour the colour of the plot
#'
#'
#' @return a ggplot
#' @export
#' @details the line corresponds to median of the mcmc simulation, the ribbon corresponds to quantiles 2.5% and 97.5%
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx", package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' ggResult(res,myCaNmod,"F01")
#'
#' @importFrom coda as.matrix
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ylab
#'
ggResult <- function(mcmc_res, myCaNmod, param, colour = "blue") {
  mat_res <- as.matrix(mcmc_res)
  columns <- grep(paste(param, "\\[",sep=""),colnames(mat_res))
  if (length(colums) == 0)
    stop("param not recognized")
  quantiles <-
    data.frame(t(apply(mat_res[, columns], 2, quantile, probs = c(.025, .5, .975))))
  names(quantiles) = c("q2.5", "q50", "q97.5")
  quantiles$year = myCaNmod$series$Year
  ggplot(quantiles, aes_string(x = "year", y = "q50")) + geom_line(colour =
                                                                     colour) + geom_ribbon(aes_string(ymin = "q2.5", ymax = "q97.5"),
                                                                                           fill = colour,
                                                                                           alpha = .33) + ylab(param)
}
