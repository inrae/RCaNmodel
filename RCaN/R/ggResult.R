
#' ggResult
#'
#' provide a ggplot of the fit
#' @param mcmc_res result sent by \link{fitmyCaNmod}
#' @param myCaNmod a CaNmod object
#' @param param the name (or a vector of name) of a parameter (either a flow or
#' a biomass)
#' @return a ggplot
#' @details the line corresponds to median of the mcmc simulation, the ribbon
#' corresponds to quantiles 2.5% and 97.5%
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- fitmyCaNmod(myCaNmod, 100)
#' #with one series
#' ggResult(res,myCaNmod,"F01")
#'
#' #with 2 series
#' ggResult(res,myCaNmod,c("F01","HerbZooplankton"))
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ylab
#' @importFrom stats quantile

#' @export
#'
ggResult <- function(mcmc_res, myCaNmod, param) {
  mat_res <- as.matrix(mcmc_res)
  quantiles <- do.call("rbind.data.frame", lapply(param, function(p) {
    columns <- grep(paste(p, "\\[", sep = ""), colnames(mat_res))
    if (length(columns) == 0)
      stop("param not recognized")
    quantiles <-
      data.frame(t(apply(
        mat_res[, columns], 2, quantile, probs = c(.025, .5, .975)
      )),
      year = myCaNmod$series$Year,
      series = as.character(p))
  }))
  names(quantiles)[1:3] <- c("q2.5", "q50", "q97.5")
  ggplot() + geom_line(data = quantiles, aes_string(x = "year", y = "q50", col =
                                                      "series")) + geom_ribbon(
                                                        data = quantiles,
                                                        aes_string(
                                                          x = "year",
                                                          ymin = "q2.5",
                                                          ymax = "q97.5",
                                                          fill = "series"
                                                        ),
                                                        alpha = .33
                                                      ) + ylab(param)
}
