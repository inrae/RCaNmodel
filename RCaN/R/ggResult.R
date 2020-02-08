
#' ggResult
#'
#' provide a ggplot of the fit
#' @param mcmc_res result sent by \link{fitmyCaNmod}
#' @param myCaNmod a CaNmod object
#' @param param the name (or a vector of name) of a parameter (either a flow or
#' a biomass)
#' @param ylab default Biomass Flux
#' @param plot_series if yes, draw three example of trajectories
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
ggResult <- function(mcmc_res, myCaNmod, param, plot_series, ylab="Biomass/Flux") {
  mat_res <- as.matrix(mcmc_res)
  quantiles <- do.call("rbind.data.frame", lapply(param, function(p) {
    columns <- which(startsWith(colnames(mat_res), paste(p, "[", sep = "")))
    if (length(columns) == 0)
      stop("param not recognized")
    quantiles <-
      data.frame(t(apply(
        mat_res[, columns],
        2,
        quantile,
        probs = c(0, .025, 0.25, .50, .75, .975, 1)
      )),
      year = myCaNmod$series$Year,
      series = as.character(p))
  }))
  names(quantiles)[1:7] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  g <- ggplot() +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",ymin = "q0",ymax = "q100",fill = "series"),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",ymin = "q2.5",ymax = "q97.5",fill = "series"),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",ymin = "q25",ymax = "q75",fill = "series"),
                alpha = .33) +
    ylab(ylab) +
    facet_wrap(quantiles$series,scales='free')

  if (plot_series){
    fewseries <- do.call("rbind.data.frame", lapply(param, function(p) {
      columns <- which(startsWith(colnames(mat_res), paste(p, "[", sep = "")))
      if (length(columns) == 0)
        stop("param not recognized")
      selectedsamples <- sample(seq_len(nrow(mat_res)), size=3)
      fewseries <-
        data.frame(t(apply(
          mat_res[selectedsamples, columns],2,identity
        )),
        year = myCaNmod$series$Year,
        series = as.character(p))
    }))
    names(fewseries)[1:3] <- c("S1", "S2", "S3")
    g <- g + geom_path(data = fewseries,
                       aes_string(x = "year", y = "S1", col ="series"),
                       lty = "solid") +
      geom_path(data = fewseries,
                aes_string(x = "year", y = "S2", col ="series"),
                lty = "twodash") +
      geom_path(data = fewseries,
                aes_string(x = "year", y = "S3", col ="series"),
                lty = "longdash")
  }
  return (g)
}
