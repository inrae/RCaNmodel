
#' ggSeries
#'
#' provide a ggplot of the fit
#' @param mysampleCaNmod result sent by \link{sampleCaN}
#' @param param the name (or a vector of name) of a parameter (either a flow or
#' a biomass)
#' @param ylab default Biomass Flux
#' @param plot_series if yes, draw three example of trajectories (default TRUE)
#' @return a ggplot
#' @details the line corresponds to median of the mcmc simulation, the ribbon
#' corresponds to quantiles 2.5% and 97.5%
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' res <- sampleCaN(myCaNmod, 100)
#' #with one series
#' ggSeries(res,"F01", TRUE)
#'
#' #with 2 series
#' ggSeries(res,c("F01","HerbZooplankton"), TRUE)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ylab
#' @importFrom stats quantile

#' @export
#'
ggSeries <- function(mysampleCaNmod,
                     param,
                     plot_series = TRUE,
                     ylab = "Biomass/Flux") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (! inherits(mysampleCaNmod, "sampleCaNmod"))
    stop("you should provide a sampleCaNmod object")
  mat_res <- as.matrix(mysampleCaNmod$mcmc)

  # take some random lines that can be drawn consistently among series
  selectedsamples <- sample(seq_len(nrow(mat_res)), size = 3)

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
      year = mysampleCaNmod$CaNmod$series$Year[
        paste(p, "[", mysampleCaNmod$CaNmod$series$Year, "]", sep = "") %in%
          colnames(mat_res)[columns]
      ],
      series = as.character(p))
  }))
  quantiles$series <- factor(as.character(quantiles$series),
                                levels = param)
  names(quantiles)[1:7] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  g <- ggplot() +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",
                           ymin = "q0",
                           ymax = "q100",
                           fill = "series"),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",
                           ymin = "q2.5",
                           ymax = "q97.5",
                           fill = "series"),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes_string(x = "year",
                           ymin = "q25",
                           ymax = "q75",
                           fill = "series"),
                alpha = .33) +
    ylab(ylab) +
    facet_wrap(~quantiles$series, scales = "free")

  if (plot_series) {
    fewseries <- do.call("rbind.data.frame", lapply(param, function(p) {
      columns <- which(startsWith(colnames(mat_res), paste(p, "[", sep = "")))
      if (length(columns) == 0)
        stop("param not recognized")
      fewseries <-
        data.frame(t(apply(
          mat_res[selectedsamples, columns], 2, identity
        )),
        year = mysampleCaNmod$CaNmod$series$Year[
          paste(p, "[", mysampleCaNmod$CaNmod$series$Year, "]", sep = "") %in%
            colnames(mat_res)[columns]
          ],
        series = as.character(p))
    }))
    fewseries$series <- factor(as.character(fewseries$series),
                      levels = param)
    names(fewseries)[1:3] <- c("S1", "S2", "S3")
    g <- g + geom_path(data = fewseries,
                       aes_string(x = "year", y = "S1", col = "series"),
                       lty = "solid") +
      geom_path(data = fewseries,
                aes_string(x = "year", y = "S2", col = "series"),
                lty = "twodash") +
      geom_path(data = fewseries,
                aes_string(x = "year", y = "S3", col = "series"),
                lty = "longdash")
  }
  return(g)
}
