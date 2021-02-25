#' pdfCaNmod
#' creates a pdf summary of the case study
#' @param myCaNmod a CaNmod oject
#' @param output_file name of the output file
#' @export
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' pdfCaNmod(myCaNmod)
#' @importFrom gridExtra ttheme_default
#' @importFrom gridExtra grid.table
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 rel
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_wrap
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics plot.new
#' @importFrom stats time

pdfCaNmod <- function(myCaNmod, output_file = "summaryCaNmod.pdf") {
  # step 2: draw a table with the input parameters
  pdf(output_file)
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.6)),
    rowhead = list(fg_params = list(cex = 0.6))
  )
  grid.table(myCaNmod$components, theme = mytheme)

  gg_foodweb <- ggCaNmod(myCaNmod)

  print(gg_foodweb)

  # step 4: draw all time series
  series <- as.data.frame(myCaNmod$series)
  ns <- dim(series)[2] - 1
  for (i in 1:ceiling(ns / 9)) {
    TS <-
      gather(series[, ((i - 1) * 9 + 2):(min(ns, (i * 9)) + 1)], key =
               "series", value = "value")
    TS$time <-
      rep(series[, 1], min(ns, (i * 9)) - (i - 1) * 9)
    gg_timeseries <- ggplot(TS, aes_string(x = "time", y = "value")) +
      facet_wrap(. ~ series, scales = "free") +
      geom_line(colour = "darkcyan") +
      geom_point(colour = "darkcyan", size = rel(.8)) +
      labs(x = "time", y = "value") +
      theme(axis.text = element_text(size = rel(.5)))
    print(gg_timeseries)
  }

  # step 5: draw a table with all the constraints
  nc <- dim(myCaNmod$constraints)[1] # number of constraints
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.9)),
    colhead = list(fg_params = list(cex = 0.9)),
    rowhead = list(fg_params = list(cex = 0.9))
  )
  for (i in 1:ceiling(nc / 20)) {
    plot.new()
    grid.table(myCaNmod$constraints[((i - 1) * 20 + 1):min(nc, (i * 20)), ],
               theme = mytheme)
  }

  dev.off()
}
