#' ggCaNmod
#' produces a ggraph representing the flow foodweb
#' @param myCaNmod a CaNmod oject
#' @param ... Additional arguments
#' @return a ggraph
#'
#' @examples
#' require(ggraph)
#' require(ggplot2)
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx", package = "RCaN"))
#' ggCaNmod(myCaNmod)
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_edge_arc
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_arc2
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggraph geom_edge_loop
#' @importFrom ggraph geom_node_point
#' @importFrom cpgsR getBoundParam
#' @importFrom graphics plot
#' @export
ggCaNmod <- function(myCaNmod) {
  # need to order the links table so that it matches the order of the components
  edge_graph <-
    myCaNmod$fluxes_def[, c("From", "To", "Trophic", "Flux")]
  edge_graph$Trophic <- as.factor(edge_graph$Trophic)
  names(edge_graph)[c(1:2)] <- c("from", "to")
  vertices_graph <-
    myCaNmod$components_param[, c("Component", "in_out")]

  g <-
    graph_from_data_frame(d = edge_graph, vertices = vertices_graph)

  gg_foodweb <- ggraph(g, 'circle') +
    geom_node_point(aes(
      x = x * 1.05,
      y = y * 1.05,
      colour = in_out
    ),
    size = 5,
    alpha = 0.33) +
    geom_node_text(aes(
      x = x * 1.1,
      y = y * 1.1,
      label = name
    ),
    size = 3,
    alpha = 1) +
    geom_edge_arc2(
      aes_string(label = "Flux", colour = "Trophic"),
      strength = 0.1,
      alpha = 0.33,
      width = 1,
      arrow = arrow(
        angle = 30,
        length = unit(0.1, "inches"),
        ends = "last",
        type = "closed"
      )
    )
  if(sum(which_loop(g))>0){
    gg_foodweb <- gg_foodweb +
    geom_edge_loop(
      aes_string(
        span = 90,
        direction = 90,
        label = "Flux",
        colour = "Trophic"
      ),
      width = 1,
      end_cap = circle(0.1, 'inches'),
      alpha = 0.33,
      arrow = arrow(
        angle = 30,
        length = unit(0.1, "inches"),
        ends = "last",
        type = "closed"
      )
    )
    }
  gg_foodweb <-gg_foodweb +
    theme(aspect.ratio = 1)
  gg_foodweb
}
