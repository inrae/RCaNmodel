#' ggNetwork
#' produces a ggraph representing the flow foodweb
#' @param myCaNmod a CaNmod oject
#' @return a ggraph
#'
#' @examples
#' require(ggraph)
#' require(ggplot2)
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' ggNetwork(myCaNmod)
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph which_loop
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_edge_arc
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_arc2
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggraph geom_edge_loop
#' @importFrom ggraph circle
#' @importFrom ggraph geom_node_point
#' @importFrom graphics plot
#' @export
ggNetwork <- function(myCaNmod) {
  # need to order the links table so that it matches the order of the components
  edge_graph <-
    myCaNmod$fluxes_def[, c("From", "To", "Trophic", "Flux")]
  edge_graph$Trophic <- as.factor(edge_graph$Trophic)
  names(edge_graph)[c(1:2)] <- c("from", "to")
  vertices_graph <-
    myCaNmod$components_param[, c("Component", "Inside")]
  vertices_graph$Inside <- ifelse(vertices_graph$Inside == 1, "internal",
                                  "external")
  names(vertices_graph)[2] <- "Status"

  g <-
    graph_from_data_frame(d = edge_graph, vertices = vertices_graph)

  gg_foodweb <- ggraph(g, "circle") +
    geom_node_point(aes_(
      x = quote(x * 1.05),
      y = quote(y * 1.05),
      colour = quote(Status)
    ),
    size = 5,
    alpha = 0.33) +
    geom_node_text(aes_(
      x = quote(x * 1.1),
      y = quote(y * 1.1),
      label = quote(name)
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
  if (sum(which_loop(g)) > 0) {
    gg_foodweb <- gg_foodweb +
      geom_edge_loop(
        aes_string(
          span = 90,
          direction = 90,
          label = "Flux",
          colour = "Trophic"
        ),
        width = 1,
        end_cap = circle(0.1, "inches"),
        alpha = 0.33,
        arrow = arrow(
          angle = 30,
          length = unit(0.1, "inches"),
          ends = "last",
          type = "closed"
        )
      )
  }
  gg_foodweb <- gg_foodweb +
    theme(aspect.ratio = 1)
  gg_foodweb
}
