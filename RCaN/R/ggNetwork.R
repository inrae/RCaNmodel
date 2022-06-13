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
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 arrow
#' @importFrom graphics plot
#' @export
ggNetwork <- function(myCaNmod) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    print("Package ggplot2 needed for this function to work.
         Please install it",
         call. = FALSE)
    return()
  }
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    print("Package ggraph needed for this function to work.
         Please install it",
         call. = FALSE)
    return()
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    print("Package igraph needed for this function to work.
         Please install it",
         call. = FALSE)
    return()
  }
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
    igraph::graph_from_data_frame(d = edge_graph, vertices = vertices_graph)

  gg_foodweb <- ggraph::ggraph(g, "circle") +
    ggraph::geom_node_point(aes_(
      x = quote(x * 1.05),
      y = quote(y * 1.05),
      colour = quote(Status)
    ),
    size = 5,
    alpha = 0.33) +
    ggraph::geom_node_text(aes_(
      x = quote(x * 1.1),
      y = quote(y * 1.1),
      label = quote(name)
    ),
    size = 3,
    alpha = 1) +
    ggraph::geom_edge_arc2(
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
  if (sum(igraph::which_loop(g)) > 0) {
    gg_foodweb <- gg_foodweb +
      ggraph::geom_edge_loop(
        aes_string(
          span = 90,
          direction = 90,
          label = "Flux",
          colour = "Trophic"
        ),
        width = 1,
        end_cap = ggraph::circle(0.1, "inches"),
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
