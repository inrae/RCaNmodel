#' launch the Shiny App
#'
#' @param ... options to shinyApp
#' @return nothing
#' @export
#'
launchConstructor <- function(...){

shiny::shinyApp(ui = RCaNconstructorUI,
                server = RCaNconstructorServer,
                options = list(...))
}
