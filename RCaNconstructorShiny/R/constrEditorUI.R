#' constrEditorUI
#' Editor of constrEditorUI
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow h1
#' @export
#'

constrEditorUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs()
          )

}
