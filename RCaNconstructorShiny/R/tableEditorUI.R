#' tabEditorUI
#' Editor of tables
#' @param id the id of ui
#' @param title the title to display
#' @importFrom shiny actionButton fluidRow h1 selectInput
#' @export
#'

tabEditorUI <- function(id, title){
    ns <- NS(id)
    tagList(shinyjs::useShinyjs(),
            h1(title),
            fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                     shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
            fluidRow(selectInput(ns("filter"),
                                 "", 
                                 "All", 
                                 "All")),
            fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
    )
}
