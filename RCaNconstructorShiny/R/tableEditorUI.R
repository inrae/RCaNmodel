#' tabEditorUI
#' Editor of tables
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow selectInput
#' @export
#'

tabEditorUI <- function(id){
    ns <- NS(id)
    tagList(shinyjs::useShinyjs(),
            fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                     shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
            fluidRow(selectInput(ns("filter"),
                                 "", 
                                 "All", 
                                 "All")),
            fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
    )
}
