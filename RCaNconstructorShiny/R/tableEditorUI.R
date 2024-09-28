#' tabEditorUI
#' Editor of tables
#' @param id the id of ui
#' @param title the tital to display
#' @importFrom shiny actionButton fluidRow h1
#' @export
#'

tabEditorUI <- function(id, title){
    ns <- NS(id)
    tagList(shinyjs::useShinyjs(),
            h1(title),
            fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                     shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
            fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
    )
}
