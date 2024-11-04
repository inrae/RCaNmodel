#' tableObsUI
#'
#' Editor of tables
#'
#' @param id the id of ui
#' @param title the tital to display
#' @importFrom shiny actionButton fluidRow h1 numericInput
#' @export
#'

tableObsUI <- function(id, title){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1(title),
          fluidRow(actionButton(ns("ok"), "VALIDATE"),
                   actionButton(ns("cancel"), "CANCEL")),

          fluidRow(actionButton(ns("add"), "Add series"),
                   actionButton(ns("remove"), "Remove series")),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
