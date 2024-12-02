#' tableObsUI
#'
#' Editor of tables
#'
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow numericInput
#' @export
#'

tableObsUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          fluidRow(actionButton(ns("ok"), "VALIDATE"),
                   actionButton(ns("cancel"), "CANCEL")),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
