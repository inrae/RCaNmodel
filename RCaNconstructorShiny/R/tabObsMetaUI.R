#' tabObsMetaUI
#'
#' edit meata information tables
#'
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow h1
#' @export
#'

tabObsMetaUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1("OBSERVATIONS Descriptions"),
          fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                   shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
          fluidRow(actionButton(ns("add"), "Add series"),
                   actionButton(ns("remove"), "Remove series")),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
