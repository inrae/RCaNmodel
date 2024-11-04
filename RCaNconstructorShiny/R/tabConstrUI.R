#' tabConstrUI
#' Visualize tables
#' @param id the id of ui
#' @param title the title to display
#' @importFrom shiny actionButton fluidRow h1
#' @export
#'

tabConstrUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1("CONSTRAINTS"),
          fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                   shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
