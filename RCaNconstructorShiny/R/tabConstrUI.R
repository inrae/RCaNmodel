#' tabConstrUI
#'
#' Visualize tables
#'
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow h1 selectInput
#' @export
#'

tabConstrUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1("CONSTRAINTS"),
          fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                   shinyjs::disabled(actionButton(ns("cancel"), "CANCEL"))),
          fluidRow(selectInput(ns("filter"),
                               "Constraint", 
                               "All", 
                               "All")),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
