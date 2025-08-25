#' tabConstrUI
#'
#' Visualize tables
#'
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow  selectInput
#' @export
#'

tabConstrUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          fluidRow(shinyjs::disabled(actionButton(ns("ok"), "VALIDATE")),
                   shinyjs::disabled(actionButton(ns("cancel"), "CANCEL")),
                   actionButton(ns("checkvalid"), "check validity")),
          fluidRow(selectInput(ns("filter"),
                               "Constraint", 
                               "All", 
                               "All")),
          fluidRow(rhandsontable::rHandsontableOutput(ns("tableedit")))
  )
}
