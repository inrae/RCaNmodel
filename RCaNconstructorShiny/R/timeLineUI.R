#' timeLineUI
#' ui of the file timeline editorœ
#' @param id the id of the ui
#'
#' @return nothing
#'
#' @importFrom datamods edit_data_ui
#' @importFrom shiny NS fluidRow tagList h3
#' @importFrom shinyjs useShinyjs 
#' @export
#'

timeLineUI <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          fluidRow(
            edit_data_ui(ns("edittl"))
          ))
}
