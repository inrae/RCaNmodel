#' fileInteractionUI
#' ui of the file interaction visual editor
#' @param id the id of the ui
#'
#' @return nothing
#'
#' @importFrom shiny NS fluidRow tagList
#' @importFrom shinyjs useShinyjs
#' @export
#'

fileInteractionUI <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          fluidRow(
            actionButton(ns("new"), "new"),
            actionButton(ns("open"), "open"),
            downloadButton(ns("savename"), "save")),
          fluidRow(
            infoUI(ns("info"))
          ))
}
