#' visNetworkUI
#' ui of the network visual editor
#' @param id the id of the ui
#'
#' @return nothing
#'
#' @importFrom shiny NS fluidRow tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom visNetwork visNetworkOutput
#' @export
#'
visNetworkUI <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          fluidRow(visNetworkOutput(ns("networkviz_proxy"),
                                    width = "100%",
                                    height = "400px")))
}
