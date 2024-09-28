#' launch the Shiny App
#'
#' @return nothing
#' @export
#'
launchConstructor <- function(){
  # library(shiny)
  # library(shinyjs)
  # library(dplyr)
  # library(magrittr)
  # library(RCaNmodel)
  # library(shiny)
  # library(shinyjs)
  # library(spsComps)
  # library( visNetwork)
  # library(rhandsontable)
  # source("R/RCaNconstructorServer.R")
  # source("R/RCaNconstructorUI.R")
  # source("R/visNetworkServer.R")
  # source("R/visNetworkUI.R")
  # source("R/tableEditorServer.R")
  # source("R/tableEditorUI.R")

shiny::shinyApp(ui = RCaNconstructorUI,
                server = RCaNconstructorServer)
}
