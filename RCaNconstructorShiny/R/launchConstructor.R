#' launch the Shiny App
#'
#' @param normal by default, use display.mode = "showcase" for debugging
#' @return nothing
#' @export
#'
launchConstructor <- function(display.mode = "normal"){
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
                server = RCaNconstructorServer,
                options = list(display.mode = display.mode))
}
