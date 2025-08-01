#' timeLineServer
#'
#' @param id the id of the ui
#' @param timeline the timeline
#'
#' @return an updated network
#' @importFrom datamods edit_data_server
#' @importFrom shiny reactiveValues reactive
#' @export

timeLineServer <- function(id, timeline){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      newtimeline <- reactiveValues(timeline = createEmptyTimeLine())
      
      observe({
        newtimeline$timeline <<- timeline$timeline
      })
      
      newtimeline$timeline <- edit_data_server("edittl", 
                                      reactive(newtimeline$timeline),
                                      add = TRUE,
                                      update = TRUE,
                                      delete = TRUE,
                                      download_excel = FALSE,
                                      download_csv = FALSE)
      
      return(newtimeline)
      
    }
  )
}
