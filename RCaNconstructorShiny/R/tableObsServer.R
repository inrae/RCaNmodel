#' tableObsServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#' @return a data.frame observation
#' @importFrom magrittr %>%
#' @importFrom dplyr anti_join bind_rows mutate select across any_of arrange all_of
#' @importFrom rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe req validate need selectInput modalButton
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @export

tableObsServer <- function(id, network, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""
      
      tabOnewnetwork <- createEmptyNetwork()
      tmpnetwork <- list()
      
      observe({
        #this observes any change in network
        for (v in names(network)){
          network[[v]]
        }
        
        req(isolate(tab$panel) == currenttab)
        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]])){
            tmpnetwork[[v]] <<- isolate(network[[v]])
          }
        }
        renamed <- tmpnetwork$metaobs$id %>%
          setNames(tmpnetwork$metaobs$Observation)
        output$tableedit <- rendertab(tmpnetwork$observations %>%
                                        dplyr::rename(all_of(renamed)))
        
      })
      
      rendertab <- function(data){
        tab <-
          rhandsontable(data,
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(manualColumnResize = TRUE)
        for (col in names(data))
          tab <- tab %>%
          hot_col(col, "numeric")
        tab <- tab %>%
          hot_col("Year", readOnly = TRUE, format = "0000")
        renderRHandsontable({tab})
      }
      
      
      
      
      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(input$tableedit)
        req(newdata)
        renamed <- tmpnetwork$metaobs$Observation %>%
          setNames(tmpnetwork$metaobs$id)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          mutate(across(any_of(c("Year")), as.integer))
        shinyCatch({
          if (max(table(c(names(newdata),
                          tmpnetwork$fluxes$Flux,
                          tmpnetwork$components$Components))) > 1)
            stop("a name is already used")
          tmpnetwork$observations <<- newdata %>%
            dplyr::rename(renamed)
        })
      })
      
      
      
      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(tmpnetwork$observations)
      })
      
      
      observe({
        ntab <- tab$panel
        if (currenttab == "Time Series" & ntab != "Time Series"){
          for (v in names(tmpnetwork)){
            if (!identical(tmpnetwork[[v]],
                           isolate(tabOnewnetwork[[v]])))
              tabOnewnetwork[[v]] <<- tmpnetwork[[v]]
          }
        }
        currenttab <<- ntab
      })
      
      return(tabOnewnetwork)
      
    }
  )
}
