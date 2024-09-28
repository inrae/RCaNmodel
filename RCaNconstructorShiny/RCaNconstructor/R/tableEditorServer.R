#' tableEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param slot either fluxes or components
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe
#' @export

tableEditorServer <- function(id, network, slot){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      hiddencols <- c("from", "to", "id")

      newnetwork <- reactiveValues()
      observe({
        newnetwork$components <- network$components
        newnetwork$fluxes <- network$fluxes
      })

      rendertab <- function(data, complist){
        tab <-
          rhandsontable(data %>%
                          mutate(
                            across(
                              any_of(c("To", "From")),
                              ~ factor(.x,
                                       levels = complist)),
                            across(
                              any_of(c("In", "Trophic")),
                              ~ as.logical(.x ))),
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(colWidths = ifelse(names(data) %in% hiddencols,
                                      1,
                                      200))
        if (slot == "fluxes"){
          tab <- tab %>%
            hot_col(c("From", "To"),
                    type = "dropdown",
                    source = newnetwork$components$component) %>%
            hot_col("Trophic", type = "checkbox")
        } else {
          tab <- tab %>%
            hot_col("In", type = "checkbox")
        }
        renderRHandsontable({tab})
      }


      observe({
        output$tableedit <- rendertab(newnetwork[[slot]],
                                      newnetwork$components$component)
        shinyjs::disable(input$ok)
        shinyjs::disable(input$cancel)
      })

      shiny::observe({
        input$tableedit$changes$changes
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })

      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(input$tableedit) %>%
          mutate(across(any_of(c("In" , "Trophic")), as.integer))

        if (slot == "fluxes"){
          newdata$from <- names(isolate(network$dictionary))[network$dictionary == newdata$From]
          newdata$to <- names(isolate(network$dictionary))[network$dictionary == newdata$To]
          newnetwork$fluxes <<- newdata
        } else {
          newfluxes <- isolate(network$fluxes)
          newfluxes$From <- newdata$component[newfluxes$from == newdata$id]
          newfluxes$To <- newdata$component[newfluxes$to == newdata$id]
          isolate({newnetwork$fluxes <<- newfluxes})
          newnetwork$components <<- newdata

        }

        # newnetwork[[slot]] <<- isolate(tmpnetwork[[slot]])
        # shinyjs::disable("ok")
        # shinyjs::disable("cancel")

      })
      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(newnetwork[[slot]],
                                      newnetwork$components$component)
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
      })



      return(newnetwork)

    }
  )
}
