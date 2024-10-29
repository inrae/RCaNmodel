#' tableEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param slot either fluxes or components
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe
#' @export

tableEditorServer <- function(id, network, slot, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""

      hiddencols <- c("from", "to", "id")

      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()


      observe({
        network$components
        network$observations
        network$fluxes
        network$dictionary
        network$model
        req(isolate(tab$panel) == currenttab)
        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }
        output$tableedit <- rendertab(tmpnetwork[[slot]],
                                      tmpnetwork$components$Component)
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
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
                              any_of(c("Inside", "Trophic")),
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
                    source = tmpnetwork$components$Component) %>%
            hot_col("Trophic", type = "checkbox")
        } else {
          tab <- tab %>%
            hot_col("Inside", type = "checkbox")
        }
        renderRHandsontable({tab})
      }




      shiny::observe({
        input$tableedit$changes$changes
        req(!is.null(input$tableedit$changes$changes))
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })

      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(input$tableedit) %>%
          mutate(across(any_of(c("Inside" , "Trophic")), as.integer))
        shinyCatch({
          if (max(table(c(newdata[, ifelse(slot == "fluxes",
                                           "Flux",
                                           "Component")],
                          isolate(names(tmpnetwork$observations)),
                          tmpnetwork[[ifelse(slot == "fluxes",
                                             "components",
                                             "fluxes")]][,
                                                         ifelse(slot == "fluxes",
                                                                "Component",
                                                                "Flux")]))) > 1)
            stop("a name is already used")
        })

        if (slot == "fluxes"){
          newdata$from <- names(isolate(network$dictionary))[network$dictionary == newdata$From]
          newdata$to <- names(isolate(network$dictionary))[network$dictionary == newdata$To]
          tmpnetwork$fluxes <<- newdata
        } else {
          newfluxes <- isolate(network$fluxes)
          newfluxes$From <- newdata$Component[newfluxes$from == newdata$id]
          newfluxes$To <- newdata$Component[newfluxes$to == newdata$id]
          # newfluxes$Flux <- paste(newdata$Component[newfluxes$from == newdata$id],
          #                         newdata$Component[newfluxes$to == newdata$id],
          #                         sep = "_")
          tmpnetwork$fluxes <<- newfluxes
          tmpnetwork$components <<- newdata

        }

        # newnetwork[[slot]] <<- isolate(tmpnetwork[[slot]])
        # shinyjs::disable("ok")
        # shinyjs::disable("cancel")

      })
      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(tmpnetwork[[slot]],
                                      tmpnetwork$components$Component)
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
      })


      observe({
        ntab <- tab$panel
        if ((currenttab == "Components" & ntab != "Components" & slot == "components") |
            (currenttab == "Fluxes" & ntab != "Fluxes" & slot == "fluxes")){
          for (v in names(tmpnetwork)){
            if (!identical(tmpnetwork[[v]],
                           isolate(newnetwork[[v]])))
              newnetwork[[v]] <<- tmpnetwork[[v]]
          }
        }
        currenttab <<- ntab
      })

      return(newnetwork)

    }
  )
}
