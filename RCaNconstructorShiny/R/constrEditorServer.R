#' constrEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom shiny isolate observe updateRadioGroupButtons updatePickerInput
#' @export

constrEditorServer <- function(id, network, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""

      hiddencols <- c("idconstraint")

      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()
      constraint <- character(0)


      observeEvent(input$constraintselect, {
        req(input$constraintselect)
        if (input$constraintselect == "New"){
          updateTextInput(session,
                          "newname",
                          "")
          shinyjs::enable("newname")
          constraint <<- character(0)
          updateRadioGroupButtons(session,
                                  "equations",
                                  choiceValues = "",
                                  choiceNames = "",
                                  selected = character(0))
        } else {
          shinyjs::disable("newname")
        }
      })


      addVal <- function(text) {
        pos <- isolate(input$equations)
        if (is.null(pos)){
          constraint <<- c(constraint, text)
          selected <- character(0)
        } else {
          constraint <<- append(constraint,
                               text,
                               after = as.integer(pos) - 1)
          selected <- pos + 1
        }

        updateRadioGroupButtons(session,
                                "equations",
                                choiceValues = seq_along(constraint),
                                choiceNames = constraint,
                                selected = selected)
      }





      observe({
        network$components
        network$observations
        network$fluxes
        network$dictionary
        network$constraints
        network$model
        req(isolate(tab$panel) == currenttab)


        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }


        if (nrow(tmpnetwork$observations) > 0)
          updateRadioGroupButtons(session,
                                  "obs",
                                  choices = setdiff(names(tmpnetwork$observations),
                                                    "Year"))

        if (nrow(tmpnetwork$constraints) > 0)
          updatePickerInput(session,
                            "constraintselect",
                            choices = c("New",
                                        sort(tmpnetwork$constraints$Id)))

        if (nrow(tmpnetwork$observations) > 0)
          updateRadioGroupButtons(session,
                                  "years",
                                  choices = sort(tmpnetwork$observations$Year))

        if (nrow(tmpnetwork$components) > 0)
          updateRadioGroupButtons(session,
                                  "components",
                                  choices = sort(tmpnetwork$components$Component))

        if (nrow(tmpnetwork$fluxes) > 0)
          updateRadioGroupButtons(session,
                                  "fluxes",
                                  choices = sort(c(tmpnetwork$fluxes$Flux,
                                                   "Allflows")))

      })


      observe({
        ntab <- tab$panel
        if (currenttab == "View Constraints" & ntab != "View Constraints"){
          for (v in names(tmpnetwork)){
            if (!identical(tmpnetwork[[v]],
                           isolate(newnetwork[[v]])))
              newnetwork[[v]] <<- tmpnetwork[[v]]
          }
        }
        currenttab <<- ntab
      })

      observeEvent(input$years,{
        addVal(isolate(input$years))
        updateRadioGroupButtons(session,
                                "years",
                                selected = character(0))
      }
      )

      observeEvent(input$obs,{
        addVal(isolate(input$obs))
        updateRadioGroupButtons(session,
                                "obs",
                                selected = character(0))
      }
      )

      observeEvent(input$fluxes,{
        addVal(isolate(input$fluxes))
        updateRadioGroupButtons(session,
                                "fluxes",
                                selected = character(0))
      }
      )

      observeEvent(input$components,{
        addVal(isolate(input$components))
        updateRadioGroupButtons(session,
                                "components",
                                selected = character(0))
      }
      )

      observeEvent(input$brackets,{
        addVal(isolate(input$brackets))
        updateRadioGroupButtons(session,
                                "brackets",
                                selected = character(0))
      }
      )

      observeEvent(input$functions,{
        addVal(isolate(input$functions))
        updateRadioGroupButtons(session,
                                "functions",
                                selected = character(0))
      }
      )

      observeEvent(input$numbers,{
        addVal(isolate(input$numbers))
        updateRadioGroupButtons(session,
                                "numbers",
                                selected = character(0))
      }
      )

      observeEvent(input$comparisons,{
        addVal(isolate(input$comparisons))
        updateRadioGroupButtons(session,
                                "comparisons",
                                selected = character(0))
      }
      )


      observeEvent(input$modifiers,{
        addVal(isolate(input$modifiers))
        updateRadioGroupButtons(session,
                                "modifiers",
                                selected = character(0))
      }
      )


      observeEvent(input$remove,{
        req(isolate(input$equations))
        constraint <<- constraint[-as.integer(isolate(input$equations))]
        if (length(constraint) == 0){
          updateRadioGroupButtons(session,
                                  "equations",
                                  choiceValues = "",
                                  choiceNames = "")
        } else {
        updateRadioGroupButtons(session,
                                "equations",
                                choiceValues = seq_along(constraint),
                                choiceNames = constraint,
                                selected = character(0))
        }

      })
      return(newnetwork)
    }
  )
}
