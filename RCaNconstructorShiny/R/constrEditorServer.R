#' constrEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom spsComps shinyCatch
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom shiny isolate observe
#' @importFrom shinyWidgets updateRadioGroupButtons updatePickerInput
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



      resetPage <- function() {
        updateTextInput(session,
                        "newname",
                        "")
        shinyjs::enable("newname")
        constraint <<- character(0)
        updateRadioGroupButtons(session,
                                "equations",
                                choices = "",
                                selected = character(0))

        req(tmpnetwork$constraints)


        if (nrow(tmpnetwork$constraints) > 0)
          updatePickerInput(session,
                            "constraintselect",
                            choices = c("New",
                                        sort(tmpnetwork$constraints$Id)),
                            selected = "New")



      }



      observeEvent(input$constraintselect, {
        req(input$constraintselect)
        if (input$constraintselect == "New"){
          resetPage()
        } else {
          shinyjs::disable("newname")
          idconstraint <- tmpnetwork$constraints$idconstraint[tmpnetwork$constraints$Id == input$constraintselect]
          constraint <<- getConstraintWord(
            convertidConstr2Constr(
              idconstraint,
              tmpnetwork$dictionary))
          updateRadioGroupButtons(session,
                                  "equations",
                                  choiceValues = seq_along(constraint),
                                  choiceNames = constraint,
                                  selected = character(0))
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
                                  size = "xs",
                                  choices = setdiff(names(tmpnetwork$observations),
                                                    "Year"),
                                  selected = character(0))



        if (nrow(tmpnetwork$observations) > 0)
          updateRadioGroupButtons(session,
                                  "years",
                                  size = "xs",
                                  choices = sort(tmpnetwork$observations$Year),
                                  selected = character(0))

        if (nrow(tmpnetwork$components) > 0)
          updateRadioGroupButtons(session,
                                  "components",
                                  size = "xs",
                                  selected = character(0),
                                  choices = sort(tmpnetwork$components$Component))

        if (nrow(tmpnetwork$fluxes) > 0)
          updateRadioGroupButtons(session,
                                  "fluxes",
                                  size = "xs",
                                  selected = character(0),
                                  choices = sort(c(tmpnetwork$fluxes$Flux,
                                                   "Allflows")))


        resetPage()

      })




      observeEvent(input$years,{
        addVal(isolate(input$years))
        updateRadioGroupButtons(session,
                                "years",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$obs,{
        addVal(isolate(input$obs))
        updateRadioGroupButtons(session,
                                "obs",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$fluxes,{
        addVal(isolate(input$fluxes))
        updateRadioGroupButtons(session,
                                "fluxes",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$components,{
        addVal(isolate(input$components))
        updateRadioGroupButtons(session,
                                "components",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$brackets,{
        addVal(isolate(input$brackets))
        updateRadioGroupButtons(session,
                                "brackets",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$functions,{
        addVal(isolate(input$functions))
        updateRadioGroupButtons(session,
                                "functions",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$numbers,{
        addVal(isolate(input$numbers))
        updateRadioGroupButtons(session,
                                "numbers",
                                size = "xs",
                                selected = character(0))
      }
      )

      observeEvent(input$comparisons,{
        addVal(isolate(input$comparisons))
        updateRadioGroupButtons(session,
                                "comparisons",
                                size = "xs",
                                selected = character(0))
      }
      )


      observeEvent(input$modifiers,{
        addVal(isolate(input$modifiers))
        updateRadioGroupButtons(session,
                                "modifiers",
                                size = "xs",
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

      observeEvent(input$ok,{
        req(length(constraint) > 0)
        shinyCatch({
          formula <- paste(constraint, collapse = "")
          valid <- checkValidity(formula, tmpnetwork)
          if (valid != "TRUE")
            stop(paste(formula, valid, sep = ":"))

          idformula <- convertConstr2idConstr(formula,
                                              tmpnetwork$dictionary)
          if (isolate(input$constraintselect) == "New"){
            if (isolate(input$newname) %in% tmpnetwork$constraint$Id)
              stop("Id already used")

            tmpnetwork$constraints <<- tmpnetwork$constraints %>%
              dplyr::bind_rows(
                data.frame(
                  Id = isolate(input$newname),
                  Constaint = formula,
                  idconstraint = idformula)
              )


          } else {
            i <- which(tmpnetwork$constraints$Id == isolate(input$constraintselect))
            tmpnetwork$constraints[i, "Constraint"] <<- formula
            tmpnetwork$constraints[i, "idconstraint"] <<- idformula
          }
        })
      })

      observe({
        ntab <- tab$panel
        if (currenttab == "Add/Edit Constraints" & ntab != "Add/Edit Constraints"){
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
