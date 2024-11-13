#' constrEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param slot either components or aliases
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom spsComps shinyCatch
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of pull
#' @importFrom shiny isolate observe updateTextInput updateSelectInput
#' @importFrom shinyWidgets updateRadioGroupButtons updatePickerInput
#' @export

constrEditorServer <- function(id, network, slot, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""
      
      
      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()
      constraint <- character(0)
      
      formcol <- ifelse(slot == "constraints",
                        "Constraint",
                        "Formula")
      
      
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
        
        req(tmpnetwork[[slot]])
        
        
        if (nrow(tmpnetwork[[slot]]) > 0)
          updatePickerInput(session,
                            "constraintselect",
                            choices = c("New",
                                        sort(tmpnetwork[[slot]][,
                                                                ifelse(slot == "constraints",
                                                                       "Id",
                                                                       "Alias")] %>%
                                               pull()
                                        )
                            ),
                            selected = "New")
        
        
        
      }
      
      
      
      observeEvent(input$constraintselect, {
        req(input$constraintselect)
        if (input$constraintselect == "New"){
          resetPage()
        } else {
          shinyjs::disable("newname")
          selected <- which(tmpnetwork[[slot]][,ifelse(slot == "constraints",
                                                       "Id",
                                                       "Alias")] %>%
                              pull() == input$constraintselect)
          idconstraint <- tmpnetwork[[slot]]$idconstraint[selected]
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
        network$metaobs
        
        req(isolate(tab$panel) == currenttab)
        
        
        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }
        
        
        if (nrow(tmpnetwork$aliases) > 0)
          updateSelectInput(session,
                            "aliases",
                            choices = c("",
                                        sort(tmpnetwork$aliases$Alias)),
                            selected = "")
        
        if (nrow(tmpnetwork$observations) > 0)
          updateSelectInput(session,
                            "obs",
                            choices = c("", 
                                        sort(tmpnetwork$metaobs$Observation)),
                            selected = "")
        
        
        
        if (nrow(tmpnetwork$observations) > 0)
          updateSelectInput(session,
                            "years",
                            choices = c("",
                                        sort(tmpnetwork$observations$Year)),
                            selected = "")
        
        if (nrow(tmpnetwork$components) > 0)
          updateSelectInput(session,
                            "components",
                            selected = "",
                            choices = c("",
                                        sort(tmpnetwork$components$Component[tmpnetwork$components$Inside == 1])))
        
        if (nrow(tmpnetwork$fluxes) > 0)
          updateSelectInput(session,
                            "fluxes",
                            selected = "",
                            choices = c("",sort(c(tmpnetwork$fluxes$Flux,
                                                  "Allflows"))))
        
        
        resetPage()
        
      })
      
      
      
      
      observeEvent(input$years,{
        addVal(isolate(input$years))
        updateSelectInput(session,
                          "years",
                          selected = "")
      }
      )
      
      observeEvent(input$obs,{
        addVal(isolate(input$obs))
        updateSelectInput(session,
                          "obs",
                          selected = "")
      }
      )
      
      observeEvent(input$fluxes,{
        addVal(isolate(input$fluxes))
        updateSelectInput(session,
                          "fluxes",
                          selected = "")
      }
      )
      
      observeEvent(input$aliases,{
        addVal(isolate(input$aliases))
        updateSelectInput(session,
                          "aliases",
                          selected = "")
      }
      )
      
      observeEvent(input$components,{
        addVal(isolate(input$components))
        updateSelectInput(session,
                          "components",
                          selected = "")
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
          valid <- checkValidity(formula,
                                 tmpnetwork,
                                 onesided = (slot == "aliases"))
          if (valid != "TRUE")
            stop(paste(formula, valid, sep = ":"))
          
          idformula <- convertConstr2idConstr(formula,
                                              tmpnetwork$dictionary)
          if (isolate(input$constraintselect) == "New"){
            if (slot == "constraints"){
              if (isolate(input$newname) %in% tmpnetwork$constraint$Id)
                stop("Id already used")
            } else {
              if (isolate(input$newname) %in% names(tmpnetwork$dictionary))
                stop("Id already used")
            }
            if (slot == "constraints"){
              tmpnetwork$constraints <<- tmpnetwork$constraints %>%
                dplyr::bind_rows(
                  data.frame(
                    Id = isolate(input$newname),
                    Constraint = formula,
                    idconstraint = idformula,
                    valid = TRUE)
                )
            } else {
              tmpnetwork$aliases <<- tmpnetwork$aliases %>%
                dplyr::bind_rows(
                  data.frame(
                    Alias = isolate(input$newname),
                    Forula = formula,
                    idconstraint = idformula,
                    id = input$newname,
                    valid = TRUE)
                )
            }
            updateTextInput(session, "newname", value = "")
            updatePickerInput(session,
                              "constraintselect",
                              choices = c("New",
                                          sort(tmpnetwork[[slot]][,
                                                                  ifelse(slot == "constraints",
                                                                         "Id",
                                                                         "Alias")] %>%
                                                 pull()
                                          )
                              ),
                              selected = "New")
            updateRadioGroupButtons(session,
                                    "equations",
                                    choices = "",
                                    selected = character(0))            
            
          } else {
            i <- which(tmpnetwork[[slot]][,ifelse(slot == "constraints",
                                                  "Id",
                                                  "Alias")] %>%
                         pull == isolate(input$constraintselect))
            tmpnetwork[[slot]][i, formcol] <<- formula
            tmpnetwork[[slot]][i, "idconstraint"] <<- idformula
          }
        })
      })
      
      observe({
        ntab <- tab$panel
        if ((currenttab == "Add/Edit Constraints" & ntab != "Add/Edit Constraints" & slot == "constraints" ) |
            (currenttab == 'View Aliases' & ntab != 'View Aliases' & slot == "aliases")){
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
