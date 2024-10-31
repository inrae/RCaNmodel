#' constrEditorUI
#' Editor of constrEditorUI
#' @param id the id of ui
#' @importFrom shinyWidgets radioGroupButtons pickerInput
#' @importFrom shiny NS actionButton fluidRow column verbatimTextOutput
#' @export
#'

constrEditorUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1("CONSTRAINT EDITOR"),
          fluidRow(column(width = 4,
                          pickerInput(ns("constraintselect"),
                                      "Constraint",
                                      choices = "New")),
                   column(width=4,
                          shinyjs::disabled(textInput(ns("newname"),
                                                      "name of the new constraint")))),

          radioGroupButtons(ns("numbers"),
                            '',
                            c(0:9, "."),
                            size = "xs",
                            selected = character(0)),
          radioGroupButtons(ns("brackets"),
                            '',
                            c("(", ")", "[",']', ":", ","),
                            size = "xs",
                            selected = character(0)),

          radioGroupButtons(ns("comparisons"),
                            'comparisons',
                            c(">=", "<=", "+", "-", "/", "*"),
                            size = "xs",
                            selected = character(0)),
          fluidRow(
            column(width = 3,
                   radioGroupButtons(ns("components"),
                                     'components',
                                     "",
                                     size = "xs",
                                     selected = character(0))),
            column(width = 3,
                   radioGroupButtons(ns("fluxes"),
                                     'fluxes',
                                     "",
                                     size = "xs",
                                     selected = character(0))),
            column(width = 3,
                   radioGroupButtons(ns("obs"),
                                     'observations',
                                     "",
                                     size = "xs",
                                     selected = character(0))),
            column(width = 3,
                   radioGroupButtons(ns("aliases"),
                                     'aliases',
                                     "",
                                     size = "xs",
                                     selected = character(0))),
            column(width = 3,
                   radioGroupButtons(ns("years"),
                                     'years',
                                     "",
                                     size = "xs",
                                     selected = character(0)))),

          fluidRow(
            column(width = 3,
                   radioGroupButtons(ns("functions"),
                                     'functions',
                                     c("mean(", "sum("),
                                     size = "xs",
                                     selected = character(0)),

            ),
            column(width = 3,
                   radioGroupButtons(ns("modifiers"),
                                     'modifiers',
                                     c("Inflows",
                                       "Outflows",
                                       "Before",
                                       "After",
                                       "Ratio",
                                       "Delta",
                                       "BeforeDelta",
                                       "BeforeRatio"),
                                     size = "xs",
                                     selected = character(0)))),

          actionButton(ns("remove"), "remove element"),
          radioGroupButtons(
            inputId = ns("equations"),
            label = "Formula",
            choices = "",
            selected = character(0)
          ),
          verbatimTextOutput(ns("message")),
          actionButton(ns("ok"), "OK"))





}
