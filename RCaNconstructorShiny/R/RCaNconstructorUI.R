#' RCaNconstructorUI
#' function that returns the ui of the the Constructor
#' @return an ui
#' @importFrom spsComps spsDepend
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom htmltools div
#' @importFrom shiny actionButton fluidPage tabsetPanel tabPanel
#' @importFrom shinyjs useShinyjs
#' @export

RCaNconstructorUI <- function(){
  fluidPage(spsDepend("toastr"),
            fluidPage(title="RCaNconstructor",


                      ################################################################################################
                      # BODY
                      ################################################################################################

                      useShinyjs(), # to be able to use shiny js
                      tabsetPanel(id = "mainpanel",
                        tabPanel("Model",
                                 fileInteractionUI("files")),



                        tabPanel("Edit TrophicNetwork",
                                 tabsetPanel(id = "editpanel",
                                   tabPanel("Visualise Trophic Network",
                                            h1("network"),
                                               visNetworkUI("visnetwork")),
                                   tabPanel("Components",
                                            h1("components"),
                                            tabEditorUI("components", "COMPONENTS")
                                   ),
                                   tabPanel("Fluxes", h1("fluxes"),
                                            tabEditorUI("fluxes", "FLUXES")
                                   )
                                 )),
                        tabPanel("Observations",
                                 tabsetPanel(id = "editobservations",
                                             tabPanel("Observation MetaInfo",
                                                      h1("Observation MetaInfo"),
                                                      tabObsMetaUI("tabmetaobs")),
                                             tabPanel("Time Series",
                                                      h1("time series"),
                                                      tableObsUI("obs", "TIME SERIES"))
                                             )),
                        tabPanel("Constraints",
                                 tabsetPanel(id = "editconstraints",
                                             tabPanel("View Constraints",
                                                      h1("Current Constraints"),
                                                      tabConstrUI("tabconstraints")),
                                             tabPanel("Add/Edit Constraints",
                                                      h1("Constraints Editor"),
                                                      constrEditorUI("constreditor")))),
                        tabPanel("Aliases",
                                 tabsetPanel(id = "editaliases",
                                             tabPanel("View Aliases",
                                                      h1("Current Aliases"),
                                                      tabConstrUI("tabaliases")),
                                             tabPanel("Add/Edit Aliases",
                                                      h1("Aliases Editor"),
                                                      constrEditorUI("editaliases"))))

                      )

            )
  )

}
