#' fileInteractionServer
#'
#' @param id the id of the ui
#' @param network the network
#' @return a updated network
#' @importFrom magrittr %>%
#' @importFrom shiny isolate observe modalDialog observeEvent tagList
#' @importFrom shiny downloadButton
#' @importFrom shiny modalButton textInput fileInput showModal removeModal
#' @importFrom spsComps shinyCatch
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate any_of select
#' @export

fileInteractionServer <- function(id, network){
  shiny::moduleServer(
    id,
    function(input, output, session) {


      orig <- ""


      newnetwork <- createEmptyNetwork()

      cleanNewtork <- function(){
        newnetwork$model <<- NULL
        newnetwork$components <<- createEmptyComponents()
        newnetwork$fluxes <<- createEmptyFluxes()
        newnetwork$dictionary <<- character()
        newnetwork$observations <<- data.frame()
      }

      observe({
        network$components
        network$fluxes
        network$dictionary
        network$model
        for (v in names(isolate(network)))
          newnetwork[[v]] <<- isolate(network[[v]])
      })

      observeEvent(input$new, {
        showModal(
          modalDialog(tile = "CAUTION",
                      "changes to current model will be lost",
                      textInput(session$ns("newname"), "name of the model", ""),
                      footer = tagList(
                        modalButton(("Cancel")),
                        actionButton(session$ns("ok"), "OK")
                      ),
                      easyClose = FALSE))
      })
      observeEvent(input$ok,{
        orig <<- tempfile()
        removeModal()
        cleanNewtork()
        newnetwork$model <- isolate(input$newname)
      })


      observeEvent(input$open,{
        showModal(
          modalDialog(tile = "CAUTION",
                      "changes to current model will be lost",
                      fileInput(session$ns("loadname"), "select file"),
                      footer = tagList(
                        modalButton(("Cancel")),
                        actionButton(session$ns("okload"), "OK")
                      ),
                      easyClose = FALSE))
      })


      observeEvent(input$okload,{
        removeModal()
        shinyCatch({
          orig <<- isolate(input$loadname$datapath)
          modelname <- stringr::str_split(input$loadname$name,"\\.")[[1]]
          newname <- paste(modelname[seq_len(length(modelname) - 1)],
                           collapse = '.')
          load_comp <- readxl::read_excel(input$loadname$datapath,
                                          sheet = "Components & input parameter") %>%
            select(any_of(c("Component",
                            "Inside",
                            "AssimilationE",
                            "Digestibility",
                            "OtherLosses",
                            "Inertia",
                            "Satiation",
                            "RefugeBiomass",
                            "x",
                            "y"))) %>%
            mutate("Inside" = as.integer(.data[["Inside"]]),
                   "id" = .data[["Component"]])

          if (!"x" %in% names(load_comp))
            load_comp$x <- as.numeric(NA)
          if (!"y" %in% names(load_comp))
            load_comp$y <- as.numeric(NA)

          load_flux <- readxl::read_excel(input$loadname$datapath,
                                          sheet = "Fluxes") %>%
            select(any_of(c("Flux",
                            "From",
                            "To",
                            "Trophic"))) %>%
            mutate("Trophic" = as.integer(.data[["Trophic"]]),
                   "id" = .data[["Flux"]],
                   "from" = load_comp$Component[match(.data[["From"]],
                                                      load_comp$Component)],
                   "to" = load_comp$Component[match(.data[["To"]],
                                                    load_comp$Component)])


          load_obs <- readxl::read_excel(input$loadname$datapath,
                                         sheet = "Input time-series")

          series <- character()
          if (nrow(load_obs) > 0){
            series <- setdiff(names(load_obs), "Year")
          }
          newnetwork$components <- load_comp
          newnetwork$observations <- load_obs
          newnetwork$fluxes <- load_flux
          newnetwork$dictionary <- c(load_flux$Flux,
                                     load_comp$Component,
                                     series)
          names(newnetwork$dictionary) <- c(load_flux$Flux,
                                            load_comp$Component,
                                            series)

          newnetwork$model <- newname


        })

      })

      observeEvent(input$save,{
        showModal(
          modalDialog(tile = "SAVE FILE",
                      downloadButton(session$ns("savename"), "select file"),
                      footer = tagList(
                        modalButton(("Cancel")),
                      ),
                      easyClose = FALSE))
      })

      observeEvent(input$savename,
                   removeModal())

      output$savename <- shiny::downloadHandler(
        filename = function() paste0(network$model, ".xlxs"),
        content = function(file){
          modelfile <- openxlsx::loadWorkbook(orig)
          openxlsx::writeData(modelfile,
                              sheet = "Components & input parameter",
                              x = newnetwork$components %>%
                                dplyr::select(!any_of("id")),
                              colNames = TRUE)

          openxlsx::writeData(modelfile,
                              sheet = "Fluxes",
                              x = newnetwork$fluxes %>%
                                dplyr::select(!any_of(c("id", "from", "to"))),
                              colNames = TRUE)
          obs <- data.frame(Year = integer())
          if (!is.null(newnetwork$observations)){
            obs <- newnetwork$observations
          }
          openxlsx::writeData(modelfile,
                              sheet = "Input time-series",
                              x = newnetwork$observations,
                              colNames = TRUE)
          openxlsx::saveWorkbook(modelfile, file)
        }
      )







      return(newnetwork)

    }
  )
}
