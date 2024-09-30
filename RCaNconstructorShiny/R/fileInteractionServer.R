#' fileInteractionServer
#'
#' @param id the id of the ui
#' @param network the network
#' @return a updated network
#' @importFrom magrittr %>%
#' @importFrom shiny isolate observe modalDialog observeEvent tagList
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


      newnetwork <- createEmptyNetwork()

      cleanNewtork <- function(){
        newnetwork$model <<- NULL
        newnetwork$components <<- createEmptyComponents()
        newnetwork$fluxes <<- createEmptyFluxes()
        newnetwork$dictionary <<- character()
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
          modelname <- stringr::str_split(input$loadname$name,"\\.")
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

          if (!"x" %in% names(land_comp))
            land_comp$x <- as.numeric(NA)
          if (!"y" %in% names(land_comp))
            land_comp$y <- as.numeric(NA)

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
          newnetwork$components <- load_comp
          newnetwork$fluxes <- load_flux
          newnetwork$dictionary <- c(load_flux$Flux, load_comp$Component)
          names(newnetwork$dictionary) <- c(load_flux$Flux, load_comp$Component)
          newnetwork$model <- newname


        })

      })







      return(newnetwork)

    }
  )
}
