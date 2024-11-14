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
#' @importFrom stats setNames
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
        newnetwork$metaobs <<- createEmptyMetaObs()
        newnetwork$aliases <<- createEmptyAliases()
      }
      
      observe({
        network$components
        network$fluxes
        network$dictionary
        network$model
        network$aliases
        network$metaobs
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
        orig <<- paste0(tempfile(), ".xlsx")
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
                            "X",
                            "Y",
                            "x",
                            "y"))) %>%
            mutate("Inside" = as.integer(.data[["Inside"]]),
                   "id" = .data[["Component"]])
          
          
          if (!"x" %in% names(load_comp)){
            if ("X" %in% names(load_comp)){
              load_comp$x <- round(-400 + 800 * (load_comp$X- min(load_comp$X)) /
                                     (max(load_comp$X) - min(load_comp$Y)))
            } else {
              load_comp$x <- as.numeric(NA)
            }
          }
          if (!"y" %in% names(load_comp)){
            if ("Y" %in% names(load_comp)){
              load_comp$y <- round(- 400 + 800 * (load_comp$Y- min(load_comp$Y)) /
                                     (max(load_comp$Y) - min(load_comp$Y)))
            } else {
              load_comp$y <- as.numeric(NA)
            }
          }
          load_comp <- load_comp %>%
            dplyr::select(!dplyr::any_of(c("X", "Y")))
          
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
          
          load_aliases <- createEmptyAliases()
          load_metaobs <- createEmptyMetaObs()
          
          
          newnetwork$dictionary <-
            generateDictionary(load_comp,
                               load_flux,
                               load_obs,
                               load_metaobs,
                               load_aliases)
          
          if ("Aliases" %in% readxl::excel_sheets(input$loadname$datapath)){
            load_aliases <- readxl::read_excel(input$loadname$datapath,
                                               sheet = "Aliases") %>%
              mutate(id = .data[["Alias"]],
                     idconstraint = sapply(Formula,
                                           convertConstr2idConstr,
                                           newnetwork$dictionary))
            if (!"Comment" %in% names(load_aliases)){
              load_aliases$Comment <- character(nrow(load_aliases))
            }
          }
          
          
          if ("MetaObs" %in% readxl::excel_sheets(input$loadname$datapath)){
            load_metaobs <- readxl::read_excel(input$loadname$datapath,
                                               sheet = "Observation MetaInfo") %>%
              mutate(id = .data[["Observation"]])
            if (!"Comment" %in% names(load_metaobs)){
              load_metaobs$Comment <- character(nrow(load_metaobs))
            }
          } else {
            load_metaobs <- data.frame(
              id = setdiff(names(load_obs), "Year"),
              Observation = setdiff(names(load_obs), "Year"),
              Comment = character(ncol(load_obs) - 1))
          }
          
          
          
          newnetwork$dictionary <-
            generateDictionary(load_comp,
                               load_flux,
                               load_obs,
                               load_metaobs,
                               load_aliases)
          newnetwork$aliases <- load_aliases
          newnetwork$metaobs <- load_metaobs
          
          
          load_constr <- readxl::read_excel(input$loadname$datapath,
                                            sheet = "Constraints")
          load_constr$idconstraint = sapply(load_constr$Constraint,
                                            convertConstr2idConstr,
                                            newnetwork$dictionary)
          
          newnetwork$constraints <- load_constr
          
          
          newnetwork$model <- newname
          newnetwork$envir <- generateSymbolicEnvir(newnetwork)
          
          if (nrow(newnetwork$constraints) > 0)
            newnetwork$constraints$valid <- sapply(
              load_constr$Constraint,
              function(constr)
                checkValidity(constr, newnetwork) == "TRUE")
          
          if (nrow(newnetwork$aliases) > 0)
            newnetwork$aliases$valid <- sapply(
              load_aliases$Formula,
              function(constr)
                checkValidity(constr, newnetwork, onesided = TRUE) == "TRUE")
          
          
        })
        
      })
      
      output$savename <- shiny::downloadHandler(
        filename = function() paste0(network$model, ".xlsx"),
        content = function(file){
          if (orig == ""){
            orig <<- paste0(tempfile(), ".xlsx")
            modelfile <- openxlsx2::wb_workbook()
          } else {
            modelfile <- openxlsx2::wb_load(orig)
          }
          sheets <- openxlsx2::wb_get_sheet_names(modelfile)
          if (!"Components & input parameter" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Components & input parameter")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Components & input parameter",
                                              x = newnetwork$components %>%
                                                dplyr::select(!any_of("id")),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Fluxes" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Fluxes")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Fluxes",
                                              x = newnetwork$fluxes %>%
                                                dplyr::select(!any_of(c("id",
                                                                        "from",
                                                                        "to"))),
                                              colNames = TRUE,
                                              na.strings = "")
          obs <- data.frame(Year = integer())
          if (!is.null(newnetwork$observations)){
            obs <- newnetwork$observations
          }
          renamed <- newnetwork$metaobs$id %>%
            setNames(newnetwork$metaobs$Observation)
          
          if (!"Input time-series" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Input time-series")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Input time-series",
                                              x = newnetwork$observations %>%
                                                dplyr::rename(dplyr::all_of(renamed)),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Constraints" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Constraints")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Constraints",
                                              x = newnetwork$constraints %>%
                                                dplyr::select(!dplyr::any_of(c("idconstraint",
                                                                               "valid"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Aliases" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Aliases")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Aliases",
                                              x = newnetwork$aliases %>%
                                                dplyr::select(!dplyr::any_of(c("id",
                                                                               "valid"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          
          if (!"Observation MetaInfo" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Observation MetaInfo")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Observation MetaInfo",
                                              x = newnetwork$metaobs %>%
                                                dplyr::select(!dplyr::any_of(c("id"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          openxlsx2::wb_save(modelfile, file, overwrite = TRUE)
        }
      )
      
      
      
      
      
      
      
      return(newnetwork)
      
    }
  )
}
