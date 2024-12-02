#' fileInteractionServer
#'
#' @param id the id of the ui
#' @param network the network
#' @return a updated network
#' @importFrom magrittr %>%
#' @importFrom shiny isolate observe modalDialog observeEvent tagList
#' @importFrom shiny downloadButton updateTextAreaInput
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
      
      param <- read.csv(system.file("info.csv", package = "RCaNconstructor"),
                        sep = "\t",
                        header=FALSE, 
                        na.strings = "")
      
      
      resetInfo <- function(){
        for (i in nrow(param)){
          if (!is.na(param[i,2]))
            updateTextAreaInput(session, 
                                param[i, 2],
                                value = "")
        }
      }
      
      
      
      
      orig <- ""
      
      
      filenewnetwork <- createEmptyNetwork()
      
      cleanNewtork <- function(){
        filenewnetwork$model <<- NULL
        filenewnetwork$components <<- createEmptyComponents()
        filenewnetwork$fluxes <<- createEmptyFluxes()
        filenewnetwork$dictionary <<- character()
        filenewnetwork$observations <<- data.frame()
        filenewnetwork$metaobs <<- createEmptyMetaObs()
        filenewnetwork$aliases <<- createEmptyAliases()
      }
      
      observe({
        network$components
        network$fluxes
        network$dictionary
        network$model
        network$aliases
        network$metaobs
        for (v in names(isolate(network)))
          filenewnetwork[[v]] <<- isolate(network[[v]])
      })
      
      observeEvent(input$new, {
        resetInfo()
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
        filenewnetwork$model <- isolate(input$newname)
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
          modelname <- stringr::str_split(isolate(input$loadname$name),"\\.")[[1]]
          newname <- paste(modelname[seq_len(length(modelname) - 1)],
                           collapse = '.')
          
          
          
          load_comp <- readxl::read_excel(orig,
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
          
          load_flux <- readxl::read_excel(orig,
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
          
          
          load_obs <- readxl::read_excel(orig,
                                         sheet = "Input time-series")
          
          series <- character()
          if (nrow(load_obs) > 0){
            series <- setdiff(names(load_obs), "Year")
          }
          filenewnetwork$components <- load_comp
          filenewnetwork$observations <- load_obs
          filenewnetwork$fluxes <- load_flux
          
          load_aliases <- createEmptyAliases()
          load_metaobs <- createEmptyMetaObs()
          
          
          filenewnetwork$dictionary <-
            generateDictionary(load_comp,
                               load_flux,
                               load_obs,
                               load_metaobs,
                               load_aliases)
          
          if ("Aliases" %in% readxl::excel_sheets(orig)){
            load_aliases <- readxl::read_excel(orig,
                                               sheet = "Aliases") %>%
              mutate(id = .data[["Alias"]],
                     idconstraint = sapply(.data[["Formula"]],
                                           convertConstr2idConstr,
                                           filenewnetwork$dictionary))
            if (!"Comment" %in% names(load_aliases)){
              load_aliases$Comment <- character(nrow(load_aliases))
            }
          }
          
          
          if ("MetaObs" %in% readxl::excel_sheets(orig)){
            load_metaobs <- readxl::read_excel(orig,
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
          
          
          
          filenewnetwork$dictionary <-
            generateDictionary(load_comp,
                               load_flux,
                               load_obs,
                               load_metaobs,
                               load_aliases)
          filenewnetwork$aliases <- load_aliases
          filenewnetwork$metaobs <- load_metaobs
          
          
          load_constr <- readxl::read_excel(orig,
                                            sheet = "Constraints")
          load_constr$idconstraint = sapply(load_constr$Constraint,
                                            convertConstr2idConstr,
                                            filenewnetwork$dictionary)
          
          filenewnetwork$constraints <- load_constr
          
          
          filenewnetwork$model <- newname
          filenewnetwork$envir <- generateSymbolicEnvir(filenewnetwork)
          
          if (nrow(filenewnetwork$constraints) > 0){
            filenewnetwork$constraints$valid <- TRUE
            filenewnetwork$constraints$validity_comments <- character(nrow(filenewnetwork$constraints))
          }
          
          if (nrow(filenewnetwork$aliases) > 0){
            filenewnetwork$aliases$valid <- TRUE
            filenewnetwork$aliases$validity_comments <- character(nrow(filenewnetwork$aliases))
          }
          
          
        })
        
        if ("INFO" %in% readxl::excel_sheets(orig)){
          info <- as.data.frame(
            readxl::read_excel(orig, 
                               col_names = FALSE)
          )
          for (i in seq_len(nrow(param))){
            if (!is.na(param[i,2]))
              updateTextAreaInput(session, 
                                  param[i, 2],
                                  value = info[i, 2])
          }
        } else {
          resetInfo()
        }
        
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
                                              x = filenewnetwork$components %>%
                                                dplyr::select(!any_of("id")),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Fluxes" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Fluxes")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Fluxes",
                                              x = filenewnetwork$fluxes %>%
                                                dplyr::select(!any_of(c("id",
                                                                        "from",
                                                                        "to"))),
                                              colNames = TRUE,
                                              na.strings = "")
          obs <- data.frame(Year = integer())
          if (!is.null(filenewnetwork$observations)){
            obs <- filenewnetwork$observations
          }
          renamed <- filenewnetwork$metaobs$id %>%
            setNames(filenewnetwork$metaobs$Observation)
          
          if (!"Input time-series" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Input time-series")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Input time-series",
                                              x = filenewnetwork$observations %>%
                                                dplyr::rename(dplyr::all_of(renamed)),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Constraints" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Constraints")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Constraints",
                                              x = filenewnetwork$constraints %>%
                                                dplyr::select(!dplyr::any_of(c("idconstraint",
                                                                               "valid",
                                                                               "validity_comments"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"Aliases" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Aliases")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Aliases",
                                              x = filenewnetwork$aliases %>%
                                                dplyr::select(!dplyr::any_of(c("id",
                                                                               "valid",
                                                                               "validity_comments"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          if (!"INFO" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "INFO")
          for (i in seq_len(nrow(param))){
            modelfile <- openxlsx2::wb_add_data(modelfile,
                                                sheet = "INFO",
                                                param[i, 1],
                                                start_col = 1,
                                                start_row = i)
            if (!is.na(param[i, 2])){
              modelfile <- openxlsx2::wb_add_data(modelfile,
                                                  sheet = "INFO",
                                                  input[[param[i, 2]]],
                                                  start_col = 2,
                                                  start_row = i)
            }
            if (!is.na(param[i, 3])){
              modelfile <- openxlsx2::wb_add_data(modelfile,
                                                  sheet = "INFO",
                                                  param[i, 3],
                                                  start_col = 3,
                                                  start_row = i)
            }
          }
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "INFO",
                                              orig,
                                              start_col = 2,
                                              start_row = 1)
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "INFO",
                                              "Value",
                                              start_col = 2,
                                              start_row = 2)
          
          if (!"Observation MetaInfo" %in% sheets)
            modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                                     "Observation MetaInfo")
          modelfile <- openxlsx2::wb_add_data(modelfile,
                                              sheet = "Observation MetaInfo",
                                              x = filenewnetwork$metaobs %>%
                                                dplyr::select(!dplyr::any_of(c("id"))),
                                              colNames = TRUE,
                                              na.strings = "")
          
          openxlsx2::wb_save(modelfile, file, overwrite = TRUE)
        }
      )
      
      
      
      
      
      
      return(filenewnetwork)
      
    }
  )
}
