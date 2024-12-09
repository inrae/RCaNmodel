#' fileInteractionServer
#'
#' @param id the id of the ui
#' @param network the network
#' @param timeline the timeline
#' @return a updated network
#' @importFrom magrittr %>%
#' @importFrom shiny isolate observe modalDialog observeEvent tagList
#' @importFrom shiny downloadButton updateTextAreaInput
#' @importFrom shiny modalButton textInput fileInput showModal removeModal
#' @importFrom spsComps shinyCatch
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom dplyr mutate any_of select pull
#' @export

fileInteractionServer <- function(id, network, timeline){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      param <- read.csv(system.file("info.csv", package = "RCaNconstructor"),
                        sep = "\t",
                        header=FALSE, 
                        na.strings = "")
      
      
      resetInfo <- function(){
        for (i in seq_len(nrow(param))){
          if (!is.na(param[i,2])){
            updateTextAreaInput(session, 
                                param[i, 2],
                                value = "")
            shinyBS::removePopover(session,
                                   param[i, 2])
            shinyBS::addPopover(session,
                                param[i, 2],
                                param[i, 3])
          }
        }
      }
      
      
      resetInfo()
      
      
      orig <- ""
      
      
      filenewnetwork <- createEmptyNetwork()
      filenewnetwork$timeline <- createEmptyTimeLine()
      
      cleanNewtork <- function(){
        filenewnetwork$model <<- NULL
        filenewnetwork$components <<- createEmptyComponents()
        filenewnetwork$fluxes <<- createEmptyFluxes()
        filenewnetwork$dictionary <<- character()
        filenewnetwork$observations <<- tibble(Year = numeric())
        filenewnetwork$metaobs <<- createEmptyMetaObs()
        filenewnetwork$aliases <<- createEmptyAliases()
        filenewnetwork$constraints <<- createEmptyConstraints()
        filenewnetwork$timeline <<- createEmptyTimeLine()
      }
      
      observe({
        #this observes any change in network
        for (v in names(network)){
          network[[v]]
        }
        for (v in names(isolate(network)))
          filenewnetwork[[v]] <<- isolate(network[[v]])
      })
      
      observe({
        timeline$timeline
        isolate(filenewnetwork$timeline <<- timeline$timeline)
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
        resetInfo()
        updateTextAreaInput(session,
                            "modelname",
                            value = isolate(input$newname))
        shinyBS::removePopover(session,"modelname")
        shinyBS::addPopover(session,"modelname", 
                            param[which(param[, 2] == "modelname" & !is.na(param[, 2])),
                                  3])
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
        orig <<- isolate(input$loadname$datapath)
        modelname <- stringr::str_split(isolate(input$loadname$name),"\\.")[[1]]
        removeModal()
        shinyCatch({
          filenewnetwork <- loadRCaNfile(orig, modelname)
          if ("INFO" %in% readxl::excel_sheets(orig)){
            info <- tibble(
              readxl::read_excel(orig, 
                                 col_names = FALSE)
            )
            for (i in seq_len(nrow(param))){
              if (!is.na(param[i,2])){
                updateTextAreaInput(session, 
                                    param[i, 2],
                                    value = info[i, 2] %>% pull())
                shinyBS::removePopover(session,
                                       param[i, 2])
                shinyBS::addPopover(session,
                                    param[i, 2],
                                    param[i, 3])
              }
            }
          } else {
            resetInfo()
          }
          
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
          modelfile <- writeRCaNfile(modelfile, filenewnetwork)
          
          openxlsx2::wb_save(modelfile, file, overwrite = TRUE)
        }
      )
      
      
      
      
      
      
      return({
        filenewnetwork
      })
      
    }
  )
}
