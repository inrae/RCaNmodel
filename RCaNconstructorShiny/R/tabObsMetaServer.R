#' tabObsMetaServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom datamods import_modal import_server
#' @importFrom dplyr mutate select across any_of
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r hot_context_menu
#' @importFrom shiny isolate observe showNotification
#' @importFrom shiny modalButton textInput fileInput showModal removeModal
#' @export

tabObsMetaServer <- function(id, network, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""
      
      hiddencols <- c("id")
      
      tabMOnewnetwork <- createEmptyNetwork()
      tmpnetwork <- list()
      tmpnetwork2 <- list()
      
      
      
      
      
      
      
      
      observe({
        #this observes any change in network
        for (v in names(network)){
          network[[v]]
        }
        req(isolate(tab$panel) == currenttab)
        
        
        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]])){
            tmpnetwork[[v]] <<- isolate(network[[v]])
            tmpnetwork2[[v]] <<- isolate(network[[v]])
          }
        }
        output$tableedit <- rendertab(tmpnetwork$metaobs)
        
      })
      
      
      rendertab <- function(data){
        tab <-
          rhandsontable(data,
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(colWidths = ifelse(names(data) %in% hiddencols,
                                      1,
                                      200),
                   manualColumnResize = TRUE,
                   columnSorting = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        renderRHandsontable({tab})
        
      }
      
      
      shiny::observeEvent(input$remove,{
        newdata <- isolate(hot_to_r(input$tableedit))
        req(nrow(newdata) > 0)
        showModal(
          modalDialog(tile = "REMOVE",
                      "remove series",
                      selectInput(session$ns("idremove"),
                                  "series to remove",
                                  newdata$Observation,
                                  multiple = TRUE),
                      footer = tagList(
                        modalButton(("Cancel")),
                        actionButton(session$ns("okremove"), "REMOVE")
                      ),
                      easyClose = FALSE))
      })
      
      observeEvent(input$okremove,{
        removeModal()
        removed <- isolate(input$idremove)
        req(length(removed) > 0)
        idremoved <- names(tmpnetwork2$dictionary)[which(tmpnetwork2$dictionary == removed)]
        tmpnetwork2$metaobs <<- tmpnetwork2$metaobs %>%
          dplyr::filter(!.data[["Observation"]] %in% removed)
        tmpnetwork2$observations <<- tmpnetwork2$observations %>%
          dplyr::select(!all_of(idremoved))
        output$tableedit <- rendertab(tmpnetwork2$metaobs)
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })
      
      
      imported <- import_server("importid", return_class = "tbl_df")
      
      observe({
        adddata <- imported$data()
        validate(need("Year" %in% names(adddata),
                      "Year should be a column name"))
        validate(need(is.numeric(as.matrix(adddata)),
                      "Data should be numeric"))
        
        
        req(nrow(adddata) > 0)
        req(ncol(adddata) > 1)
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
        newdata <- hot_to_r(input$tableedit) %>%
          mutate(across(everything(), as.character))
        common <- intersect(setdiff(names(adddata), "Year"),
                            c(isolate(tmpnetwork2$components$Component),
                              isolate(tmpnetwork2$fluxes$Flux),
                              isolate(newdata$Observation)))
        
        validate(need(length(common) == 0, paste('name',
                                                 common,
                                                 "already used")))
        if (!is.null(newdata)){
          newdata <- newdata %>%
            dplyr::bind_rows(tibble(id = setdiff(names(adddata), "Year"),
                                    Observation = setdiff(names(adddata),
                                                          "Year"),
                                    Comment = character(ncol(adddata) - 1)))
          tmpnetwork2$metaobs <<- newdata
          if (nrow(tmpnetwork$observation) > 0){
            tmpnetwork2$observations <<- tmpnetwork$observations %>%
              dplyr::full_join(adddata) %>%
              dplyr::arrange(.data[["Year"]])
          } else {
            tmpnetwork2$observations <<- adddata %>%
              dplyr::arrange(.data[["Year"]])
          }
        } else {
          newdata <-
            tibble(id = setdiff(names(adddata), "Year"),
                   Observation = setdiff(names(adddata),
                                         "Year"),
                   Comment = character(ncol(adddata) - 1))
          tmpnetwork2$observations <<- adddata
        }
        tmpnetwork2$metaobs <<- newdata
        output$tableedit <- rendertab(newdata)
      })
      
      
      
      
      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(isolate(input$tableedit))
        req(newdata)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          mutate(across(any_of(c("Year")), as.integer))
        shinyCatch({
          if (max(table(c(newdata$Observation,
                          tmpnetwork2$fluxes$Flux,
                          tmpnetwork2$components$Component))) > 1)
            stop("a name is already used")
          for (v in names(isolate(network))){
            if(!identical(isolate(tmpnetwork2[[v]]),
                          tmpnetwork[[v]])){
              tmpnetwork[[v]] <<- tmpnetwork2[[v]]
            }
          }
          shinyjs::disable("ok")
          shinyjs::disable("cancel")
          
        })
      })
      
      shiny::observeEvent(input$add,{
        import_modal(
          id = session$ns("importid"),
          from = c("file", "copypaste", "googlesheets", "url"),
          title = "Import data to be used in application"
        )
      })
      
      shiny::observe({
        input$tableedit$changes$changes
        req(!is.null(input$tableedit$changes$changes))
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })
      
      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(network$metaobs)
        tmpnetwork2$metaobs <<- tmpnetwork$metaobs
        tmpnetwork2$observations <<- tmpnetwork$observations
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
      })
      
      
      observe({
        ntab <- tab$panel
        if (currenttab == "Observation MetaInfo" & ntab != "Observation MetaInfo") {
          for (v in names(tmpnetwork2)){
            if (!identical(tmpnetwork2[[v]],
                           isolate(tabMOnewnetwork[[v]])))
              tabMOnewnetwork[[v]] <<- tmpnetwork2[[v]]
          }
        }
        currenttab <<- ntab
      })
      
      return(tabMOnewnetwork)
      
    }
  )
}
