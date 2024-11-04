#' tableObsServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#' @return a data.frame observation
#' @importFrom magrittr %>%
#' @importFrom dplyr anti_join bind_rows mutate select across any_of arrange
#' @importFrom rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe req validate need selectInput modalButton
#' @importFrom datamods import_modal import_server
#' @importFrom rlang .data
#' @export

tableObsServer <- function(id, network, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""

      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()

      observe({
        network$components
        network$observations
        network$fluxes
        network$dictionary
        network$model
        req(isolate(tab$panel) == currenttab)
        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }
        output$tableedit <- rendertab(tmpnetwork$observations)

      })

      rendertab <- function(data){
        req(nrow(data) > 0)
        tab <-
          rhandsontable(data,
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(manualColumnResize = TRUE)
        for (col in names(data))
           tab <- tab %>%
           hot_col(col, "numeric")
         tab <- tab %>%
           hot_col("Year", readOnly = TRUE, format = "0000")
        renderRHandsontable({tab})
      }

      shiny::observeEvent(input$add,{
        import_modal(
          id = session$ns("importid"),
          from = c("file", "copypaste", "googlesheets", "url"),
          title = "Import data to be used in application"
        )
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
        newdata <- hot_to_r(input$tableedit)
        common <- intersect(setdiff(names(adddata), "Year"),
                            c(isolate(tmpnetwork$components$Component),
                              isolate(tmpnetwork$fluxes$Flux),
                              isolate(names(newdata))))

        validate(need(length(common) == 0, paste('name',
                                                 common,
                                                 "already used")))
        if (!is.null(newdata)){
          newdata <- newdata %>%
            dplyr::full_join(adddata) %>%
            dplyr::arrange(.data[["Year"]])
        } else {
          newdata <- adddata
        }
        output$tableedit <- rendertab(newdata)
      })





      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(input$tableedit)
        req(newdata)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          mutate(across(any_of(c("Year")), as.integer))
        shinyCatch({
          if (max(table(c(names(newdata),
                          tmpnetwork$fluxes$Flux,
                          tmpnetwork$components$Components))) > 1)
            stop("a name is already used")
        })
      })

      shiny::observeEvent(input$remove,{
        newdata <- hot_to_r(input$tableedit)
        validate(need(!is.null(newdata), "no data"))
        validate(need(nrow(newdata) > 0, "no data"))
        validate(need(ncol(newdata) > 1, "no series"))
        series <- setdiff(names(newdata), "Year")
        showModal(
          modalDialog(tile = "Remove series",
                      selectInput(session$ns("seriesremoved"),
                                  "select file",
                                  series,
                                  multiple = TRUE),
                      footer = tagList(
                        actionButton(session$ns("okremoved"), "OK"),
                        modalButton(("Cancel")),
                      ),
                      easyClose = FALSE))

      })


      observeEvent(input$okremoved,{
        removeModal()
        deleted <- isolate(input$seriesremoved)
        req(input$seriesremoved)
        validate(need(length(deleted) > 0, "nothing to remove"))
        newdata <- hot_to_r(input$tableedit)
        req(newdata)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          dplyr::select(!dplyr::all_of(deleted))
        output$tableedit <- rendertab(newdata)

      })


      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(tmpnetwork$observations)
      })


      observe({
        ntab <- tab$panel
        if (currenttab == "Time Series" & ntab != "Time Series"){
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
