#' tableEditorServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param slot either fluxes or components
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select across any_of where
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export

tableEditorServer <- function(id, network, slot, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""
      
      hiddencols <- c("from", "to", "id", "x", "y")
      
      tabEnewnetwork <- createEmptyNetwork()
      tmpnetwork <- list()
      
      idcolumn <- ifelse(slot == "components",
                         "Component",
                         "Flux")
      
      
      wholedata <- tibble()
      
      if (isTRUE(getOption("shiny.testmode"))) {
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      }
      
      
      updateSelectInput(session,
                        "filter",
                        label = ifelse(slot == "components",
                                       "Component",
                                       "Flux"))
      observe({
        #this observes any change in network
        for (v in names(network)){
          network[[v]]
        }
        req(isolate(tab$panel) == currenttab)
        for (v in names(isolate(network))){
          if(!ident_tol(isolate(network[[v]]),
                        tmpnetwork[[v]])){
            tmpnetwork[[v]] <<- isolate(network[[v]])
          }
        }
        output$tableedit <- rendertab(tmpnetwork[[slot]],
                                      tmpnetwork$components$Component)
        if (!isTRUE(getOption("shiny.testmode"))) {
          shinyjs::disable("ok")
          shinyjs::disable("cancel")
        }
        if (slot == "components"){
          wholedata <<- tmpnetwork$components
        } else {
          wholedata <<- tmpnetwork$fluxes
        }
        if (slot == "components"){
          choices <- sort(tmpnetwork$components$Component)
        } else {
          choices <- sort(tmpnetwork$fluxes$Flux)
        }
        updateSelectInput(session,
                          "filter",
                          choices = c("All", 
                                      choices),
                          selected = "All")
      })
      
      observeEvent(input$filter, {
        output$tableedit <- rendertab(createSubData(),
                                      tmpnetwork$components$Component)
      })
      
      
      createSubData <- function(){
        fval <- isolate(input$filter)
        cdata <- hot_to_r(isolate(input$tableedit))
        req(cdata)
        if (length(hiddencols) > 0)
          cdata <- cdata %>%
          tibble::rownames_to_column("rowname") %>%
          dplyr::left_join(wholedata %>%
                             tibble::rownames_to_column("rowname") %>%
                             dplyr::select(any_of(c("rowname", hiddencols))),
                           by = "rowname") %>%
          tibble::column_to_rownames("rowname")
        
        req(nrow(cdata) > 0)
        wholedata <<- wholedata %>%
          filter_with_rownames(!.data[["rowname"]] %in% rownames(cdata)) %>%
          bind_rows(cdata %>% mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)))) 
        wholedata <<- wholedata[rownames(wholedata), , drop = FALSE]
        if (fval != "All"){
          subdata <- filter_with_rownames(wholedata , 
                                          .data[[idcolumn]] == fval) 
        } else {
          subdata <- wholedata
        }
        
        subdata
      }
      
      updateWholeData <- function(){
        cdata <- hot_to_r(isolate(input$tableedit))
        req(nrow(cdata) > 0)
        wholedata <<- wholedata %>%
          filter_with_rownames(!.data[["rowname"]] %in% rownames(cdata)) %>%
          bind_rows(cdata %>%
                      tibble::rownames_to_column("rowname") %>%
                      dplyr::left_join(wholedata %>%
                                         tibble::rownames_to_column("rowname") %>%
                                         dplyr::select(any_of(c("rowname", hiddencols))),
                                       by = "rowname") %>%
                      tibble::column_to_rownames("rowname") %>%
                      mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)))) 
        wholedata <<- wholedata[rownames(wholedata), , drop = FALSE]
      }
      
      
      rendertab <- function(data, complist){
        if (length(hiddencols) > 0)
          data <- data %>%
            select(!any_of(hiddencols))
        
        tab <-
          rhandsontable(data %>%
                          mutate(
                            across(
                              any_of(c("To", "From")),
                              ~ factor(.x,
                                       levels = complist)),
                            across(
                              any_of(c("Inside", "Trophic")),
                              ~ as.logical(.x ))),
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(columnSorting = TRUE,
                   manualColumnResize = TRUE)
        if (slot == "fluxes"){
          tab <- tab %>%
            hot_col(c("From", "To"),
                    type = "dropdown",
                    source = tmpnetwork$components$Component) %>%
            hot_col("Trophic", type = "checkbox")
        } else {
          tab <- tab %>%
            hot_col("Inside", type = "checkbox")
        }
        renderRHandsontable({tab})
      }
      
      
      
      
      shiny::observe({
        input$tableedit$changes$changes
        req(!is.null(input$tableedit$changes$changes))
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })
      
      shiny::observeEvent(input$ok,{
        updateWholeData()
        newdata <- wholedata %>%
          mutate(across(any_of(c("Inside" , "Trophic")), as.integer))
        shinyCatch({
          if (max(table(c(newdata[, ifelse(slot == "fluxes",
                                           "Flux",
                                           "Component")],
                          isolate(names(tmpnetwork$observations)),
                          tibble(
                            tmpnetwork[[ifelse(slot == "fluxes",
                                               "components",
                                               "fluxes")]])[,
                                                            ifelse(slot == "fluxes",
                                                                   "Component",
                                                                   "Flux")] %>%
                          dplyr::pull()))) > 1)
          stop("a name is already used")
        })
        
        if (slot == "fluxes"){
          newdata$from <- names(isolate(network$dictionary))[match(newdata$From,
                                                                   network$dictionary)]
          newdata$to <- names(isolate(network$dictionary))[match(newdata$To,
                                                                 network$dictionary)]
          tmpnetwork$fluxes <<- newdata
        } else {
          newfluxes <- isolate(network$fluxes)
          newfluxes$From <- newdata$Component[match(newfluxes$from,
                                                    newdata$id)]
          
          newfluxes$To <- newdata$Component[match(newfluxes$to,
                                                  newdata$id)]
          # newfluxes$Flux <- paste(newdata$Component[newfluxes$from == newdata$id],
          #                         newdata$Component[newfluxes$to == newdata$id],
          #                         sep = "_")
          tmpnetwork$fluxes <<- newfluxes
          tmpnetwork$components <<- newdata
          
        }
        
        # tabEnewnetwork[[slot]] <<- isolate(tmpnetwork[[slot]])
        # shinyjs::disable("ok")
        # shinyjs::disable("cancel")
        
      })
      shiny::observeEvent(input$cancel,{
        wholedata <<- tmpnetwork[[slot]]
        output$tableedit <- rendertab(tmpnetwork[[slot]],
                                      tmpnetwork$components$Component)
        if (!isTRUE(getOption("shiny.testmode"))) {
          shinyjs::disable("ok")
          shinyjs::disable("cancel")
        }
      })
      
      
      observe({
        ntab <- tab$panel
        if ((currenttab == "Components" & ntab != "Components" & slot == "components") |
            (currenttab == "Fluxes" & ntab != "Fluxes" & slot == "fluxes")){
          for (v in names(tmpnetwork)){
            if (!ident_tol(tmpnetwork[[v]],
                           isolate(tabEnewnetwork[[v]])))
              tabEnewnetwork[[v]] <<- tmpnetwork[[v]]
          }
        }
        currenttab <<- ntab
      })
      
      return(tabEnewnetwork)
      
    }
  )
}
