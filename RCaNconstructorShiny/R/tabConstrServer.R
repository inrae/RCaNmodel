#' tabConstrServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param slot components or aliases
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r hot_cell
#' @importFrom shiny isolate observe showNotification
#' @export

tabConstrServer <- function(id, network, slot, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""

      hiddencols <- c("idconstraint", "id", "valid")

      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()

      wholedata <- data.frame()
      


      idcolumn <- "Id"





      observe({
        network$components
        network$observations
        network$fluxes
        network$dictionary
        network$constraints
        network$model
        network$metaobs

        req(isolate(tab$panel) == currenttab)


        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }
        output$tableedit <- rendertab(tmpnetwork[[slot]])
        
        wholedata <<- tmpnetwork$constraints
        choices <- sort(tmpnetwork$constraints$Id)
        updateSelectInput(session,
                          "filter",
                          choices = c("All", 
                                      choices),
                          selected = "All")

      })
      
      observeEvent(input$filter, {
        output$tableedit <- rendertab(createSubData())
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
          bind_rows(cdata %>% mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)))) 
        wholedata <<- wholedata[rownames(wholedata), , drop = FALSE]
      }

      formcol <- ifelse(slot == "constraints",
                        "Constraint",
                        "Formula")

      rendertab <- function(data){
        if (nrow(data) > 0){
          data[, formcol] <- sapply(data$idconstraint,
                                    convertidConstr2Constr,
                                    dictionary = tmpnetwork$dictionary)


          if (slot == "aliases"){
            allconstraints <- data$Formula
          } else {
            allconstraints <- data$Constraint
          }
          validity_comments <- sapply(
            allconstraints,
            function(constr)
              checkValidity(constr,
                            tmpnetwork,
                            onesided = (slot == "aliases")))
          data$valid <- (validity_comments == "TRUE")
        } else {
          validity_comments <- character(0)
        }




        col_highlight <- 1
        row_highlight <- which(!data$valid) - 1
        
        if (length(hiddencols) > 0)
          data <- data %>%
          select(!any_of(hiddencols))
        
        tab <-
          rhandsontable(data %>%
                          mutate(
                            across(
                              any_of(c("Active")),
                              ~ as.logical(.x ))),
                        strechH = "all",
                        row_highlight = as.vector(row_highlight),
                        col_highlight = col_highlight,
                        overflow = "visible") %>%
          hot_cols(manualColumnResize = TRUE,
                   columnSorting = TRUE) %>%
          hot_col(2,
                  renderer = "
                   function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]
                       hcols = instance.params.col_highlight
                       hcols = hcols instanceof Array ? hcols : [hcols]

                       if (hrows.includes(row)) {
                         td.style.background = 'red'
                       }


                     }
                   }")
        for (i in as.vector(row_highlight)){
          tab <- tab %>%
            hot_cell(i + 1, 2, comment = as.character(validity_comments[i + 1]))
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
        
        newdata <- wholedata
        req(newdata)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          mutate(across(any_of(c("Active")), as.integer))
        tryCatch({
          for (tr in newdata$`Time-range`){
            tmp <- as.numeric(eval(parse(text=tr)))
          }
          for (const in newdata[, formcol]  %>% pull()) {
            valid <- checkValidity(const,
                                   tmpnetwork,
                                   onesided = (slot == "aliases"))
            if (valid != "TRUE")
              stop(paste(const, valid, sep = ":"))
          }
          countid <- table(newdata$Id)
          if (max(countid) > 1)
            stop(paste(paste(names(countid)[which(countid > 1)],
                             collapse = ", "),
                       "non unique Id"))
          newdata$idconstraint <- convertConstr2idConstr(newdata[, formcol] %>% pull(),
                                                         tmpnetwork$dictionary)
          tmpnetwork[[slot]] <<- newdata
          shinyjs::disable("ok")
          shinyjs::disable("cancel")},
          error = function(e){
            showNotification(e,
                             type = "error")
          }
        )



      })

      shiny::observeEvent(input$cancel,{
        wholedata <<- tmpnetwork[[slot]]
        output$tableedit <- rendertab(tmpnetwork[[slot]])
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
      })


      observe({
        ntab <- tab$panel
        if ((currenttab == "View Constraints" & ntab != "View Constraints" & slot == "constraints") |
            (currenttab == 'View Aliases' & ntab != 'View Aliases' & slot == "aliases")) {
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
