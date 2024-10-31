#' tabConstrServer
#'
#' @param id the id of the ui
#' @param network the current network
#' @param tab selected tab
#'
#' @return an updated network
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across any_of
#' @importFrom rhandsontable rhandsontable hot_col renderRHandsontable hot_cols
#' @importFrom rhandsontable hot_rows hot_to_r
#' @importFrom shiny isolate observe
#' @export

tabConstrServer <- function(id, network, tab){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      currenttab <- ""

      hiddencols <- c("idconstraint")

      newnetwork <- createEmptyNetwork()
      tmpnetwork <- list()








      observe({
        network$components
        network$observations
        network$fluxes
        network$dictionary
        network$constraints
        network$model
        req(isolate(tab$panel) == currenttab)


        for (v in names(isolate(network))){
          if(!identical(isolate(network[[v]]),
                        tmpnetwork[[v]]))
            tmpnetwork[[v]] <<- isolate(network[[v]])
        }
        output$tableedit <- rendertab(tmpnetwork$constraints)

      })

      rendertab <- function(data){
        data$Constraint <- sapply(data$idconstraint,
                                  convertidConstr2Constr,
                                  dictionary = tmpnetwork$dictionary)
        tab <-
          rhandsontable(data %>%
                          mutate(
                            across(
                              any_of(c("Active")),
                              ~ as.logical(.x ))),
                        strechH = "all",
                        overflow = "visible") %>%
          hot_cols(colWidths = ifelse(names(data) %in% hiddencols,
                                      1,
                                      200),
                   manualColumnResize = TRUE)
        renderRHandsontable({tab})
      }




      shiny::observe({
        input$tableedit$changes$changes
        req(!is.null(input$tableedit$changes$changes))
        shinyjs::enable("ok")
        shinyjs::enable("cancel")
      })

      shiny::observeEvent(input$ok,{
        newdata <- hot_to_r(input$tableedit)
        req(newdata)
        validate(need(nrow(newdata) > 0, "no data"))
        newdata <- newdata %>%
          mutate(across(any_of(c("Active")), as.integer))
        tryCatch({
          for (tr in newdata$`Time-range`){
            tmp <- as.numeric(eval(parse(text=tr)))
          }
          for (const in newdata$Constraint) {
            valid <- checkValidity(const, tmpnetwork)
            if (valid != "TRUE")
              stop(paste(const, valid, sep = ":"))
          }
          countid <- table(newdata$Id)
          if (max(countid) > 1)
            stop(paste(paste(names(countid)[which(countid > 1)],
                             collapse = ", "),
                       "non unique Id"))
          tmpnetwork$constraints <<- newdata
          shinyjs::disable("ok")
          shinyjs::disable("cancel")},
          error = function(e){
            showNotification(e,
                             type = "error")
          }
        )



      })

      shiny::observeEvent(input$cancel,{
        output$tableedit <- rendertab(tmpnetwork$constraints)
        shinyjs::disable("ok")
        shinyjs::disable("cancel")
      })


      observe({
        ntab <- tab$panel
        if (currenttab == "View Constraints" & ntab != "View Constraints"){
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
