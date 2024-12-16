library(shiny)
library(shinyjs)
devtools::load_all("../../")
shinyApp(ui = RCaNconstructorUI,
         server = RCaNconstructorServer)
