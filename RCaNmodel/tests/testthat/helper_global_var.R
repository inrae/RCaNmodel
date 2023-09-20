templatefiles <- list(system.file("extdata",
                                  "CaN_template_mini.xlsx", package = "RCaNmodel"),
                      system.file("extdata",
                                  "CaN_template_mini_generic.xlsx", package = "RCaNmodel"))

generictemplate <- c(FALSE, TRUE)
myCaNmod <- buildCaN(templatefiles[[1]])
myCaNmod_generic <- buildCaN(templatefiles[[2]], generic = TRUE)
sampleCaNmod <- sampleCaN(myCaNmod, 100)
