files <- list(system.file("extdata",
                          "CaN_template_mini.xlsx", package = "RCaN"),
              system.file("extdata",
                          "CaN_template_mini_generic.xlsx", package = "RCaN"))

generict <- c(FALSE, TRUE)

test_that("class of outputs objects are correct", {
  for (i in seq_along(files)){
    myCaNmod <- buildCaN(files[[i]], generic = generict[i])
    expect_s4_class(myCaNmod$AAll, "Matrix")
    expect_s4_class(myCaNmod$A, "Matrix")
    expect_type(myCaNmod$v, "double")
    expect_type(myCaNmod$b, "double")
    expect_s4_class(myCaNmod$C, "Matrix")
    expect_s4_class(myCaNmod$CAll, "Matrix")
  }
})


test_that("Dimensions of outputs objects are correct", {
  for (i in seq_along(files)){
    myCaNmod <- buildCaN(files[[i]], generic = generict[i])
    expect_equal(ncol(myCaNmod$AAll),
                 length(myCaNmod$species) +
                   length(myCaNmod$flow) * nrow(myCaNmod$series))
    expect_equal(ncol(myCaNmod$A),
                 length(myCaNmod$species) +
                   length(myCaNmod$flow) * nrow(myCaNmod$series))
    expect_equal(ncol(myCaNmod$C),
                 length(myCaNmod$species) +
                   length(myCaNmod$flow) * nrow(myCaNmod$series))
    expect_equal(ncol(myCaNmod$CAll),
                 length(myCaNmod$species) +
                   length(myCaNmod$flow) * nrow(myCaNmod$series))
  }
})

test_that("inconsistencies in names are detected",{
  for (i in seq_along(files)){
    myCaNmod <- buildCaN(files[[i]], generic = generict[i])
    components <- myCaNmod$components_param
    components$Component[1] <- paste0(components$Component[1], " ")

    expect_error(buildCaN(list(components_param = components,
                               fluxes_def = myCaNmod$fluxes_def,
                               series = myCaNmod$series,
                               constraints = myCaNmod$constraints),
                          generic = generict[i]))

    fluxes <- myCaNmod$fluxes_def
    fluxes$From[1] <- paste0(components$fluxes$From[1], " ")

    expect_error(buildCaN(list(components_param = myCaNmod$components_param,
                               fluxes_def = fluxes,
                               series = myCaNmod$series,
                               constraints = myCaNmod$constraints)))

    fluxes <- myCaNmod$fluxes_def
    fluxes$To[1] <- paste0(components$fluxes$To[1], " ")

    expect_error(buildCaN(list(components_param = myCaNmod$components_param,
                               fluxes_def = fluxes,
                               series = myCaNmod$series,
                               constraints = myCaNmod$constraints),
                          generic = generict[i]))
  }


})

