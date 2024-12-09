#' writeRCaNfile
#'
#' @param modelfile the workbook to write
#' @param filenewnetwork the description of the network
#' @param param the parameter of the info
#' @param input the input for which to retrieve info
#' @param orig the original file
#'
#' @return an updated modefile
#' @importFrom tibble tibble
#' @export
#'
writeRCaNfile <- function(modelfile, filenewnetwork, param, input, orig){
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
  obs <- tibble(Year = integer())
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
  if (!"TimeLines" %in% sheets)
    modelfile <- openxlsx2::wb_add_worksheet(modelfile,
                                             "TimeLines")
  modelfile <- openxlsx2::wb_add_data(modelfile,
                                      sheet = "TimeLines",
                                      x = filenewnetwork$timeline)
  
  
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
  return(modelfile)
}