#' writeCaN2XLS
#'
#' writes an xlsx file that would correspond to the template of the CaNMod
#' object
#' @param file the name of the export file
#' @param myCaNmod a CaNmod object to be exported
#' @export
#'
#' @examples
#' myCaNmod <- buildCaN(system.file("extdata",
#'  "CaN_template_mini.xlsx", package = "RCaNmodel"))
#'  writeCaN2XLS(paste(tempdir(),
#'                          "/export.xlsx",
#'                          sep = ""),
#'                    myCaNmod)
#'
#' @importFrom writexl write_xlsx
writeCaN2XLS <- function(file,myCaNmod) {
  if (!endsWith(file, ".xlsx"))
    stop("file name should end with .xlsx")
  if (!dir.exists(dirname(file))){
    stop("the directory does not exist")
  }
  if (file.exists(file))
    stop("file already exists")
  list_data <- list(myCaNmod$components_param,
                    myCaNmod$fluxes_def,
                    myCaNmod$series,
                    myCaNmod$constraints)
  names(list_data) <- c("Components & input parameter",
                        "Fluxes",
                        "Input time-series",
                        "Constraints")
  if (!is.null(myCaNmod$dynamics)){
    list_data <- c(list_data, myCaNmod$dynamics)
    names(list_data) <- c(list_data,
                          "Dynamics")
  }

  if (!is.null(myCaNmod$aliases)){
    list_data <- c(list_data, myCaNmod$aliases)
    names(list_data) <- c(list_data,
                          "Aliases")
  }
  write_xlsx(list_data,
             path =  file)
}
