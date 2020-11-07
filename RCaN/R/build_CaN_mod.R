#' build_CaNmod
#'
#' function that reads the template and return build the complete model
#' description, including all the underlying equations
#' @param file the name of the file enclosing the model description
#'
#' @return a CaNmod object with following elements
#' \itemize{
#'  \item{"components_param"}{the table of components description}
#'  \item{"species"}{the name of the species}
#'  \item{"fluxes_def}{the table of fluxes definition}
#'  \item{"ntstep"}{the number of time steps}
#'  \item{"data_series_name"}{the names of the data series}a
#'  \item{"constraints"}{the table of constraints description}
#'  \item{"H"}{the H matrix from (I-H).B+N.F}
#'  \item{"N"}{the N matrix from (I-H).B+N.F}
#'  \item{"A"}{matrix of active constraints A.x<=b}
#'  \item{"AAll"}{matrix of all constraints A.x<=b}
#'  \item{"b"}{vector of active constraints A.x<=b}
#'  \item{"bAll"}{vector of all constraints A.x<=b}
#'  \item{"C"}{matrix of active constraints C.x=v}
#'  \item{"CAll"}{matrix of all constraints C.x=v}
#'  \item{"v"}{vector of active constraints C.x=v}
#'  \item{"vAll"}{vector of all constraints C.x=v}
#'  \item{"L"}{matrix of B=L.F, since B0 is a parameter, there is no M}
#'  \item{"symbolic_enviro"}{an environment storing all symbolic objects
#'  required for the computation}
#' }
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata",
#'  "CaN_template_mini.xlsx", package = "RCaN"))
#'
#' @importFrom readxl read_excel
#' @importFrom Matrix Matrix
#' @importFrom stats na.omit
build_CaNmod <- function(file) {
  #Components & input parameter
  components_param <- as.data.frame(
    read_excel(file, sheet = "Components & input parameter")
  )

  #read Fluxes
  fluxes_def <- as.data.frame(
    read_excel(file, sheet = "Fluxes")
  )

  #read Times series
  series <- as.data.frame(
    read_excel(file, sheet = "Input time-series")
  )

  #read constraints
  constraints <- as.data.frame(
    read_excel(file, sheet = "Constraints")
  )

  myCaNmod <- build_CaNmod_fromR(components_param,
                                 fluxes_def,
                                 series,
                                 constraints)


  return(myCaNmod)
}
