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
#'  \item{"data_series_name"}{the names of the data series}
#'  \item{"constraints"}{the table of constraints description}
#'  \item{"H"}{the H matrix from (I-H).B+N.F}
#'  \item{"N"}{the N matrix from (I-H).B+N.F}
#'  \item{"A"}{matrix of constraints A.x<=b}
#'  \item{"b"}{vector of constraints A.x<=b}
#'  \item{"C"}{matrix of constraints C.x=v}
#'  \item{"v"}{vector of constraints C.x=v}
#'  \item{"L"}{matrix of B=L.F+M}
#'  \item{"M"}{vector of B=L.F+M}
#'  \item{"symbolic_enviro"}{an environment storing all symbolic objects
#'  required for the computation}
#' }
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata",
#'  "CaN_template_mini.xlsx", package = "RCaN"))
#'
#' @importFrom xlsx read.xlsx
#' @importFrom Matrix Matrix
#' @importFrom stats na.omit
build_CaNmod <- function(file) {
  #Components & input parameter
  components_param <-
    read.xlsx(file, sheetName = "Components & input parameter")
  if (length(which(!(
    components_param$in_out %in% c("In", "Out")
  ))) > 0)
    paste("compontents in_out should be either 'Out' or 'In'")
  index_species <- which(components_param$in_out == "In")
  components <- components_param$Component
  species <- as.character(components_param$Component[index_species])
  nbspecies <- length(species)

  #read Fluxes
  fluxes_def <- read.xlsx(file, sheetName = "Fluxes")
  flow <- as.character(fluxes_def$Flux)
  nbfluxes <- nrow(fluxes_def)
  if (length(which(!(fluxes_def$From %in% components))) > 0)
    stop(paste(
      "In sheet fluxes, column From, not recognized:",
      fluxes_def$From[!(fluxes_def$From %in% components)]
    ))
  if (length(which(!(fluxes_def$To %in% components))) > 0)
    stop(paste("In sheet fluxes, column To, not recognized:",
               fluxes_def$To[!(fluxes_def$To %in% components)]))
  fluxes_from <- match(fluxes_def$From, species)
  fluxes_to <- match(fluxes_def$To, species)

  if (length(which(fluxes_def$Trophic != 0 &
                   fluxes_def$Trophic != 1)) > 0)
    stop("In sheet fluxes, Trophic should be 1 or 0")
  is_trophic_flux <- fluxes_def$Trophic == 1

  #read Times series
  series <- read.xlsx(file, sheetName = "Input time-series")
  ntstep <- nrow(series)
  data_series_name <- names(series)[-1]


  #read constraints
  constraints <- read.xlsx(file, sheetName = "Constraints")
  lessthan <- grep("<", constraints$Constraint)
  greaterthan <- grep(">", constraints$Constraint)
  equality <- grep("^[^<>]+$", constraints$Constraint)

  constraints_word <-
    unlist(sapply(as.character(constraints$Constraint), function(x)
      strsplit(x, split = "/|\\+|=|<|\\*|>|\\-|\\)|\\(|[[:space:]]")))
  not_recognized <-
    which(
      !constraints_word %in% c(
        as.character(components),
        flow,
        data_series_name,
        "mean",
        "sum",
        "na.rm",
        "TRUE",
        "FALSE",
        ""
      ) & suppressWarnings(is.na(as.numeric(constraints_word)))
    )
  if (length(not_recognized) > 0)
    stop(paste("words not recognized in constraints:",
               constraints_word[not_recognized]))

  #build matrices H and N
  H <- diag(1 - exp(-components_param$OtherLosses[index_species]))
  N <- matrix(0, nbspecies, nbfluxes)
  N[cbind(fluxes_from, 1:nbfluxes)] <- -1 #this is an outgoing flow
  N[na.omit(cbind(fluxes_to, 1:nbfluxes))] <-
    na.omit(
      N[cbind(fluxes_to, 1:nbfluxes)] + ifelse(
        is_trophic_flux,
        components_param$AssimilationE[match(fluxes_def$To, components)] *
          components_param$Digestibility[match(fluxes_def$From, components)],
        1
      )
    ) #if it is not a trophic flow, we do not take into account assimilation
      # and digestibility
  N <-
    sweep(N, 1, STATS = diag(H) /
            (components_param$OtherLosses[index_species]), "*")
  rownames(N) <- species
  colnames(N) <- flow
  colnames(H) <- rownames(H) <- species

  #build symbolic objects in a specific environment
  symbolic_enviro <-
    generate_symbolic_objects(flow,
                              species,
                              ntstep,
                              H,
                              N,
                              components_param$InitialBiomass[index_species],
                              series)


  #build A matrix and b corresponding to constraints A.x<=b
  nbparam <- length(symbolic_enviro$param)
  A <-
    Matrix::Matrix(0, 0, length(symbolic_enviro$param), sparse = TRUE)
  #first column stores -b
  colnames(A) <- as.character(symbolic_enviro$param)

  ####add flow positiveness
  A <-
    rbind(A, cbind(rep(0, nbparam - 1), diag(-1, nbparam - 1, nbparam -
                                               1)))
  rownames(A) <- paste(colnames(A)[-1], ">=0")

  ####add refuge biomasses/biomass positiveness
  A <-
    rbind(A, do.call(
      rbind,
      lapply(components_param$Component[components_param$Component %in% species], function(sp)
        treat_constraint(
          paste(sp, ">=", ifelse(
            is.na(components_param$RefugeBiomass[components_param$Component == sp]),
            0,
            components_param$RefugeBiomass[components_param$Component == sp]
          )),
          symbolic_enviro,
          name_constr = paste("Biomass positiveness_refuge", sp, sep = "_")
        ))
    ))

  ####add satiation
  species_flow_to <-
    unique(as.character(fluxes_def$To[fluxes_def$To %in% species &
                                        is_trophic_flux]))
  A <-
    rbind(A, do.call(
      rbind,
      lapply(species_flow_to[!is.na(components_param$Satiation[match(species_flow_to, components_param$Component)])],
             function(sp)
               treat_constraint(
                 paste(
                   paste(fluxes_def$Flux[fluxes_def$To == sp &
                                           is_trophic_flux], collapse = "+"),
                   "<=",
                   components_param$Satiation[components_param$Component == sp],
                   "*",
                   sp
                 ),
                 symbolic_enviro,
                 name_constr = paste("satiation", sp, sep = "_")
               ))
    ))
  ####add inertia
  A <-
    rbind(A, do.call(rbind, lapply(components_param$Component[components_param$Component %in%
                                                                species &
                                                                !is.na(components_param$Inertia)],
                                   function(sp) {
                                     #increase
                                     emigrants <-
                                       as.character(fluxes_def$Flux)[as.character(fluxes_def$From) == sp &
                                                                       !fluxes_def$Trophic]
                                     treat_constraint(
                                       paste(
                                         sp,
                                         "[-1] >=",
                                         sp,
                                         "[1:(length(",
                                         sp,
                                         ")-1)]*exp(-",
                                         components_param$Inertia[components_param$Component == sp],
                                         ")",
                                         ifelse(length(emigrants) >
                                                  0, paste(
                                                    "-",
                                                    paste(
                                                      emigrants,
                                                      collapse = "-",
                                                      "[1:(length(",
                                                      sp,
                                                      ")-1)]",
                                                      sep = ""
                                                    ),
                                                    sep = ""
                                                  ), ""),
                                         #we do not take into account emigrants
                                         sep = ""
                                       ),
                                       symbolic_enviro,
                                       name_constr = paste("inertia_sup", sp, sep = "_")
                                     )
                                   })))
  A <-
    rbind(A, do.call(rbind, lapply(components_param$Component[components_param$Component %in%
                                                                species &
                                                                !is.na(components_param$Inertia)],
                                   function(sp) {
                                     #decrease
                                     immigrants <-
                                       as.character(fluxes_def$Flux)[as.character(fluxes_def$To) == sp &
                                                                       !fluxes_def$Trophic]
                                     treat_constraint(
                                       paste(
                                         sp,
                                         "[-1] <=",
                                         sp,
                                         "[1:(length(",
                                         sp,
                                         ")-1)]*exp(",
                                         components_param$Inertia[components_param$Component == sp],
                                         ")",
                                         ifelse(length(immigrants) >
                                                  0, paste(
                                                    "+",
                                                    paste(
                                                      immigrants,
                                                      collapse = "+",
                                                      "[1:(length(",
                                                      sp,
                                                      ")-1)]",
                                                      sep = ""
                                                    ),
                                                    sep = ""
                                                  ), ""),
                                         #we do not take into account imemigrants
                                         sep = ""
                                       ),
                                       symbolic_enviro,
                                       name_constr = paste("inertia_inf", sp, sep = "_")
                                     )
                                   })))


  ####add constraint provided by user
  if (length(lessthan) + length(greaterthan) > 0) {
    A <-
      rbind(A, do.call(
        rbind,
        mapply(
          function(c, yr, id)
            treat_constraint(c, symbolic_enviro, yr, id),
          as.character(constraints$Constraint[c(lessthan, greaterthan)]),
          as.character(constraints$Time.range[c(lessthan, greaterthan)]),
          as.character(constraints$Id[c(lessthan, greaterthan)])
        )
      ))

  }
  b <- -A[, 1]
  A <- A [, -1]

  tmp <- expand.grid(flow, series$Year)
  colnames(A) <- paste(tmp[, 1], "[", tmp[, 2], "]", sep = "")


  ####build matrix for equality constraint  C x = v and fill it
  C <-
    Matrix::Matrix(0, 0, length(symbolic_enviro$param), sparse = TRUE)
  colnames(C) <- as.character(symbolic_enviro$param)
  if (length(equality) > 0) {
    C <-
      rbind(C, do.call(
        rbind,
        mapply(
          function(c, yr, id)
            treat_constraint(c, symbolic_enviro, yr, id),
          as.character(constraints$Constraint[equality]),
          as.character(constraints$Time.range[equality]),
          as.character(constraints$Id[equality])
        )
      ))
  }
  v <- -C[, 1]
  C <- C[, -1]

  ####build matrix L and M of B=L.F+M
  L <-
    Matrix::Matrix(0,
                   0,
                   length(symbolic_enviro$param),
                   sparse = TRUE) #first column stores -b
  L <-
    rbind(L, do.call("rbind",  lapply(species, function(sp)
      do.call(
        "rbind",
        lapply(as.vector(expand(eval(
          parse(text = sp), symbolic_enviro
        ))), function(s)
          build_vector_constraint(s, symbolic_enviro))
      ))))
  tmp <- expand.grid(series$Year, species)
  rownames(L) <- paste(tmp[, 2], "[", tmp[, 1], "]", sep = "")
  M <- L[, 1]
  L <- L[, -1]
  colnames(L) <- colnames(A)


  myCaNmod <- list(
    components_param = components_param,
    species = species,
    fluxes_def = fluxes_def,
    flow = flow,
    series = series,
    ntstep = ntstep,
    data_series_name = data_series_name,
    constraints = constraints,
    H = H,
    N = N,
    A = A,
    C = C,
    v = v,
    L = L,
    M = M,
    b = b,
    symbolic_enviro = symbolic_enviro
  )
  class(myCaNmod) <- "CaNmod"


  return(myCaNmod)
}
