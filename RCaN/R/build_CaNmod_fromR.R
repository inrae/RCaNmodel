#' build_CaNmod_fromR
#'
#' builds a CaNmodobject directly from R by providing dataframes instead of
#' an excel template
#' description, including all the underlying equations
#' @param components_param a data frame describing compartments
#' @param fluxes_def a data frame describing fluxes
#' @param series a data frame describing time series of observations
#' @param constraints a data frame describing constraints
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
#' require(readxl)
#' file <- system.file("extdata",
#'  "CaN_template_mini.xlsx", package = "RCaN")
#'
#'  #for this example, we take the table as provided in the template file
#'  #but you can provide your own table form other sources
#'
#'  #read compartments
#'   components_param <-
#' read_excel(file, sheet = "Components & input parameter")
#'
#' #read Fluxes
#' fluxes_def <- read_excel(file, sheet = "Fluxes")
#'
#' #read Times series
#' series <- read_excel(file, sheet = "Input time-series")
#'
#' #read constraints
#' constraints <- read_excel(file, sheet = "Constraints")
#'
#' myCaNmod <- build_CaNmod_fromR(components_param,
#'                                fluxes_def,
#'                                series,
#'                                constraints)
#'
#' @importFrom Matrix Matrix
#' @importFrom stats na.omit
#'
build_CaNmod_fromR <- function(components_param,
                               fluxes_def,
                               series,
                               constraints
                         ) {
  #Components & input parameter
  #remove totally empty rows that sometimes happen with xlsx
  components_param <-
    components_param[!rowSums(is.na(components_param)) ==
                       ncol(components_param), ]
  if (length(which(!(
    components_param$in_out %in% c("In", "Out")
  ))) > 0)
    paste("compontents in_out should be either 'Out' or 'In'")
  index_species <- which(components_param$in_out == "In")
  components <- components_param$Component
  species <- as.character(components_param$Component[index_species])
  nbspecies <- length(species)

  #Fluxes
  #remove totally empty rows that sometimes happen with xlsx
  fluxes_def <-
    fluxes_def[!rowSums(is.na(fluxes_def)) ==
                 ncol(fluxes_def), ]
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

  # Times series
  #remove totally empty rows that sometimes happen with xlsx
  series <-
    series[!rowSums(is.na(series)) ==
             ncol(series), ]
  ntstep <- nrow(series)
  data_series_name <- names(series)[-1]


  #constraints
  #remove totally empty rows that sometimes happen with xlsx
  constraints <-
    constraints[!rowSums(is.na(constraints)) ==
                  ncol(constraints), ]
  #we keep only active constraints
  if (! "Active" %in% names(constraints))
    constraints$Active <- TRUE

  lessthan <- grep("<", constraints$Constraint)
  greaterthan <- grep(">", constraints$Constraint)
  equality <- grep("^[^<>]+$", constraints$Constraint)

  pattern_indices <- "(\\[[[:digit:]]*:[[:digit:]]*\\])"
  pattern_indices <- paste(pattern_indices,
                           "(\\[c\\(([[:digit:]]*,)+[[:digit:]]*\\)\\])",
                           sep = "|")
  pattern_indices <- paste(pattern_indices,
                           "(\\[[[:digit:]]*\\])",
                           sep = "|")

  constraints_word <-
    unlist(sapply(as.character(constraints$Constraint), function(x)
      strsplit(x, split = ",|/|\\+|=|<|\\*|>|\\-|\\)|\\(|[[:space:]]")))
  constraints_word <- gsub(pattern_indices,
                           "",
                           constraints_word)
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
  H <- diag(1 - exp(-components_param$OtherLosses[index_species]),
            nrow=length(index_species))
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
            (components_param$OtherLosses[index_species,drop=FALSE]), "*")
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
  rownames(A) <- paste("Flow positiveness",
                       gsub( "_"," : ", colnames(A)[-1]),
                       sep = "_")

  ####add refuge biomasses/biomass positiveness
  A <-
    rbind(A, do.call(
      rbind,
      lapply(
        components_param$Component[components_param$Component %in% species],
        function(sp)
          treat_constraint(
            paste(sp, ">=", ifelse(
              is.na(
                components_param$RefugeBiomass[
                  components_param$Component == sp]),

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
      lapply(
        species_flow_to[!is.na(
          components_param$Satiation[match(species_flow_to,
                                           components_param$Component)])],
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
    rbind(A,
          do.call(
            rbind,
            lapply(
              components_param$Component[components_param$Component %in%
                                           species &
                                           !is.na(components_param$Inertia)],
              function(sp) {
                #increase
                emigrants <-
                  as.character(fluxes_def$Flux)[
                    as.character(fluxes_def$From) == sp &
                      !fluxes_def$Trophic]
                treat_constraint(
                  paste(
                    sp,
                    "[-1] >=",
                    sp,
                    "[1:(length(",
                    sp,
                    ")-1)]*exp(-",
                    components_param$Inertia[
                      components_param$Component == sp],
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
                  name_constr = paste("inertia_sup",
                                      sp,
                                      sep = "_")
                )
              })))
  A <-
    rbind(A,
          do.call(rbind,
                  lapply(
                    components_param$Component[
                      components_param$Component %in% species &
                        !is.na(components_param$Inertia)],
                    function(sp) {
                      #decrease
                      immigrants <-
                        as.character(fluxes_def$Flux)[
                          as.character(fluxes_def$To) == sp &
                            !fluxes_def$Trophic]
                      treat_constraint(
                        paste(
                          sp,
                          "[-1] <=",
                          sp,
                          "[1:(length(",
                          sp,
                          ")-1)]*exp(",
                          components_param$Inertia[
                            components_param$Component == sp],
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
                          #we do not take into account
                          #imemigrants
                          sep = ""
                        ),
                        symbolic_enviro,
                        name_constr = paste("inertia_inf",
                                            sp,
                                            sep = "_")
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
          as.character(constraints$`Time-range`[c(lessthan, greaterthan)]),
          as.character(constraints$Id[c(lessthan, greaterthan)])
        )
      ))

  }
  b <- -A[, 1]
  A <- A [, -1]

  tmp <- expand.grid(flow, series$Year)
  colnames(A) <- c(paste(species, "[", series$Year[1], "]", sep = ""),
                   paste(tmp[, 1], "[", tmp[, 2], "]", sep = ""))


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
          as.character(constraints$`Time-range`[equality]),
          as.character(constraints$Id[equality])
        )
      ))
  }
  v <- -C[, 1]
  C <- C[, -1]
  colnames(C) <- colnames(A)


  AAll <- A
  bAll <- b
  CAll <- C
  vAll <- v

  activeconstr <- subset(constraints, as.logical(constraints$Active))
  activeconstr <- paste("^[",
                        paste(activeconstr, collapse = '|', sep = ""),
                        "]",
                        " : ")

  b <- b[grep(activeconstr, rownames(A))]
  A <- A[grep(activeconstr, rownames(A)), ]
  v <- v[grep(activeconstr, rownames(C))]
  C <- C[grep(activeconstr, rownames(C)), ]


  #we remove inactive constraint

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
    AAll = AAll,
    C = C,
    CAll = CAll,
    v = v,
    vAll=vAll,
    L = L,
    b = b,
    bAll= bAll,
    symbolic_enviro = symbolic_enviro
  )
  class(myCaNmod) <- "CaNmod"


  return(myCaNmod)
}
