#' getTimerestrictedRCaNfile
#'
#' This function generates a RCaN file restricted to a specific time range
#' based on another RCaN file
#' @param infile path to the orginal RCaN file
#' @param outfile where to write the resulting RCaNfile
#' @param from first year of the restricted range, if NULL (default), the same
#' as the initial model year
#' @param to last year of the restricted range, if NULL (default), the same
#' as the last model year
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom stringr str_match_all str_replace_all
#' @importFrom rlang !! sym
#' @return nothing but writes the file where specified
#'
#' @examples
#' infile <- system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaNmodel")
#' outfile <- paste0(tempfile(), ".xlsx")
#' getTimerestrictedRCaNfile(infile, outfile, from = 1989)
#' @export
#' 

getTimerestrictedRCaNfile <-
  function(infile, outfile, from = NULL, to = NULL) {
    if (!file.exists(infile)) stop("input file does not exist")
    if (file.exists(outfile) && !dir.exists(outfile))
      stop("outfile already exists")
    if (!dir.exists(dirname(outfile))) 
      stop("the path of outfile does not exist")
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      print("Package openxlsx needed for this function to work.
         Please install it",
            call. = FALSE)
      return()
    }
    components_param <- as.data.frame(
      read_excel(infile, sheet = "Components & input parameter")
    )
    
    #read Fluxes
    fluxes_def <- as.data.frame(
      read_excel(infile, sheet = "Fluxes")
    )
    
    #read Times series
    series <- as.data.frame(
      read_excel(infile, sheet = "Input time-series")
    )
    
    #read constraints
    constraints <- as.data.frame(
      read_excel(infile, sheet = "Constraints")
    )
    
    aliases <- NULL
    if ("Aliases" %in% excel_sheets(infile)) {
      aliases <-  as.data.frame(
        read_excel(infile, sheet = "Aliases")
      )
    }
    y0 <- min(series$Year)
    yN <- max(series$Year)
    
    if (is.null(to)) to <- yN
    if (is.null(from)) from <- y0
    if (from < y0 || from > yN) stop("from should is outside model range")
    if (to < y0 | to > yN) stop("to should is outside model range")
    if (to <= from) stop("to should be greater than from")
    if (to == yN & from == y0) stop("model range unchanged")
    
    series <- series %>%
      filter(!!sym("Year") >= from & !!sym("Year") <= to)
    
    constraintsallyears <- constraints %>%
      filter(is.na(!!sym("Time-range")))
    constraintsrestricted <- constraints %>%
      filter(!is.na(!!sym("Time-range")))
    if (nrow(constraintsrestricted) > 0){
      times <- sapply(constraintsrestricted$`Time-range`,
                      function(x) {
                        tr <- eval(parse(text=x))
                        deparse(tr[tr %in% from:to])
                      }
      )
      times <- as.character(sapply(times, dput))
      constraintsrestricted <- constraintsrestricted %>%
        mutate(`Time-range` = times) %>%
        filter(!(!!sym("Time-range") %in% c("numeric(0)", "integer(0)")))
      constraints <- bind_rows(constraintsallyears, constraintsrestricted)
    }
    
    
    #subfunction to replace in a formula
    replaceformula <- function(cons){
      years <- unique(str_match_all(cons, "\\[\\s*(.*?)\\s*\\]")[[1]])
      if (nrow(years) > 0){
        newyears <- sapply(years[, 2], function(x) {
          oldyears <- eval(parse(text = x))
          paste0("[", deparse(oldyears[oldyears %in% from:to]), "]")
        })
        names(newyears) <- years[, 1]
        cons <- str_replace_all(cons, fixed(newyears))
        if (any(newyears %in% c("[numeric(0)]", "[integer(0)]"))) cons <- NA
      }
      cons
    }
    
    if (nrow(constraints) > 0){
      for (i in seq_len(nrow(constraints))){
        constraints$Constraint[i] <- replaceformula(constraints$Constraint[i])
      }
      constraints <- constraints %>%
        filter(!is.na(!!sym("Constraint")))
    }
    
    if (!is.null(aliases)){
      names(aliases) <- c("Alias", "Formula")
      if (nrow(aliases > 0)){
        for (i in seq_len(nrow(aliases))){
          aliases[i, 2] <- replaceformula(aliases[i, 2])
        }
        aliases <- aliases %>%
          filter(!is.na(!!sym("Formula")))
      }
    }
    out <- list("Components & input parameter" = components_param,
                "Fluxes" = fluxes_def,
                "Input time-series" = series,
                "Constraints" = constraints)
    if (!is.null(aliases)){
      if (nrow(aliases) > 0)
        out <- append(out, list("Aliases" = aliases))
    }
    openxlsx::write.xlsx(out, file = outfile)
    
  }
