#' checkValidity
#'
#' @param myconstraint the formula
#' @param network the network object
#' @param onesided TRUE for alias, default FALSE
#'
#' @return a message (TRUE if valid otherwise the error message)
#' @importFrom stringr str_extract_all
#' @importFrom stats na.omit 
#' @importFrom symengine expand Vector V get_type get_args get_str
#' @export
#'

checkValidity <- function(myconstraint, network, onesided = FALSE){
  message <- "TRUE"
  tryCatch({
    if (myconstraint == "")
      stop("empty constraint")
    symbolic_enviro <- network$envir
    years <- network$observations$Year
    ###first we checked if they are vector year indices and change them
    ###for corresponding vector indices
    #[year:year]
    matches <- str_extract_all(myconstraint, "\\[[:digit:]*:[:digit:]*\\]")
    if (length(matches[[1]]) > 0) {
      for (i in seq_along(matches[[1]])) {
        indices <- match(strsplit(matches[[1]][i], "\\[|\\]|:")[[1]][-1],
                         years)
        indices <- paste("[",
                         paste(indices, collapse = ":"),
                         "]",
                         sep = "")
        myconstraint <- gsub(matches[[1]][i],
                             indices,
                             myconstraint,
                             fixed = TRUE)
      }
    }
    
    #[c(year,year,year...)]
    matches <- str_extract_all(myconstraint,
                               "\\[c\\(([:digit:]*,)+[:digit:]*\\)\\]")
    if (length(matches[[1]]) > 0) {
      for (i in seq_along(matches[[1]])) {
        indices <- match(strsplit(matches[[1]][i], "\\[c\\(|\\,|\\)\\]")[[1]][-1],
                         years)
        indices <- paste("[c(",
                         paste(indices, collapse = ","),
                         ")]",
                         sep = "")
        myconstraint <- gsub(matches[[1]][i],
                             indices,
                             myconstraint,
                             fixed = TRUE)
      }
    }
    
    #[year]
    matches <- str_extract_all(myconstraint, "\\[[:digit:]*\\]")
    if (length(matches[[1]]) > 0) {
      for (i in seq_along(matches[[1]])) {
        indices <- match(strsplit(matches[[1]][i], "\\[|\\]")[[1]][-1],
                         years)
        indices <- paste("[",
                         indices,
                         "]",
                         sep = "")
        myconstraint <- gsub(matches[[1]][i],
                             indices,
                             myconstraint,
                             fixed = TRUE)
      }
    }
    
    if (!onesided){
      sign <-
        ifelse(length(grep("<=", myconstraint)) > 0, "<=", ifelse(length(grep(
          ">=", myconstraint
        )) > 0, ">=", "="))
      tmp <- strsplit(myconstraint, sign)[[1]]
      if (sign == "<=" | sign == "=") {
        left <- tmp[1]
        right <- tmp[2]
      } else if (sign == ">=") {
        left <- tmp[2]
        right <- tmp[1]
      } else{
        stop(paste("unrecognized sign in constraint:", myconstraint))
      }
      if (length(tmp) == 1)
        stop("constraint seems to be one sided")
    } else {
      left <- myconstraint
      right <- "1"
    }
    
    
    symbolic_enviro$sum <- function(x, ..., na.rm = TRUE) {
      if (inherits(x, "VecBasic"))
        return(sum(x))
      base::sum(x, ..., na.rm = na.rm)
    }
    symbolic_enviro$mean <- function(x, ..., na.rm = TRUE) {
      if (inherits(x, "VecBasic"))
        return(mean(x))
      base::mean(x, ..., na.rm = na.rm)
    }
    
    symbolic_enviro$geomean <- function(x, ..., na.rm = TRUE) {
      if (inherits(x, "VecBasic"))
        stop("geomean can be applied only to time series")
      exp(base::mean(log(x), ..., na.rm = na.rm))
    }
    
    symbolic_constraint <-
      eval(parse(text = left), symbolic_enviro)  -
      eval(parse(text = right), symbolic_enviro)
    if (length(symbolic_constraint) == 1) #this is not a vector but a single
      #constraint
      symbolic_constraint <- V(symbolic_constraint)  #conversion into VecBasic
    
    
    
    
    
    
    
    valid <- 
      
      
      lapply(as.vector(symbolic_constraint), function(basic_constraint){
        
        #We know check that there is no division
        if (get_type(basic_constraint) == "Add") {
          denom <- lapply(as.list(get_args(basic_constraint)),
                          function(x) {
                            if (!get_type(x) %in% c("RealDouble", "Symbol")) {
                              members <- as.list(get_args(x))
                              types <- sapply(members, get_type)
                              if ("Pow" %in% types)
                                return(members[[which(types == "Pow")]])
                              return(S(1))
                            } else{
                              return(S(1))
                            }
                          })
          
          denominator <- 1 / do.call("prod", denom)
          nb_elem <- length(as.list(get_args(basic_constraint)))
          numerator <- do.call(sum,
                               lapply(seq_len(nb_elem), function(x) {
                                 as.list(get_args(basic_constraint))[[x]] *
                                   denominator
                               }))
        } else if (get_type(basic_constraint) != "Mul") {
          numerator <- basic_constraint
          denominator <- S(1)
        } else {
          members <- as.list(get_args(basic_constraint))
          types <- sapply(members, get_type)
          if ("Pow" %in% types) {
            numerator <- do.call(prod, members[-which(types == "Pow")])
            denominator <- members[which(types == "Pow")]
          } else {
            numerator <- basic_constraint
            denominator <- S(1)
            
          }
        }
        
        basic_constraint <- expand(numerator)
        
        mycoeffs <- NULL
        
        if (get_type(basic_constraint) != "Add") {
          if (get_type(basic_constraint) == "Symbol") {
            mycoeffs <- 1
            names(mycoeffs) <- get_str(basic_constraint)
          } else if (get_type(basic_constraint) == "Mul") {
            mycoeffs <- as.numeric(as.list(get_args(basic_constraint))[[1]])
            names(mycoeffs) <- get_str(as.list(get_args(basic_constraint))[[2]])
          } else if (get_type(basic_constraint) == "NaN") {
            mycoeffs <- NA
            names(mycoeffs) <- "1"
          }else{
            mycoeffs <- as.numeric(basic_constraint)
            names(mycoeffs) <- "1"
          }
        } else {
          mycoeffs <- sapply(as.list(get_args(basic_constraint)), function(e) {
            if (get_type(e) %in% c("Integer", "RealDouble")) {
              return(c("1" = as.numeric(e)))
            } else if (get_type(e) == "Symbol") {
              val <- 1
              names(val) <- get_str(e)
              return(val)
            } else if (get_type(e) == "NaN") {
              return(c("1" = NA))
            } else {
              val <- as.list(get_args(e))[[1]]
              val <- ifelse(get_type(val) == "NaN",
                            NA,
                            as.numeric(val))
              names(val) <- get_str(as.list(get_args(e))[[2]])
              return(val)
            }
          })
        }
        all(is.numeric(na.omit(mycoeffs))) | (length(na.omit(mycoeffs)) == 0)
      })
    if (!all(unlist(valid)))
      stop("constraint does not seem linear")
  },
  error = function(e)
    message <<- e
  )
  return (as.character(message))
  
  
  
}
