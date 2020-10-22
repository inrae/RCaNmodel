#' toggle_constraint
#'
#' Activate or inactivate a constraint.
#' @param myCaNmod a CaNmod object with following elements
#' @param constr a vector of strings identifying the constraints to be toogled.
#' The elements will be compared to the beginning of the line names of
#' matrices A and C (regular expressions).
#' @return an updated CaNmod object
#' @export
#'
#' @examples
#' myCaNmod <- build_CaNmod(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' toggle_constraint(myCaNmod, "C02")
#'
#' @importFrom Matrix rbind2



toggle_constraint <- function(myCaNmod, constr){
  allconstraints <- c(rownames(myCaNmod$AAll),
                      rownames(myCaNmod$CAll))
  allconstraintsactive <- c(rownames(myCaNmod$A),
                            rownames(myCaNmod$C))

  for (co in constr){
    if (sum(startsWith(allconstraints, co)) == 0)
      stop(paste("no match for constraint", co))
  }

  #we make a loop over constraint
  for (co in constr){
    suballA <- rownames(myCaNmod$AAll)[startsWith(rownames(myCaNmod$AAll), co)]
    subactiveA <- rownames(myCaNmod$A)[startsWith(rownames(myCaNmod$A), co)]
    inactiveA <- suballA[!suballA %in% subactiveA]

    suballC <- rownames(myCaNmod$CAll)[startsWith(rownames(myCaNmod$CAll), co)]
    subactiveC <- rownames(myCaNmod$C)[startsWith(rownames(myCaNmod$C), co)]
    inactiveC <- suballC[!suballC %in% subactiveC]

    if (length(suballA) > 0){
      if (length(subactiveA) > 0){
        myCaNmod$A <- myCaNmod$A[!rownames(myCaNmod$A) %in% subactiveA, ]
        myCaNmod$b <- myCaNmod$b[!rownames(myCaNmod$A) %in% subactiveA]
        print(paste("disactivate inequality", subactiveA))
      }
      if (length(inactiveA) > 0){
        myCaNmod$A <- rbind2(myCaNmod$A,
                             myCaNmod$AAll[inactiveA, ])
        myCaNmod$b <- c(myCaNmod$b,
                        myCaNmod$bAll[rownames(myCaNmod$AAll) %in% inactiveA])
        print(paste("activate inequality", inactiveA))
      }
    }
    if (length(suballC) > 0){
      if (length(subactiveC) > 0){
        myCaNmod$C <- myCaNmod$C[!rownames(myCaNmod$C) %in% subactiveC, ]
        myCaNmod$v <- myCaNmod$v[!rownames(myCaNmod$C) %in% subactiveC]
        print(paste("disactivate equality", subactiveC))
      }
      if (length(inactiveC) > 0){
        myCaNmod$C <- rbind2(myCaNmod$C,
                             myCaNmod$CAll[inactiveC, ])
        myCaNmod$v <- c(myCaNmod$v,
                        myCaNmod$vAll[rownames(myCaNmod$CAll) %in% inactiveC])
        print(paste("activate equality", inactiveC))
      }
    }
  }
  return (myCaNmod)
}
