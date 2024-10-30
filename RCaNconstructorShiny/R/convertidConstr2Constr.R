#' convertidConstr2Constr
#'
#' @param constraint the constraint with id
#' @param dictionary  the dictionary
#'
#' @return a formula in dictionary
#' @export
#'
convertidConstr2Constr <- function(constraint, dictionary){
  constraints_word <- getConstraintWord(constraint)
  for (i in seq_along(constraints_word)){
    word <- constraints_word[i]
    word2 <- gsub("^Before", "", word)
    word2 <- gsub("^After", "", word)
    word2 <- gsub("^Ratio", "", word)
    word2 <- gsub("^Delta", "", word)
    if (word2 %in% names(dictionary)){
      word <- gsub(word2,
                   dictionary[word2],
                   word,
                   fixed = TRUE)
      constraints_word[i] <- word
    }

  }

  paste(constraints_word, collapse = "")

}
