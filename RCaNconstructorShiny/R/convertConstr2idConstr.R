#' convertConstr2idConstr
#'
#' @param constraint the constraint
#' @param dictionary  the dictionary
#'
#' @return a formula in id wording
#' @export
#'
convertConstr2idConstr <- function(constraint, dictionary){
  constraints_word <- getConstraintWord(constraint)

  for (i in seq_along(constraints_word)){
    word <- constraints_word[i]
    word2 <- gsub("^Before", "", word)
    word2 <- gsub("^After", "", word2)
    word2 <- gsub("^Ratio", "", word2)
    word2 <- gsub("^Delta", "", word2)
    if (word2 %in% dictionary){
      word <- gsub(word2,
                   paste0('{',
                          names(dictionary)[dictionary == word2],
                          '}'),
                   word,
                   fixed = TRUE)
      constraints_word[i] <- word
    }

  }

  paste(constraints_word, collapse = "")

}
