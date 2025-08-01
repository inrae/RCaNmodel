#' ident_tol
#' compares x and y and returns TRUE or FALSE if x and y are equal given
#' tolerance
#'
#' @param x object 1 
#' @param y object 2
#' @param tolerance numerical toleance
#'
#' @return a logical
#' 
#' @importFrom tibble tibble is_tibble
#' @export
#' 
#' @examples
#' require(dplyr)
#' ident_tol(tibble(iris),
#'     tibble(iris |>
#'       mutate(Petal.Length = Petal.Length + .1)),
#'          tolerance = 0.2)
#'          
#' ident_tol(tibble(iris),
#'     tibble(iris |>
#'       mutate(Petal.Length = Petal.Length + .1)),
#'          tolerance = 0.01)
ident_tol <- function(x, y, tolerance = 0.001){
  if (is_tibble(x) | is_tibble(y)){
    x <- tibble(x)
    y <- tibble(y)
  }
  isTRUE(all.equal(x, y, tolerance = tolerance, scale = 1))
}