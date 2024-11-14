#' filter_with_rownames
#'
#' @param data the data ti filter
#' @param ... argument sent to filter
#'
#' @return filtered data with kept rownames
#' @export
#' 
#' @examples
#' filter_with_rownames(iris, Species == "versicolor")
#'
filter_with_rownames <- function(data, ...){
  data %>%
           tibble::rownames_to_column("rowname") %>%
    dplyr::filter(...) %>%
    tibble::column_to_rownames("rowname")
}
