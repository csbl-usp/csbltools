#'  search_multiple_names
#'
#'  Wrapper to search match in a R vector for many words
#'
#' @param vector_of_interest The vector that you want to perform a search in.
#' @param query_vector A vector of the words that you want to match (at least one of them).
#' Defaults to \code{FALSE}.
#' @param return_index Logical (TRUE or FALSE). Indicates whether the search will return indexes or the values themselves in the vector.
#' Defaults to false (thus values are returned).
#' @examples
#' # Vector contains data to be filtered my search vector
#' csblers <- c("Andre Nicolau", "Andre Guilherme", "Deney Muleque Doido", "Thiago Hirata", "Tiago Lubiana")
#' # Vector contains term to find in vector of interest
#' query <- c("Thiago", "Tiago")
#' # Function command to find one or more values in other vector
#' search_multiple_names(vector_of_interest = csblers, query_vector = query)
#' search_multiple_names(vector_of_interest = csblers, query_vector = query, return_index = TRUE)


search_multiple_names <- function(vector_of_interest, query_vector, return_index = FALSE, ...) {
  query_string <- paste(query_vector, collapse = "|")
  return(
    grep(pattern = query_string, x = vector_of_interest, perl = TRUE, value = !return_index, ...)
  )
}
