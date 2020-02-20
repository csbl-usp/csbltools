#'  search_multiple_names
#'
#'  Wrapper to search match in a R vector for many words
#'
#' @param vector_of_interest The vector that you want to perform a search in.
#' @param query_vector A vector of the words that you want to match (at least one of them).
#' @param logical_operator A string of logical operator to define search criteria to query_vector.
#' Defaults to \code{FALSE}.
#' @param return_index Logical (TRUE or FALSE). Indicates whether the search will return indexes or the values themselves in the vector.
#' Defaults to false (thus values are returned).
#' @examples
#' # Vector de interesse para realizar a busca
#' csblers <- c("Andre Nicolau", "Andre Guilherme", "Deney Muleque Doido", "Thiago Hirata", "Tiago Lubiana")
#' # Vector contendo os valores que serão buscados
#' query <- c("Thiago", "Tiago")
#' # Comando grep para buscar os valores de interesse
#' search_multiple_names(vector_of_interest, query_vector = query)
#' search_multiple_names(vector_of_interest, query_vector = query, return_index = TRUE)


search_multiple_names <- function(vector_of_interest, query_vector, logical_operator = c("OR", "AND"), return_index = FALSE, ...) {
  if(logical_operator == "OR") {
    query_string <- paste(query_vector, collapse = "|")
  } else if(logical_operator == "AND") {
    query_string <- paste(query_vector, collapse = "&")
  }
  return(
    grep(pattern = query_string, x = vector_of_interest, perl = TRUE, value = !return_index)
  )
}
