#'  search_multiple_names
#'
#'  Wrapper to search match in a R vector for many words
#'
#' @param vector_of_interest The vector that you want to perform a search in.
#' @param query_vector A vector of the words that you want to match (at least one of them)
#' @param perl Logical (TRUE or FALSE). Indicates whether perl-compatible regex will be used.
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


search_multiple_names <- function(vector_of_interest, query_vector, ...) {
  return(
    grep(pattern = paste(query_vector, collapse = "|"), x = vector_of_interest, perl = perl, value = !return_index)
  )
}
