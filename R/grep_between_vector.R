# Function to search vector patterns in other interest vector
grep_vectors <- function(x, search, perl = TRUE, value = TRUE) {
  return(
    grep(pattern = paste(search, collapse = "|"), x = x, perl = perl, value = value)
  )
}
