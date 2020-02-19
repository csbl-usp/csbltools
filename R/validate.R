#' @import yaml

#' Validate input data
#'
#' This function validates input data based on formats rules.
#' 
#' @param input Input to be validated.
#' @param type Input type. 
#'
#' @return TRUE if input follows its type rules FALSE otherwise
#' 
#' @examples
#' 
#' @rdname validate 
#' @export

validate <- function(input, type=c("expression", "annotation", "degs")){
    if (missing(input)) { stop('Must provide input data') }
    if (length(type) > 1) { stop('Must choose data type') }

    rules <- yaml::yaml.load_file(system.file('formats',
                                              paste0(type,'.yml'),
                                              package='csbltools'))

    for (column in rules$columns) {
        if (column$required) {
            # checks if column exists
            if ( !column$name %in% colnames(input) ) {
                stop(sprintf('There is no column named %s', column$name))
            } else {
                # checks column type
                if ( class(input[, column$name]) != column$type ) {
                    stop(sprintf('The column %s is of type %s instead of %s', 
                                 column$name,
                                 class(input[, column$name]),
                                 column$type))
                }
            }
        } 
    }
}
