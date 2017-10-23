#' Create a CSBL project directory with the standard structure
#'
#' This function will create a directory with the standard folders for
#' a project in the CSBL: data, intermediate, results and src.
#'
#' @param directory The path into which the folders should be created.
#' @param force If directory exists, processing will not be stopped.
#'
#' @return An appropriate file environment for CSBL projects
#'
#' @examples
#' # Create project file structure
#' \dontrun{
#' create_project(directory <- "~/test", force=FALSE)
#' }
#'
#' @rdname create_project
#' @export

create_project <- function(directory, force=FALSE){
	if(dir.exists(directory)){
	    if(!force){
			stop("The folder ", directory, " already exists! Use force=TRUE to overwrite.")
        }
    }else{
		dir.create(directory)
	}
	dir.create(file.path(directory, "data"))
	dir.create(file.path(directory, "intermediate"))
	dir.create(file.path(directory, "results"))
	dir.create(file.path(directory, "src"))
}
