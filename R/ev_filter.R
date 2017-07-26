#' @importFrom stats median sd
NULL

#' Expression and variance filtering
#' 
#' This function allows you to filter a gene expression dataset
#' by the number of genes you want in the resulting dataset. 
#'
#' @param expr Object of class \code{data.frame} containing gene expression data.
#' genes must be in the rows, samples in the columns, and any identifiers must be 
#' in the rownames.
#' @param genenum Number of genes to keep in the resulting object.
#' @param prop Proportion of most variant genes to keep in the resulting object.
#' @return Object of class \code{data.frame} with filtered expression values
#' @examples
#' # Get example expression data
#' expr <- CEMiTool::expr
#' # Select 60% most expressed and 2000 most variant genes
#' expr_f <- ev_filter(expr, 2000, 0.6)
#'
#' @rdname ev_filter 
#' @export

ev_filter <- function(expr, genenum, prop){
    if(floor(nrow(expr)*prop) > genenum){
        y <- filter_prop(expr, prop, fun=mean)
    }else{
        y <- expr
    }
    res <- filter_rows(y, genenum, fun=sd)
    return(res)
}

filter_prop <- function(expr, prop, fun=mean){
    rows <- floor(prop * nrow(expr))
    return(filter_rows(expr, rows, fun))
}

filter_rows <- function(expr, rows, fun=sd){
    rows <- min(rows, nrow(expr))
    val <- apply(expr, 1, fun)
    sel_rows <- order(val, decreasing=TRUE)[1:rows]
    return(expr[sel_rows, ])
}
