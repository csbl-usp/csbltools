#' Collapse redundant probes into gene symbols
#'
#' This function collapses redundant probe expression rows into single rows
#' based on repeated gene symbols in a given column. 
#' 
#' @param expr Object of class \code{data.frame} containing expression values,
#' a column with probe symbols and a column with gene symbols
#' @param probe_col Character string specifying the name of the column containing 
#' probe names. Can also take rownames as input using the string "rownames".
#' @param gene_col Character string specifying the name of the column containing 
#' gene symbols to be used to collapse. 
#' @param data_table Logical. If \code{TRUE} will return a \code{data.table} object.
#' Defaults to \code{FALSE}. 
#' @param method Method used to collapse the probes. Can take one of "max_mean", 
#' "min_mean", "col_mean" or "col_median".
#'
#' @return Object of class \code{data.frame} or \code{data.table}
#' 
#' @examples

#' # Create mock probe expression data
#' mock <- CEMiTool::expr
#' mock$Symbol <- rownames(mock)
#' rownames(mock) <- NULL
#' mock <- mock[1:(nrow(mock)/2),]
#' mock2 <- mock[,names(mock) != "Symbol"]
#' mock2 <- mock2 * 2
#' mock2$Symbol <- mock$Symbol
#' mock <- rbind(mock, mock2)
#' mock <- mock[order(mock$Symbol), ]
#' rownames(mock) <- paste0("probe_", rownames(mock))

#' # Collapse redundant probes
#' expr_col <- collapse_rows(mock, probe_col="rownames", gene_col="Symbol", method="max_mean")

collapse_rows <- function(expr, probe_col, gene_col, data_table=F, method=c("max_mean", "min_mean", "col_mean", "col_median")){
    if(length(grep('data.table', installed.packages())) == 0){
        install.packages('data.table')
        require(data.table)
    }else if(length(grep('data.table', search())) == 0){
        suppressPackageStartupMessages(require(data.table))
    }
    
    if (probe_col == "rownames"){
        expr <- data.table(expr, keep.rownames=T)
        setnames(expr, "rn", "rownames")
    }else{
        expr <- data.table(expr)
    }
    
    if(method=="max_mean" | method=="min_mean"){ 
        expr[, rowmean := rowMeans(.SD[, !c(probe_col, gene_col), with=F])]
        if(method=="max_mean"){
            res <- expr[order(rowmean, decreasing=T)][, .SD[1], by=gene_col][, rowmean:=NULL]
        }
        else if(method=="min_mean"){
            res <- expr[order(rowmean, decreasing=T)][, .SD[.N], by=gene_col][, rowmean:=NULL]
        }
    }
    else if(method=="col_mean"){
        res <- expr[, lapply(.SD[, !c(probe_col), with=F], mean), by=gene_col]
    }
    else if(method=="col_median"){
        res <- expr[, lapply(.SD[, !c(probe_col), with=F], median), by=gene_col]   
    }
    else stop("method must be 'max_mean', 'min_mean', 'col_mean' or 'col_median'\n")
    
    if(!data_table){
        return(data.frame(res))
    }else{ return(res[]) }
}
