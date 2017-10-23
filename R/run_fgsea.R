#' @import fgsea
#' @import clusterProfiler
NULL

#' Run fgsea
#'
#' This function takes an input ranks and gmt file and runs
#' gene set enrichment analysis via the fgsea package. Output files
#' can also be generated with write=TRUE, and results will be filtered with
#' filter=TRUE
#'
#' @param ranks Path to a file containing gene (rows) ranks for different
#' classes (columns).
#' @param gmt Path to a gmt pathways file
#' @param filter Whether or not to remove ranks without enrichment (default=TRUE).
#' @param directory Directory into which output files should be written (default="./Results").
#' @param force If TRUE, \code{directory} will be overwritten (default=FALSE).
#'
#' @return Enrichment tables
#' @rdname run_fgsea
#'
#' @export

run_fgsea <- function(ranks, gmt, filter=TRUE, directory="./Results", force=FALSE){

    # Open and split gmt
    gmt <- clusterProfiler::read.gmt(gmt)
    gmt <- split(gmt$gene, gmt$ont)

    # Open and prepare ranks
    ranks_df <- read.delim(ranks)
    ranks_ch <- colnames(ranks_df)[-1]
    ranks_ch <- setNames(ranks_ch, ranks_ch)

    # Select data frames and run fastGSEA
    tmp_ranks <- lapply(ranks_ch, function(rankname){
      tmpdf <- ranks_df[,c('gene', rankname)]
      tmpdf <- tmpdf[complete.cases(tmpdf),]
      tmpranks <- tmpdf[[rankname]]
      names(tmpranks) <- tmpdf$gene
      fgseaRes <- fgsea::fgsea(pathways = gmt, stats = tmpranks,
                               minSize = 15, maxSize = 500, nperm = 1000)
      fgseaRes <- as.data.frame(fgseaRes)
      fgseaRes <- fgseaRes[,c('pathway', 'padj', 'NES')]
      fgseaRes
    })

    if(filter){
        # Remove ranks without enrichment
        tmp_ranks <- Filter(function(x) nrow(x) > 1, tmp_ranks)
    }

    # Write output
    if(dir.exists(directory)){
        if(!force){
            stop("Stopping analysis: ", directory, " already exists! Use force=TRUE to overwrite.")
        }
    }else{
        dir.create(directory, recursive=TRUE)
    }
    for(name_rank in names(tmp_ranks)){
      rank_out <- tmp_ranks[[name_rank]]
      rank_nameout <- paste0('fgsea_', name_rank, '.tsv')
      write.table(rank_out, file = file.path(directory, rank_nameout),
                  sep = '\t', quote = F, row.names = F)
    }
}
