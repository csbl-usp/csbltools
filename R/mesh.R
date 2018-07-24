library(RSQLite)
meshdb <- dbConnect(SQLite(), sprintf('%s/MeSH.db/extdata/MeSH.db.sqlite', .libPaths()[1]))
mesh <- dbGetQuery(meshdb, 'SELECT DISTINCT MESHTERM,SYNONYM FROM DATA')
dbDisconnect(meshdb)

unstack_text <- function(x) {
    many_commas <- grepl(', .*, ', x)
    # fast method, for zero or one level of stacking
    x[!many_commas] <- sub('(.*), (.*)', '\\2 \\1', x[!many_commas])
    # slow method, for two or more levels of stacking
    x[many_commas] <- x[many_commas] %>%
        strsplit(', ') %>%
        lapply(rev) %>%
        sapply(paste, collapse=' ')
    x
}

mesh <- unique(data.frame(
    term=unstack_text(mesh$MESHTERM),
    synonym=unstack_text(sub('[|].*', '', mesh$SYNONYM))
))
mesh <- unique(rbind(unique(mesh$term) %>% data.frame(term=., synonym=.), mesh))

mesh_term <- function(x) {
    matching <- grepl(x, mesh$synonym, ignore.case=TRUE)
    unique(mesh[matching, 'term'])
}

mesh_synonyms <- function(x, drop_plurals=TRUE) {
    term <- mesh_term(sprintf('^%s$', x))
    ret <- mesh[mesh$term==term, 'synonym']
    if (drop_plurals)
        ret <- setdiff(ret, paste0(ret, 's'))
    ret
}
