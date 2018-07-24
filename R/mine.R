connect_geo <- function() {
    suppressPackageStartupMessages(require(GEOmetadb))
    geodb <- sprintf('%s/GEOmetadb/GEOmetadb.sqlite', .libPaths()[1])
    dbConnect(SQLite(), geodb)
}
#geo_con <- connect_geo()

connect_sra <- function() {
    suppressPackageStartupMessages(require(SRAdb))
    sradb <- sprintf('%s/SRAdb/SRAmetadb.sqlite', .libPaths()[1])
    dbConnect(SQLite(), sradb)
}
#sra_con <- connect_sra()

#PLATFORM_TECHNOLOGIES <- unique(dbGetQuery(geo_con, 'SELECT technology FROM gpl')[[1]])
PLATFORM_TECHNOLOGIES <- c(
    'antibody',
    'high-throughput sequencing',
    'in situ oligonucleotide',
    'mixed spotted oligonucleotide/cDNA',
    'MPSS',
    'MS',
    'oligonucleotide beads',
    'other',
    'RT-PCR',
    'SAGE NlaIII',
    'SAGE RsaI',
    'SAGE Sau3A',
    'SARST',
    'spotted DNA/cDNA',
    'spotted oligonucleotide',
    'spotted peptide or protein',
    'tissue',
    NA
)

#EXPERIMENT_TYPES <- unique(unlist(strsplit(dbGetQuery(geo_con, 'SELECT type FROM gse')[[1]], ';\t')))
EXPERIMENT_TYPES <- c(
    'Expression profiling by array',
    'Expression profiling by genome tiling array',
    'Expression profiling by high throughput sequencing',
    'Expression profiling by MPSS',
    'Expression profiling by RT-PCR',
    'Expression profiling by SAGE',
    'Expression profiling by SNP array',
    'Genome binding/occupancy profiling by array',
    'Genome binding/occupancy profiling by genome tiling array',
    'Genome binding/occupancy profiling by high throughput sequen',
    'Genome binding/occupancy profiling by high throughput sequencing',
    'Genome binding/occupancy profiling by SNP array',
    'Genome variation profiling by array',
    'Genome variation profiling by genome tiling array',
    'Genome variation profiling by high throughput sequencing',
    'Genome variation profiling by SNP array',
    'Methylation profiling by ',
    'Methylation profiling by array',
    'Methylation profiling by genome tiling array',
    'Methylation profiling by high throughput sequencing',
    'Methylation profiling by SNP array',
    'Non-coding RNA profiling by array',
    'Non-coding RNA profiling by genome tiling array',
    'Non-coding RNA profiling by high throughput sequencing',
    'Other',
    'Protein profiling by Mass Spec',
    'Protein profiling by protein array',
    'SNP genotyping by SNP array',
    'Third-party reanalysis'
)
