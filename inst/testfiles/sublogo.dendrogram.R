read.fasta <- function
### Read sequences in FASTA format into a named character vector
(infile
### Name of the sequence file
 ){
  infile
}
##debug(read.fasta)

### Letters in the DNA alphabet, used to auto-detect sequence type
dna.letters <- c("*","A","T","G","C")
### DNA identity substitution matrix.
dna.identity <- matrix(0,nrow=length(dna.letters),ncol=length(dna.letters),
                       dimnames=list(dna.letters,dna.letters))
diag(dna.identity) <- 1
dna.identity['*','*'] <- 0
seqs.to.mat <- function
### Calculate pairwise differences between sequences using a
### substitution matrix.
(seq.vec,
### DNA or protein sequences.
 subs.mat=NULL){
### Substitution matrix with dimnames that match the letters used in
### the sequence data, or a character vector that specifies a common
### substitution matrix (as defined in the Biostrings package). NULL
### specifies that we will guess a suitable substitution matrix to
### match your input sequences (DNA=>identity, protein=>BLOSUM62).
  if(is.null(names(seq.vec)))names(seq.vec) <- seq.vec
  chars <- sapply(seq.vec,nchar)
  seqsum <- table(chars)
  chars
### The matrix of distances between each input sequence, with dimnames
### corresponding to either the sequences, or the sequence names (if
### they exist)
}
##debug(seqs.to.mat)

make.logo.ps <- function
### Create a logo using weblogo, then read it in using grImport
(helices,
### Sequences to plot in the logo
 psbase
### Base filename for the logo postscript and xml files, should be the
### full path name except for the trailing .ps
 ){
  psfile <- paste(psbase,'ps',sep='.')
  helices
### Grid picture grob as read using readPicture
}
##debug(make.logo.ps)

sublogo.dendrogram <- function
### Main function for drawing sublogo dendrograms.
(M,
### difference matrix as constructed by seqs.to.mat (although in
### principle any object with a valid as.dist method could be used)
 main='',
### plot title
 subtit=NULL,
### plot subtitle
 base=tempfile(),
### base file name for temporary logo files
 cutline=150,
### Distance for cutting the tree. Draw a sublogo for each
### leaf. Normally you will plot once, then inspect the dendrogram to
### determine which is a good value for cutline, then plot again using
### your chosen cutline.
 dend.width=30,
### Percent of the plot to be occupied by the dendrogram. The logos
### will be displayed with equal widths on either side.
 cex=1
### character expansion factor for the dendrogram
 ){
  hc <- hclust(as.dist(M),method="average")
  hc
### The dendrogram from the call to hclust
}
##debug(sublogo.dendrogram)

sublogo <- function
### Draw a sublogo dendrogram for a sequence motif.
(seqs,
### Character vector of DNA or protein sequences (optionally named
### with sequence labels).
 mat=NULL,
### Substitution matrix passed to seqs.to.mat.
 ...
### Other arguments to pass to sublogo.dendrogram (see that help page
### for a full description).
 ){
  sublogo.dendrogram(seqs.to.mat(seqs,mat),...)
}

.result <- list(
  dna.identity = list(
    description = "DNA identity substitution matrix.", format = "",
    title = "dna identity"),
  dna.letters = list(
    description = "Letters in the DNA alphabet, used to auto-detect sequence type",
    format = "", title = "dna letters"),
  make.logo.ps = list(
    description = "Create a logo using weblogo, then read it in using grImport",
    `item{helices}` = "Sequences to plot in the logo", `item{psbase}` = "Base filename for the logo postscript and xml files, should be the\nfull path name except for the trailing .ps",
    value = "Grid picture grob as read using readPicture", format = "",
    title = "make logo ps"),
  read.fasta = list(
    description = "Read sequences in FASTA format into a named character vector",
    `item{infile}` = "Name of the sequence file", format = "",
    title = "read fasta"),
  seqs.to.mat = list(
    description = "Calculate pairwise differences between sequences using a\nsubstitution matrix.",
    `item{seq.vec}` = "DNA or protein sequences.", `item{subs.mat}` = "Substitution matrix with dimnames that match the letters used in\nthe sequence data, or a character vector that specifies a common\nsubstitution matrix (as defined in the Biostrings package). NULL\nspecifies that we will guess a suitable substitution matrix to\nmatch your input sequences (DNA=>identity, protein=>BLOSUM62).",
    value = "The matrix of distances between each input sequence, with dimnames\ncorresponding to either the sequences, or the sequence names (if\nthey exist)",
    format = "", title = "seqs to mat"),
  sublogo = list(
    description = "Draw a sublogo dendrogram for a sequence motif.",
    `item{seqs}` = "Character vector of DNA or protein sequences (optionally named\nwith sequence labels).",
    `item{mat}` = "Substitution matrix passed to \\code{\\link{seqs.to.mat}}.",
    `item{\\dots}` = "Other arguments to pass to \\code{\\link{sublogo.dendrogram}} (see that help page\nfor a full description).",
    format = "", title = "sublogo"),
  sublogo.dendrogram = list(
    description = "Main function for drawing \\code{\\link{sublogo}} dendrograms.",
    `item{M}` = "difference matrix as constructed by \\code{\\link{seqs.to.mat}} (although in\nprinciple any object with a valid as.dist method could be used)",
    `item{main}` = "plot title", `item{subtit}` = "plot subtitle",
    `item{base}` = "\\code{base} file name for temporary logo files",
    `item{cutline}` = "Distance for cutting the tree. Draw a \\code{\\link{sublogo}} for each\nleaf. Normally you will plot once, then inspect the dendrogram to\ndetermine which is a good value for \\code{cutline}, then plot again using\nyour chosen \\code{cutline}.",
    `item{dend.width}` = "Percent of the plot to be occupied by the dendrogram. The logos\nwill be displayed with equal widths on either side.",
    `item{cex}` = "character expansion factor for the dendrogram",
    value = "The dendrogram from the call to hclust", format = "",
    title = "sublogo dendrogram"))
