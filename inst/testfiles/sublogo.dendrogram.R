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

.result <- 
 list(dna.identity = list(definition = "dna.identity <- matrix(0,nrow=length(dna.letters),ncol=length(dna.letters),\n                       dimnames=list(dna.letters,dna.letters))",  
     description = "DNA identity substitution matrix.", format = "",  
     title = "dna identity"), dna.letters = list(definition = "dna.letters <- c(\"*\",\"A\",\"T\",\"G\",\"C\")",  
     description = "Letters in the DNA alphabet, used to auto-detect sequence type",  
     format = "", title = "dna letters"), make.logo.ps = list( 
     definition = "make.logo.ps <- function\n### Create a logo using weblogo, then read it in using grImport\n(helices,\n### Sequences to plot in the logo\n psbase\n### Base filename for the logo postscript and xml files, should be the\n### full path name except for the trailing .ps\n ){\n  psfile <- paste(psbase,'ps',sep='.')\n  helices\n### Grid picture grob as read using readPicture\n}",  
     description = "Create a logo using weblogo, then read it in using grImport",  
     `item{helices}` = "Sequences to plot in the logo", `item{psbase}` = "Base filename for the logo postscript and xml files, should be the\nfull path name except for the trailing .ps",  
     value = "Grid picture grob as read using readPicture", format = "",  
     title = "make logo ps"), read.fasta = list(definition = "read.fasta <- function\n### Read sequences in FASTA format into a named character vector\n(infile\n### Name of the sequence file\n ){\n  infile\n}",  
     description = "Read sequences in FASTA format into a named character vector",  
     `item{infile}` = "Name of the sequence file", format = "",  
     title = "read fasta"), seqs.to.mat = list(definition = "seqs.to.mat <- function\n### Calculate pairwise differences between sequences using a\n### substitution matrix.\n(seq.vec,\n### DNA or protein sequences.\n subs.mat=NULL){\n### Substitution matrix with dimnames that match the letters used in\n### the sequence data, or a character vector that specifies a common\n### substitution matrix (as defined in the Biostrings package). NULL\n### specifies that we will guess a suitable substitution matrix to\n### match your input sequences (DNA=>identity, protein=>BLOSUM62).\n  if(is.null(names(seq.vec)))names(seq.vec) <- seq.vec\n  chars <- sapply(seq.vec,nchar)\n  seqsum <- table(chars)\n  chars\n### The matrix of distances between each input sequence, with dimnames\n### corresponding to either the sequences, or the sequence names (if\n### they exist)\n}",  
     description = "Calculate pairwise differences between sequences using a\nsubstitution matrix.",  
     `item{seq.vec}` = "DNA or protein sequences.", `item{subs.mat}` = "Substitution matrix with dimnames that match the letters used in\nthe sequence data, or a character vector that specifies a common\nsubstitution matrix (as defined in the Biostrings package). NULL\nspecifies that we will guess a suitable substitution matrix to\nmatch your input sequences (DNA=>identity, protein=>BLOSUM62).",  
     value = "The matrix of distances between each input sequence, with dimnames\ncorresponding to either the sequences, or the sequence names (if\nthey exist)",  
     format = "", title = "seqs to mat"), sublogo = list(definition = "sublogo <- function\n### Draw a sublogo dendrogram for a sequence motif.\n(seqs,\n### Character vector of DNA or protein sequences (optionally named\n### with sequence labels).\n mat=NULL,\n### Substitution matrix passed to seqs.to.mat.\n ...\n### Other arguments to pass to sublogo.dendrogram (see that help page\n### for a full description).\n ){\n  sublogo.dendrogram(seqs.to.mat(seqs,mat),...)\n}",  
     description = "Draw a sublogo dendrogram for a sequence motif.",  
     `item{seqs}` = "Character vector of DNA or protein sequences (optionally named\nwith sequence labels).",  
     `item{mat}` = "Substitution matrix passed to seqs.to.mat.",  
     `item{\\dots}` = "Other arguments to pass to sublogo.dendrogram (see that help page\nfor a full description).",  
     format = "", title = "sublogo"), sublogo.dendrogram = list( 
     definition = "sublogo.dendrogram <- function\n### Main function for drawing sublogo dendrograms.\n(M,\n### difference matrix as constructed by seqs.to.mat (although in\n### principle any object with a valid as.dist method could be used)\n main='',\n### plot title\n subtit=NULL,\n### plot subtitle\n base=tempfile(),\n### base file name for temporary logo files\n cutline=150,\n### Distance for cutting the tree. Draw a sublogo for each\n### leaf. Normally you will plot once, then inspect the dendrogram to\n### determine which is a good value for cutline, then plot again using\n### your chosen cutline.\n dend.width=30,\n### Percent of the plot to be occupied by the dendrogram. The logos\n### will be displayed with equal widths on either side.\n cex=1\n### character expansion factor for the dendrogram\n ){\n  hc <- hclust(as.dist(M),method=\"average\")\n  hc\n### The dendrogram from the call to hclust\n}",  
     description = "Main function for drawing sublogo dendrograms.",  
     `item{M}` = "difference matrix as constructed by seqs.to.mat (although in\nprinciple any object with a valid as.dist method could be used)",  
     `item{main}` = "plot title", `item{subtit}` = "plot subtitle",  
     `item{base}` = "base file name for temporary logo files",  
     `item{cutline}` = "Distance for cutting the tree. Draw a sublogo for each\nleaf. Normally you will plot once, then inspect the dendrogram to\ndetermine which is a good value for cutline, then plot again using\nyour chosen cutline.",  
     `item{dend.width}` = "Percent of the plot to be occupied by the dendrogram. The logos\nwill be displayed with equal widths on either side.",  
     `item{cex}` = "character expansion factor for the dendrogram",  
     value = "The dendrogram from the call to hclust", format = "",  
     title = "sublogo dendrogram")) 
