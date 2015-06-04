tdir <- system.file("testfiles",package="inlinedocs")
testfiles <- Sys.glob(file.path(tdir,"*.R"))
library(inlinedocs)
options(warn=2)
library(parallel)
options(mc.cores=detectCores())
mclapply(testfiles, test.file, verbose=FALSE)

