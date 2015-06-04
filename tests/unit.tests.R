tdir <- system.file("testfiles",package="inlinedocs")
testfiles <- Sys.glob(file.path(tdir,"*.R"))
library(inlinedocs)
options(warn=2)
output.and.error <- function(f){
  output <- capture.output({
    result <- try(test.file(f, verbose=FALSE))
  })
  list(output=output,
       result=result)
}
LAPPLY <- if(interactive() && require(parallel)){
  options(mc.cores=detectCores())
  mclapply
}else{
  lapply
}
result.lists <- LAPPLY(testfiles, output.and.error)
names(result.lists) <- testfiles
is.error <- sapply(result.lists, function(L)inherits(L$result, "try-error"))
failed <- result.lists[is.error]
if(length(failed)){
  for(f in names(failed)){
    output <- failed[[f]]$output
    cat("\n\n", f, "\n")
    cat(paste(output, collapse="\n"))
  }
  stop("test failures")
}
