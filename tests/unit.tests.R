tdir <- system.file("testfiles",package="inlinedocs")
testfiles <- Sys.glob(file.path(tdir,"*.R"))
options(warn=2)
output.and.error <- function(f){
  print(f)
  output <- capture.output({
    result <- try(inlinedocs::test.file(
      f, CRAN.checks=CRAN.checks, verbose=TRUE))
  })
  list(output=output,
       result=result)
}
LAPPLY <- if(interactive() && requireNamespace("future.apply")){
  future::plan("multisession")
  future.apply::future_lapply
}else{
  lapply
}

CRAN.checks <- interactive() #need here because of parallel execution.
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
