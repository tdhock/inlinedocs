test.file <- function
### Check an R code file with inlinedocs to see if the
### extract.docs.file parser accurately extracts all the code inside!
### The code file should contain a variable .result which is the
### documentation list that you should get when you apply
### extract.docs.file to the file. We check for identity of elements
### of elements of the list, so the order of elements should not
### matter, and thus this should be a good robust unit test.
(f,
### File name of R code file with inlinedocs to parse and check.
  CRAN.checks=TRUE,
### try to make a package and run CRAN checks?
  verbose=FALSE
### Show output?
){
  ##seealso<< \code{\link{save.test.result}}
  e <- new.env()
  suppressWarnings(sys.source(f,e))
  ## these are the items to check for, in no particular order
  .result <- e$.result
  parsers <- e$.parsers
  result <- extract.docs.file(f, parsers, verbose=verbose)
  for(FUN in names(.result)){
    fun <- result[[FUN]]
    .fun <- .result[[FUN]]
    ## first check to make sure all the stored items are there
    for(N in names(.fun)){
      .res <- .fun[[N]]
      res <- fun[[N]]
      if(is.null(res) || is.na(res) || is.na(.res) || .res!=res){
        cat(
          "\n-----\n",res,"\n-----\nin ",FUN,
          "$",N,", expected:\n-----\n",.res,"\n-----\n",
          sep="")
        stop("docs mismatch in ",f)
      }
    }
    ## now check and see if there are no additional items!
    additional <- !names(fun)%in%names(.fun)
    show <- fun[additional] ##ignore NULL extracted items
    show <- show[!sapply(show,is.null)]
    not.def <- show[names(show) != "definition"]
    if(length(not.def)){
      cat("\n")
      print(not.def)
      stop("extracted some unexpected docs!")
    }
  }
  ## make sure there are no unexpected outer lists
  not.expected <- names(result)[!names(result)%in%names(.result)]
  if(length(not.expected)){
    print(not.expected)
    stop("extracted some unexpected documentation objects!")
  }
  ## finally make a package using this file and see if it passes
  ## without warnings TDH 27 May 2011 added !interactive() since
  ## recursive calling R CMD check seems to systematically
  ## fail... ERROR: startup.Rs not found. This file is usually copied
  ## to the check directory and read as a .Rprofile, as done in
  ## tools:::.runPackageTests ... is this a bug in R? Anyway for now
  ## let's just not run the R CMD check.
  if(CRAN.checks && is.null(e$.dontcheck)){
    make.package.and.check(f,parsers,verbose)
  }
  if(verbose)cat("\n")
}

make.package.and.check <- function
### Assemble some R code into a package and process it using R CMD
### check, stopping with an error if the check resulted in any errors
### or warnings.
(f, ##<< R code file name from which we will make a package
 parsers=default.parsers,
### Parsers to use to make the package documentation.
 verbose=TRUE
### print the check command line?
 ){
  pkgname <- sub("[.][rR]$","",basename(f))
  pkgdir <- file.path(tempdir(),pkgname)
  if(file.exists(pkgdir))unlink(pkgdir,recursive=TRUE)
  rdir <- file.path(pkgdir,"R")
  if(verbose)print(rdir)
  dir.create(rdir,recursive=TRUE)
  sillydir <- system.file("silly",package="inlinedocs")
  tocopy <- file.path(sillydir,c("DESCRIPTION","NAMESPACE"))
  file.copy(tocopy,pkgdir)
  f.lines.in <- readLines(f)
  f.lines.out <- grep("^[.]parsers", f.lines.in, invert=TRUE, value=TRUE)
  writeLines(f.lines.out, file.path(rdir, "code.R"))
  package.skeleton.dx(pkgdir,parsers)
  cmd <- sprintf("%s CMD check --as-cran %s",
                 file.path(R.home("bin"), "R"),
                 pkgdir)
  if(verbose)cat(cmd,"\n")
  checkLines <- system(cmd, intern=TRUE, ignore.stderr=!verbose)
  all.warnLines <- grep("WARNING|ERROR|NOTE",checkLines,value=TRUE)
  ignore.lines <- c( # false positives.
    ##Status: 1 WARNING, 2 NOTEs
    "Status",
    ##* checking R code for possible problems ... NOTE
    "possible problems",
    ##* checking for code which exercises the package ... WARNING
    "exercises",
    ##* checking DESCRIPTION meta-information ... NOTE
    "meta-information",
    ##ERROR: ld.so: object 'libgtk3-nocsd.so.0' from LD_PRELOAD cannot be preloaded (failed to map segment from shared object): ignored.
    "LD_PRELOAD",
    ##* checking CRAN incoming feasibility ... NOTE
    "incoming feasibility")
  ignore.regex <- paste(ignore.lines, collapse="|")
  badLines <- grep(ignore.regex, all.warnLines, value=TRUE, invert=TRUE)
  if(length(badLines)>0){
    cat(paste(checkLines, collapse="\n"), "\n")
    print(badLines)
    stop("ERROR/WARNING/NOTE encountered in package check!")
  }
}

save.test.result <- function
### For unit tests, this is an easy way of getting a text
### representation of the list result of extract.docs.file.
(f
### R code file with inlinedocs to process with extract.docs.file.
 ){
  .result <- extract.docs.file(f)
  dump(".result",tmp <- tempfile(),control=NULL)
  lines <- readLines(tmp)
  escaped <- gsub("\\dots", "\\\\dots", lines, fixed=TRUE)
  cat(paste(escaped, "\n"))
}
