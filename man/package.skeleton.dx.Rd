\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\title{Package skeleton deluxe}
\description{Generates Rd files for a package based on R code and DESCRIPTION
metadata. After reading the pkgdir/R/*.R code files to find inline
documentation (by default R code in *.r files will not be used for
inlinedocs), writes docs to pkgdir/man/*.Rd files, possibly
overwriting the previous files there.}
\usage{package.skeleton.dx(pkgdir = "..", 
    parsers = NULL, namespace = FALSE, 
    excludePattern = "[.][rsqS]$", 
    verbose = FALSE)}
\arguments{
  \item{pkgdir}{Package directory where the DESCRIPTION file lives. Your code
should be in pkgdir/R.}
  \item{parsers}{List of Parser functions, which will be applied in sequence to
extract documentation from your code. Default NULL means to first
search for a definition in the variable "parsers" in
pkgdir/R/.inlinedocs.R, if that file exists. If not, we use the
list defined in options("inlinedocs.parsers"), if that is
defined. If not, we use the package global default in the variable
\code{\link{default.parsers}}.}
  \item{namespace}{A logical indicating whether a NAMESPACE file should be generated
for this package. If \code{TRUE}, all objects whose name starts
with a letter, plus all S4 methods and classes are exported.}
  \item{excludePattern}{A regular expression matching the files that are not to be
processed e.g. because inlinedocs can not handle them yet (like
generic function definitions). Default value means to only process
inlinedocs in .R files. Set excludePattern=NULL to process all
code files, e.g. *.r files.}
  \item{verbose}{show messages about parser functions used?}
}



\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{

owd <- setwd(tempdir())

## get the path to the silly example package that is provided with
## package inlinedocs
testPackagePath <- file.path(system.file(package="inlinedocs"),"silly")
## copy example project to the current unlocked workspace that can
## be modified
file.copy(testPackagePath,".",recursive=TRUE)

## generate documentation .Rd files for this package
package.skeleton.dx("silly")

## check the package to see if generated documentation passes
## without WARNINGs.
if(interactive()){
  cmd <- sprintf("\%s CMD check --as-cran silly",file.path(R.home("bin"), "R"))
  print(cmd)
  system(cmd)
}
## cleanup: remove the test package from current workspace again
unlink("silly",recursive=TRUE)
setwd(owd)

}
