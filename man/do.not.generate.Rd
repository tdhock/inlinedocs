\name{do.not.generate}
\alias{do.not.generate}
\title{do not generate}
\description{Make a Parser Function used to indicate that certain Rd files
should not be generated.}
\usage{do.not.generate(...)}
\arguments{
  \item{\dots}{Character strings indicating Rd files without the .Rd suffix.}
}

\value{A Parser Function that will delete items from the outer
Documentation List.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{
silly.pkg <- system.file("silly",package="inlinedocs")
owd <- setwd(tempdir())
file.copy(silly.pkg,".",recursive=TRUE)

## define a custom Parser Function that will not generate some Rd
## files
custom <- do.not.generate("silly-package","Silly-class")
parsers <- c(default.parsers,list(exclude=custom))

## At first, no Rd files in the man subdirectory.
man.dir <- file.path("silly","man")
dir(man.dir)

## Running package.skeleton.dx will generate bare-bones files for
## those specified in do.not.generate, if they do not exist.
package.skeleton.dx("silly",parsers)
Rd.files <- c("silly-package.Rd","Silly-class.Rd","silly.example.Rd")
Rd.paths <- file.path(man.dir,Rd.files)
stopifnot(all(file.exists(Rd.paths)))

## Save the modification times of the Rd files
old <- file.info(Rd.paths)$mtime

## make sure there is at least 2 seconds elapsed, which is the
## resolution for recording times on windows file systems.
Sys.sleep(4) 

## However, it will NOT generate Rd for files specified in
## do.not.generate, if they DO exist already.
package.skeleton.dx("silly",parsers)
mtimes <- data.frame(old,new=file.info(Rd.paths)$mtime)
rownames(mtimes) <- Rd.files
mtimes$changed <- mtimes$old != mtimes$new
print(mtimes)
stopifnot(mtimes["silly-package.Rd","changed"]==FALSE)
stopifnot(mtimes["Silly-class.Rd","changed"]==FALSE)
stopifnot(mtimes["silly.example.Rd","changed"]==TRUE)

unlink("silly",recursive=TRUE)
setwd(owd)
}
