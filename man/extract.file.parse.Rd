\name{extract.file.parse}
\alias{extract.file.parse}
\title{File content analysis}
\description{Using the base \code{parse} function, analyse the file to link
preceding "prefix" comments to each active chunk. Those comments form
the default description for that chunk. The analysis also looks for
S4 class "setClass" calls and R.oo setConstructorS3 and \code{\link[R.methodsS3]{setMethodS3}}
calls in order to link the documentation of those properly.}
\usage{extract.file.parse(code)}
\arguments{
  \item{code}{Lines of R source \code{code} in a character vector - note that any
nested \code{source} statements are \emph{ignored} when scanning
for class definitions.}
}
\details{If the definition chunk does not contain a
description, any immediately preceding sequence consecutive
"prefix" lines will be used instead.

Class and method definitions can take several forms,
determined by expression type: \describe{
\item{assignment (<-)}{Ordinary assignment of value/function;}
\item{setClass}{Definition of S4 class;}
\item{setConstructorS3}{Definition of S3 class using R.oo package;}
\item{R.methodsS3::setMethodS3}{Definition of method for S3 class using R.oo package.}}
Additionally, the value may be a name of a function defined elsewhere,
in which case the documentation should be copied from that other definition.
This is handled using the concept of documentation links.

The \code{R.methodsS3::setMethodS3} calls introduce additional
complexity: they will define an additional S3 generic (which
needs documentation to avoid warnings at package build time)
unless one already exists. This also is handled by "linking"
documentation. A previously unseen generic is linked to the
first defining instances, subsequent definitions of that generic
also link back to the first defining instance.}
\value{Returns an invisible list of .DocLink objects.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





