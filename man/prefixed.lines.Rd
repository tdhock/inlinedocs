\name{prefixed.lines}
\alias{prefixed.lines}
\title{prefixed lines}
\description{The primary mechanism of inline documentation is via consecutive
groups of lines matching the specified prefix regular expression
"\code{^### }" (i.e. lines beginning with "\code{### }") are
collected as follows into documentation sections:\describe{
\item{description}{group starting at line 2 in the code}
\item{arguments}{group following each function argument}
\item{value}{group ending at the penultimate line of the code}}
These may be added to by use of the \code{##<<} constructs
described below.}
\usage{prefixed.lines(src, ...)}
\arguments{
  \item{src}{
}
  \item{\dots}{
}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{
test <- function
### the description
(x,
### the first argument
 y ##<< another argument
 ){
  5
### the return value
##seealso<< foobar
}
src <- getSource(test)
prefixed.lines(src)
extract.xxx.chunks(src)
}
