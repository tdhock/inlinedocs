\name{extract.docs.file}
\alias{extract.docs.file}
\title{extract docs file}
\description{Apply all parsers relevant to extract info from just 1 code file.}
\usage{extract.docs.file(f, parsers = NULL, ...)}
\arguments{
  \item{f}{File name of R code to read and parse.}
  \item{parsers}{Parser Functions to use to parse the code and extract
documentation.}
  \item{\dots}{Other arguments to pass to Parser Functions.}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{
f <- system.file("silly","R","silly.R",package="inlinedocs")
extract.docs.file(f)
}
