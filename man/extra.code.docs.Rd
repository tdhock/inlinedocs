\name{extra.code.docs}
\alias{extra.code.docs}
\title{Extract documentation from code chunks}
\description{Parse R code to extract inline documentation from comments around
each function. These are not able to be retreived simply by
looking at the "source" attribute. This is a Parser Function that
can be used in the parser list of package.skeleton.dx(). TODO:
Modularize this into separate Parsers Functions for S4 classes,
prefixes, ##<<blocks, etc. Right now it is not very clean!}
\usage{extra.code.docs(code, objs, ...)}
\arguments{
  \item{code}{Code lines in a character vector containing multiple R objects to
parse for documentation.}
  \item{objs}{The objects defined in the code.}
  \item{\dots}{ignored}
}

\value{named list of lists, one for each object to document.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





