\name{make.package.and.check}
\alias{make.package.and.check}
\title{make package and check}
\description{Assemble some R code into a package and process it using R CMD
check, stopping with an error if the check resulted in any errors
or warnings.}
\usage{make.package.and.check(f, parsers = default.parsers, verbose = TRUE)}
\arguments{
  \item{f}{R code file name from which we will make a package}
  \item{parsers}{Parsers to use to make the package documentation.}
  \item{verbose}{print the check command line?}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





