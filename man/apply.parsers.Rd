\name{apply.parsers}
\alias{apply.parsers}
\title{apply parsers}
\description{Parse \code{code} to r objs, then run all the \code{parsers} and return the
documentation list.}
\usage{apply.parsers(code, parsers = default.parsers, 
    verbose = FALSE, 
    ...)}
\arguments{
  \item{code}{Character vector of \code{code} lines.}
  \item{parsers}{List of Parser Functions.}
  \item{verbose}{Echo names of Parser Functions?}
  \item{\dots}{Additional arguments to pass to Parser Functions.}
}

\value{A list of extracted documentation from \code{code}.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





