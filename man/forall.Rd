\name{forall}
\alias{forall}
\title{forall}
\description{For each object in the package that satisfies the criterion
checked by \code{subfun}, parse source using \code{FUN} and return the resulting
documentation list.}
\usage{forall(FUN, subfun = function(x) TRUE)}
\arguments{
  \item{FUN}{Function to apply to each element in the package.}
  \item{subfun}{Function to select subsets of elements of the package, such as
is.function. subfun(x)==TRUE means \code{FUN} will be applied to x and
the result will be returned.}
}

\value{A Parser Function.}

\author{Toby Dylan Hocking <toby.hocking@r-project.org> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus MÃ¼ller [aut], R Core Team [ctb, cph]}





