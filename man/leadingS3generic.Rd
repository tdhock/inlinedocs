\name{leadingS3generic}
\alias{leadingS3generic}
\title{check whether function name is an S3 generic}
\description{Determines whether a function name looks like an S3 generic function}
\usage{leadingS3generic(name, env, ...)}
\arguments{
  \item{name}{name of function}
  \item{env}{environment to search for additional generics}
  \item{\dots}{ignored here}
}
\details{This function is one of the default parsers, but exposed as
possibly of more general interest. Given a function name of the form
x.y.z it looks for the generic function x applying to objects of class
y.z and also for generic function x.y applying to objects of class z.


Assumes that the first name which matches any known
generics is the target generic function, so if both x and x.y
are generic functions, will assume generic x applying to objects
of class y.z}
\value{If a matching generic found returns a list with a single component:
\item{.s3method}{a character vector containing generic name and object name.
}

If no matching generic functions are found, returns an empty list.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





