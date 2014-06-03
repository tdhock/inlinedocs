\name{DocLink-class}
\Rdversion{1.1}
\docType{class}
\alias{DocLink-class}
\alias{DocLink}

\title{Link documentation among related functions}
\description{The \code{.DocLink} class provides the basis for hooking together
documentation of related classes/functions/objects. The aim is that
documentation sections missing from the child are inherited from
the parent class.}
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(DocLink ...)}}
\section{Slots}{
  \describe{
    \item{\code{name}:}{(\code{character}) name of object}
    \item{\code{created}:}{(\code{character}) how created}
    \item{\code{parent}:}{(\code{character}) parent class or NA}
    \item{\code{code}:}{(\code{character}) actual source lines}
    \item{\code{description}:}{(\code{character}) preceding description block}
  }
}
\section{Methods}{
No methods defined with class "DocLink" in the signature.
}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





