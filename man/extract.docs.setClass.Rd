\name{extract.docs.setClass}
\alias{extract.docs.setClass}
\title{S4 class inline documentation}
\description{Using the same conventions as for functions, definitions of S4 classes
in the form \code{setClass("classname",\dots)} are also located and
scanned for inline comments.}
\usage{extract.docs.setClass(doc.link)}
\arguments{
  \item{doc.link}{DocLink object as created by \code{extract.file.parse}.
Note that \code{source} statements are \emph{ignored} when scanning for
class definitions.}
}
\details{Extraction of S4 class documentation is currently limited to expressions
within the source code which have first line starting with
\code{setClass("classname"}. These are located from the source file
(allowing also for white space around the \code{setClass} and \code{(}).
Note that \code{"classname"} must be a quoted character string;
expressions returning such a string are not matched.

For class definitions, the slots (elements of the
\code{representation} list) fill the role of function
arguments, so may be documented by \code{##<<} comments on
the same line or \code{### } comments at the beginning of the
following line.

If there is no explicit title on the first line of setClass, then
one is made up from the class name.

The class definition skeleton includes an \code{Objects from the Class}
section, to which any \code{##details<<} documentation chunks are
written. It is given a vanilla content if there are no specific
\code{##details<<} documentation chunks.}


\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}





