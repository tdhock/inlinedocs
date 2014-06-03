\name{test.file}
\alias{test.file}
\title{test file}
\description{Check an R code file with inlinedocs to see if the
extract.docs.file parser accurately extracts all the code inside!
The code file should contain a variable .result which is the
documentation list that you should get when you apply
extract.docs.file to the file. We check for identity of elements
of elements of the list, so the order of elements should not
matter, and thus this should be a good robust unit test.}
\usage{test.file(f, verbose = TRUE)}
\arguments{
  \item{f}{File name of R code file with inlinedocs to parse and check.}
  \item{verbose}{Show output?}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}



\seealso{\code{\link{save.test.result}}}

