\name{lonely}
\alias{lonely}
\docType{data}
\title{\code{\link{lonely}}}
\description{List of parser functions that operate on single objects. This list
is useful for testing these functions.}
\usage{"lonely"}




\examples{
  f <- function # title
### description
  (x, ##<< arg x
   y
### arg y
   ){
    ##value<< a list with elements
    list(x=x, ##<< original x value
         y=y, ##<< original y value
         sum=x+y) ##<< their sum
    ##end<<
  }
  src <- \code{\link{getSource}}(f)
  \code{\link{lonely}}$\code{\link{extract.xxx.chunks}}(src)
  \code{\link{lonely}}$\code{\link{prefix}}ed.lines(src)
}
