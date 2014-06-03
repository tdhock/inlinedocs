\name{lonely}
\alias{lonely}
\docType{data}
\title{lonely}
\description{List of parser functions that operate on single objects. This list
is useful for testing these functions.}
\usage{lonely}




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
  src <- getSource(f)
  lonely$extract.xxx.chunks(src)
  lonely$prefixed.lines(src)
}
