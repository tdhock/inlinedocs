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

.result <- list(
  f = list(
    description = "description",
    `item{y}` = "arg \\code{y}",
    `item{x}` = "arg \\code{x}",
    value = "a list with elements\n\\item{x}{original \\code{x} value}\n\\item{y}{original \\code{y} value}\n\\item{sum}{their sum}",
    title = "title",
    format = ""))
