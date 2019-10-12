remove.attributes.file <- function
### Remove attributes from all R objects defined in a file. This may
### be useful if for some reason we want to remove the examples from
### the ex attribute of functions.
(f
### File to parse and then write objects without attributes.
){
  parsed <- parse(f)
  e <- new.env()
  eval(parsed, e)
  con <- file(f, "w")
  for(obj.name in ls(e)){
    obj <- e[[obj.name]]
    attributes(obj) <- NULL
    cat(obj.name, "<-", file=con)
    dput(obj, con)
  }
  close(con)
}
