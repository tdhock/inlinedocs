remove.attributes.file <- function(f){
  parsed <- parse(f)
  e <- new.env()
  eval(parsed, e)
  con <- file(f, "w")
  for(obj.name in ls(e)){
    obj <- e[[obj.name]]
    attributes(obj) <- NULL
    dput(obj, con)
  }
  close(con)
}
