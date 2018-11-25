uri_format <- function(string, prefix = NULL){

  if (!is.null(prefix)){
    if(!grepl("://", prefix) && !grepl(":", string)){
      if (!grepl(":$", prefix)) prefix <- (paste0(prefix, ":"))
      string <- paste0(prefix, string)
    } else if (grepl(":", prefix)){
      if(!grepl("\\w+:/*\\w", string)) # don't prefix already prefixed terms
      string <- paste0(prefix, string)
    }
  }
  if(!grepl("^<.*>$", string))
    string <- paste0("<", string, ">")
  string
}