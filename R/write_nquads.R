#' write object out as nquads
#'
#' @param x an object that can be represented as nquads
#' @param file output filename
#' @param prefix URI prefix that should be used to identify column names.
#' Can be of the form "http://schema.org/", or "iris:".
#' @param compress Compress nquads with gzip? (Very effective for large
#' files. Note that Virtuoso and other parsers can read compressed files
#' directly.)
#' @param ... additional parameters, see examples
#'
#' @export
#'
#' @examples
#' iris_file <- tempfile(fileext = ".nq.gz")
#' library(datasets)
#' write_nquads(iris, iris_file, compress = TRUE)
#'
#' # Use key_column to indicate that a column should be treated
#' # as the primary key.  This column is then prefixed (if not
#' # already a URI) and used as the subject for the corresponding
#' # rows.
#' car_file <- tempfile(fileext = ".nq")
#' mtcars$model <- rownames(mtcars)
#' mtcars$car_id <- 1:length(mtcars[[1]])
#' write_nquads(mtcars,
#'              car_file,
#'              prefix = "mtcars",
#'              key_column = "car_id")
write_nquads <- function(x,
                         file,
                         prefix = NULL,
                         compress = FALSE,
                         ...){
  UseMethod("write_nquads")
}

## FIXME Consider making these functions 'opt in' dependencies.
## dplyr, tidyr could be soft dependencies, like json-ld (& jsonlite)
## Alternately, this whole methods stack could be broken out to separate
## utility package.

## could also include a `read_nquads` that simply reverses these processes
## manually, returning data as (long-format) data.frame or list??



#' @export
write_nquads.data.frame <- function(x,
                                    file,
                                    prefix = NULL,
                                    compress = FALSE,
                                    ...){

  if (is.null(prefix)) {
    prefix <- paste0(deparse(substitute(x)), ":")
    warning(paste("prefix not declared, using", prefix))
  }
  prefix <- uri_prefix(prefix)

  if(compress){
    file <- gzfile(file, open = "wb")
  }

  df <- normalize_table(x, ...)
  table_to_nquads(df,
                   file = file,
                   prefix = prefix,
                   ...)
}

uri_format

#' @export
#' @importFrom jsonlite toJSON
write_nquads.list  <- function(x,
                               file,
                               prefix = NULL,
                               compress = FALSE,
                               ...){

  if(compress){
    file <- gzfile(file, open = "wb")
  }

  ## Avoid a hard dependency on jsonld package
  if (!requireNamespace("jsonld", quietly = TRUE)) {
    stop("jsonld package required to convert to lists to nquads",
         call. = FALSE)
  }
  jsonld_to_rdf <- getExportedValue("jsonld", "jsonld_to_rdf")


  ## Handle prefix
  if (is.null(prefix)) {
    prefix <- paste0(deparse(substitute(x)), ":")
    warning(paste("prefix not declared, using", prefix))
  }
  prefix <- uri_prefix(prefix)

  ## Handle compression



  json <- jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE)

  ##  Use context to set a prefix and a base URI
  prepend <- paste0('{\n"@context": {"@base": "',
                   getOption("rdf_base_uri", "rdf://"), '", ',
                   '"@vocab": "', prefix,
                    '" },\n"@graph": ')
  append <- '}'
  jsonld <- paste(c(prepend, json, append), sep = "\n", collapse = "\n")

  options <- list(base = getOption("rdf_base_uri", "rdf://"),
                 format = "application/nquads")

  writeLines(jsonld_to_rdf(jsonld, options = options),
             file)
}






#' @importFrom tidyr gather
#' @importFrom dplyr left_join
normalize_table <- function(df, key_column = NULL, ...){
  ## gather looses col-classes, so pre-compute them (with base R)
  col_classes <- data.frame(datatype =
                              vapply(df,
                                     xs_class,
                                     character(1)))
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL

  ## Use row names as key (subject), unless a key column is specified
  ## Should we verify that requested key column is indeed a unique key first?
  out <- df
  if (is.null(key_column)) {
    out$subject <- as.character(1:dim(out)[[1]])
  } else {
    names(out)[names(out) == key_column] <- "subject"
  }

  ## FIXME consider taking an already-gathered table to avoid dependency?

  suppressWarnings(# Possible warnings about mixed types
    out <- tidyr::gather(out,
                         key = "predicate",
                         value = "object",
                         -"subject"))

  ## merge is Slow! ~ 5 seconds for 800K triples
  ## (almost as much time as rdf_parse)
  # merge(out, col_classes, by = "predicate")

  dplyr::left_join(out, col_classes, by = "predicate")

}


## Currently written to be base-R compatible,
## but a tidyverse implementation may speed serialization.
## However, this seems to be fast enough that it is rarely the bottleneck

## NOTE: paste0 is a little slow ~ 1 s on 800K triples
## No datatype on blank (missing) nodes
uri_table <- function(df, prefix){

  blank_object <-is.na(df$object)
  blank_subject <- is.na(df$subject)

  df$subject[!blank_subject] <- vapply(df$subject[!blank_subject],
                                       uri_format,
                                       character(1L),
                                       prefix = prefix)
  df$predicate <- vapply(df$predicate,
                         uri_format,
                         character(1L),
                         prefix = prefix)


  df$datatype[blank_object] <- as.character(NA)
  ## NA needs to become a unique blank node number, could do uuid or _:r<rownum>
  df$object[blank_object] <- paste0("_:r", which(blank_object))
  df$subject[blank_subject] <- paste0("_:r", which(blank_subject))

  ## strings and URIs do not get a datatype
  is_uri_object <- grepl("^<*\\w+:/*\\w.*>*$", df$object)
  ## Strings should be quoted
  is_string <- !is_uri_object & !blank_object
  df$object[is_string] <- paste0('\"', df$object[is_string] , '\"')

  ## Non-blank objects with URIs need <> instead, but not blanks!
  df$object[!blank_object] <- gsub("(^\\w+:/*\\w.*$)", "<\\1>",
                                  df$object[!blank_object])


 df
}


## x is a data.frame with columns: subject, predicate, object, & datatype
#' @importFrom utils write.table
table_to_nquads <- function(x, file, prefix, graph = ".", ...){

  x <- uri_table(x, prefix)

  ## quads format needs a graph column
  if (! "graph" %in% names(x)) x$graph <- graph

  ## assumes datatype is not empty (e.g. string)
  needs_type <- !is.na(x$datatype)
  x$object[needs_type] <- paste0('\"', x$object[needs_type],
                                  '\"^^<', x$datatype[needs_type], ">")


  ## drop datatype column now
  x <- x[c("subject", "predicate", "object", "graph")]

  ## write table is a little slow, ~ 1s on 800K triples,
  ## but readr cannot write in nquads style
  utils::write.table(x, file, col.names = FALSE,
                     quote = FALSE, row.names = FALSE)
}


## Don't explicitly type characters as strings, since this is default
xs_class <- function(x){
  type <- switch(class(x)[[1]],
                 "numeric" = "xs:decimal",
                 "factor" = "xs:string",
                 "logical" = "xs:boolean",
                 "integer" = "xs:integer",
                 "Date" = "xs:date",
                 "POSIXct" = "xs:dateTime",
                 NULL
                )
  string <- gsub("^xs:",
                 "http://www.w3.org/2001/XMLSchema#",
                 type)
  ## consistent return length, character(1)
  if (length(string) == 0) {
    string <- as.character(NA)
  }
  string
}


# "x" -> "x:"
# "x:" -> "x:"
# "http://stuff" -> "http://stuff"
uri_prefix <- function(x){
  abs_uri <- grepl("^\\w+://", x)
  if (abs_uri) {
    if (!grepl("[#/]$", x)) return(paste0(x, "#"))
    return(x)
  }
  if (!grepl(":$", x)) return(paste0(x, ":"))
  x
}





