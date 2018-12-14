library(arkdb)
library(dbplyr)
library(rdftools)

## No, cannot stream directly, would have to normalize_nquads first.
stream_nq <-
  arkdb::streamable_table(
    function(file, ...) readr::read_tsv(file, ...),
    function(x, path, omit_header, ...)
      rdftools:::table_to_nquads(x = x,
                         file = path,
                         omit_header = omit_header,
                         ...
      ),
    "nq")


## Use arkdb to stream a database connection into nquads:


# Need to create a db with normalize_table(data) in it.
ark(db, ".", lines = 50000, streamable_table = stream_nq,  compress = "none")
