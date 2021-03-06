% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_nquads.R
\name{write_nquads}
\alias{write_nquads}
\title{write object out as nquads}
\usage{
write_nquads(x, file, prefix = NULL, compress = grepl("[.]gz$", file),
  ...)
}
\arguments{
\item{x}{an object that can be represented as nquads}

\item{file}{output filename}

\item{prefix}{URI prefix that should be used to identify column names.
Can be of the form "http://schema.org/", or "iris:".}

\item{compress}{Compress nquads with gzip? (Very effective for large
files. Note that Virtuoso and other parsers can read compressed files
directly.)}

\item{...}{additional parameters, see examples}
}
\description{
write object out as nquads
}
\examples{
iris_file <- tempfile(fileext = ".nq.gz")
library(datasets)
write_nquads(iris, iris_file)

# Use key_column to indicate that a column should be treated
# as the primary key.  This column is then prefixed (if not
# already a URI) and used as the subject for the corresponding
# rows.
car_file <- tempfile(fileext = ".nq")
mtcars$model <- rownames(mtcars)
mtcars$car_id <- 1:length(mtcars[[1]])
write_nquads(mtcars,
             car_file,
             prefix = "mtcars",
             key_column = "car_id")
}
