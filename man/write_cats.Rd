\name{write_cats}
\alias{write_cats}

\title{Write appropriate files for YourCast analysis}

\description{Write a text file for every country age group formatted to the
original YourCast specifications.}

\usage{\method{write_cats}{yourcast}(df, stub="mort", sex=1, base=getwd(),
no_vars=c("location_id", "iso3", "age_group_id", "year", "sex"))}

\arguments{
  \item{df}{A data.frame with at least the variables,
  c("location_id", "iso3", "age_group_id", "year", "sex"), and one continuous
  variable to be used for analysis.}
  \item{stub}{The stub name to be used to proceed all saved files.}
  \item{sex}{The sex to subset the data.frame on.}
  \item{base}{The location to save the files.}
  \item{no_vars}{Variables not to include in the saved files for analysis.}
}

\references{\url{http://gking.harvard.edu/yourcast}}

\author{Neal Marquez \email{nmarquez@uw.edu}}

\seealso{\code{\link{yourcast}},\code{\link{summary.yourcast}}}
