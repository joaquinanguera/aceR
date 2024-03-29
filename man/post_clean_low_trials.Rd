% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-post.R
\name{post_clean_low_trials}
\alias{post_clean_low_trials}
\title{Scrub processed data with too few trials}
\usage{
post_clean_low_trials(
  df,
  app_type = c("classroom", "explorer"),
  min_trials = 5,
  extra_demos = NULL
)
}
\arguments{
\item{df}{a df, output by \code{\link{proc_by_module}}, containing processed
ACE or SEA data.}

\item{app_type}{character. What app type produced this data? One of
\code{c("classroom", "explorer")}. Must be specified.}

\item{min_trials}{Minimum number of trials to require in most restrictive condition.
Defaults to 5. This condition is checked against the \code{*_count} summary columns,
that count all trials with a valid response time (and all no-go trials, if a response
was not expected.)}

\item{extra_demos}{Character vector specifying any custom-added demographics
columns (beyond app defaults) to pass through the function. Defaults to \{code{NULL}.}
}
\value{
a df, similar in structure to \code{proc}, but with records with too few trials
converted to \code{NA}.
}
\description{
User-friendly wrapper to replace records with too many no-responses with \code{NA}
in ACE data processed with \code{\link{proc_by_module}}.
}
