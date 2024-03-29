% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-trim.R
\name{trim_initial_trials}
\alias{trim_initial_trials}
\title{Trim initial trials from ACE/SEA data}
\usage{
trim_initial_trials(df, n = 5, exclude = c(), verbose = TRUE)
}
\arguments{
\item{df}{a \code{\link{data.frame}} containing formatted trialwise ACE data. 

This includes data loaded with the following methods: 
\enumerate{
  \item \code{\link{load_ace_file}}
  \item \code{\link{load_ace_bulk}}
}}

\item{n}{numeric. How many trials to remove from each participant's data? Defaults to 5.
When set as an \emph{integer}, removes the first N trials from each condition.
When set as a \emph{decimal} between 0 and 1, removes the first (N*100) percent of trials 
from each condition (more trials removed from longer tasks). Either way, this function
will remove the first N trials from each \emph{condition} completed by each participant.}

\item{exclude}{character vector. Specify the names of modules (proper naming convention!)
that should be \emph{ignored}. Defaults to an empty character vector, so that all modules
are scrubbed. Note that this function always excludes spatial span tasks, as those are
much shorter than other modules.}

\item{verbose}{logical. Print details? Defaults to \code{TRUE}.}
}
\value{
Returns the input data, with the first N trials for each participant \emph{removed}.
}
\description{
Applies corresponding \code{\link{ace_trims}} to every session of data.
If subsequent trimming is to be done by reacion time, we recommend calling this function first,
and then calling \code{\link{trim_rt_trials_range}} or \code{\link{trim_rt_trials_sd}}.
}
\section{Assumptions}{

Assumes the \code{\link{data.frame}} is nested, with two columns:
\code{module} (character) and \code{data} (list, each containing a \code{\link{data.frame}}).
}

