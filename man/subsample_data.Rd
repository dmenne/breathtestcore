% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsample_data.R
\name{subsample_data}
\alias{subsample_data}
\title{Decimate densely sampled 13C time series}
\usage{
subsample_data(data, sample_minutes)
}
\arguments{
\item{data}{Data frame with columns \code{patient_id, group, minute, pdr}.}

\item{sample_minutes}{Required average density. When points are more closely
spaced, data are subsampled. No upsampling occurs when data are more sparse.}
}
\description{
When data of a record are more closely spaced than \code{sample_minutes}, 
these are spline-subsampled to \code{sample_minutes}. In the region of the initial slope,
i.e. the initial fifth of the time, the record is sampled more densely.
Too dense sampling leads to non-convergent \code{nlme} fits and to long runs
with Stan-based fits. 
The function is used internally by function \code{link{nlme_fit}} in 
package \code{breathtestcore} and is exported 
for use  by package \code{breathteststan}.
}
