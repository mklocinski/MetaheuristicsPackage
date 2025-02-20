% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mth_utilities.R
\name{next_in_line}
\alias{next_in_line}
\title{Sort items into queues based on queue status}
\usage{
next_in_line(items, queues, min_max = 1)
}
\arguments{
\item{items}{a list of items to sort.}

\item{queues}{a list of queues to sort items into.}

\item{min_max}{(Optional) Default value 1 instructs function to evaluate based on maximum value, -1 instructs function to evaluate based on minimum value.}
}
\value{
a list containing item order per queue.
}
\description{
Next in line (used for sorting items to queues with evaluation).
}
\details{
Pending.
}
\examples{
next_in_line(items = list(4, 50, 6, 3, 1, 9), queues = list(1, 2))

}
