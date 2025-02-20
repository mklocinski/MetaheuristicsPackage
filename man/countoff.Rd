% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mth_utilities.R
\name{countoff}
\alias{countoff}
\title{Count off items in a list into queues}
\usage{
countoff(items, queues)
}
\arguments{
\item{items}{a list of items to sort.}

\item{queues}{a list of queues to sort items into.}
}
\value{
a list containing item order per queue.
}
\description{
Sorts items into queues based on item position in item list.
}
\details{
Works the same dealing n cards to m people, except that it allows for
remainders to be included (so the mth player can get (n/m) + 1 cards).

To sort items into queues based on queue length, use 'next_in_line' function.
}
