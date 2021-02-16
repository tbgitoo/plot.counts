\name{significance_labels}
\alias{significance_labels}

\title{
significance_labels
}
\description{
Converts a numerical p-value matrix or vector into a corresponding character matrix or vector with the text labels for significance
}
\usage{
significance_labels(x,levels=c(0.1,0.05,0.01,0.001),codes=c(".","*","**","***"))
}

\arguments{
  \item{x}{
  P-value vector or matrix
}
  \item{levels}{
 Cut-off levels for significance
}
\item{codes}{
 Significance codes for values below the corresponding levels
}
}
\details{
\code{levels} should be sorted in decreasing order, and the \code{codes} indicate the code to show for values below or equal to the corresponding level
}
\value{
A matrix or vector of dimensions identical to \code{x}, but containing the text codes for the significance level rather than the numerical p-values.
}
\author{
Thomas Braschler
}
\examples{

significance_labels(c(0.01,0.04,0.05,0.5))
}

\keyword{ misc }

