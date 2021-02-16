\name{sd_mean}
\alias{sd_mean}
\title{sd_mean
}
\description{
Calculates the standard deviation of the mean
}
\usage{
sd_mean(x, na.rm = FALSE)
}

\arguments{
  \item{x}{
Data vector}
 \item{na.rm}{ If true, NA values are removed prior to calculation }

}
\details{
The standard deviation of the mean is the standard deviation per measurement divided by the square root of the number of measurements
}
\value{
Estimated standard deviation of the mean
}

\author{
Thomas Braschler
}


\examples{
x<-c(1,2,3,4,3.5,2.5,1)
sd(x)
sd_mean(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

