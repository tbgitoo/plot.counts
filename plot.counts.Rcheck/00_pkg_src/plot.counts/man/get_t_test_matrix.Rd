\name{get_t_test_matrix}
\alias{get_t_test_matrix}

\title{
Get a T-Test matrix comparing the results under different treatments
}
\description{
Compares the data obtained for each value of the controlling factor, and constructs a matrix with the t-test results
}
\usage{
get_t_test_matrix(treatment_factor, data, ...)
}

\arguments{
  \item{treatment_factor}{
  Factor indicating the treatment conditions
}
  \item{data}{
 Data obtained under the different conditions
}
\item{...}{
Additional arguments to be passed down to the underlying \code{\link{t.test}} function
}
}
\details{
The full list of data should be given, so that standard deviations can be calculated. More precisely, there should be several entries where \code{treatment_factor} has identical values.
}
\value{
A matrix containing the p-values of t-tests, comparing the data for each of the possible pairwise combinations of \code{treatment_factor}
}
\author{
Thomas Braschler
}
\examples{
test_data=data.frame(condition=c(rep("A",5),rep("B",5),rep("C",8)),outcome=c(1,3,2,2.5,3.2,8,8.25,9,8.5,7.5,0.1,0.5,-0.5,0.2,-0.25,0,0,1))
get_t_test_matrix(test_data$condition,test_data$outcome)
}

\keyword{ misc }

