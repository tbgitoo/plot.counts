\name{plot_dot_column}
\alias{plot_dot_column}

\title{
plot_dot_column
}
\description{
Dot-plot showing individual values, with lateral shift if the y values are too close to show all the values
}
\usage{
	             
plot_dot_column(x,y,y_threshold_for_shifting=0.1,lateral_shift=0.06,type="p",group_order=NULL,...)
}

\arguments{
  \item{x}{
  x values
}
  \item{y}{
 associated y values
}
  \item{y_threshold_for_shifting}{
 Do lateral shift for display if two y-values with the same x-value differ by less than the threshold value
}

 \item{lateral_shift}{
 Lateral shift to be used between the points
} 
 
\item{type}{
Type argument to be passed to \link{lines} used internally
}
 
 \item{...}{
Additional arguments to be passed to \link{lines} used internally
}


}
\details{
The function is intended to handle an x vector with ascending, unique values, and associated y data
}
\author{
Thomas Braschler
}

\keyword{ misc }

\examples{
plot_dot_column(x=c(1,2,3),y=c(2,2,3),sd_y=c(1,1,0.5))

}
