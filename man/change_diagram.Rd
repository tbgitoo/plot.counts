\name{change_diagram}
\alias{change_diagram}

\title{
change_diagram
}
\description{
Diagram indicating the change from initial to final state by dots connected by lines
}
\usage{
change_diagram(y_initial,y_final,x_initial=NULL,x_final=NULL,groups=NULL,plot.new=TRUE,...)
}

\arguments{
  \item{y_initial}{
 Initially measured values, should be a numeric vector.
}
  \item{y_final}{
 Final measured values, should be numeric vector of length identical to \code{y_initial}.
}
  \item{x_initial}{
Initial x-values. Not mandatory, but if provided, should be numeric vector of the same length as \code{y_initial}.
}

\item{x_final}{
Final x-values. Not mandatory, but if provided, should be numeric vector of the same length as \code{y_initial}.
}
\item{groups}{
Group identifier. Not mandatory, but if provided, should be numeric vector, character vector or factor of the same length as \code{y_initial}.
}
\item{plot.new}{Start a new plot (as opposed to drawing onto some pre-existing plot
}



 
\item{...}{
Additional graphical parameters, to be passed specifically to \link{plot} and \link{lines} used internally. If pch, bg or col (see \link{par}) are provided, they can be vectors. If in addition a \code{groups} vector is provided, the values are used within the specific groups. 
}
}

\author{
Thomas Braschler
}

\keyword{ misc }
