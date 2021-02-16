\name{barplot_dots}
\alias{barplot_dots}

\title{
barplot_dots
}
\description{
Bar plots for count data with errorbars and significance levels
}
\usage{
barplot_dots(treatment,data,main=NULL,xlab=NULL,ylab=NULL,sig_codes=NULL,ylim=NULL,xpd=NA,cex.axis=NULL,cex.lab=NULL,...)
}

\arguments{
  \item{treatment}{
 Vector describing the conditions applied. The distinct values in this vector will be the labels of the bars.
}
  \item{data}{
 Vector describing the individual results corresponding to the \code{treatment} conditions. Both \code{treatment} and \code{data} should be the raw data, not aggregated.
}
  \item{main}{
Title of graphics (graphical parameter as described in \link{par})
}

\item{xlab}{
Label of the x-axis (graphical parameter as described in \link{par})
}
\item{ylab}{
Label of the y-axis (graphical parameter as described in \link{par})
}
\item{sig_codes}{
Significance labels to be shown above the bars, as many entries as there are unique values in \code{treatment}
}
\item{ylim}{
Limits of the y-axis (graphical parameter as described in \link{par})
}

\item{xpd}{
Clipping (graphical parameter as described in \link{par}). Namely, supply \code{xpd=FALSE} for clipping of bars if \code{ylim} is provided with a range not starting at zero.
}

\item{cex.axis}{
Character size for the axis (graphical parameter as described in \link{par})
}

\item{cex.lab}{
Character size for the labels (graphical parameter as described in \link{par})
}
\item{las}{
Label orientation for axis (graphical parameter as described in \link{par})
}
\item{sig.cex}{
Character expansion for the significance label (see \link{barplot_with_errorbars}, used internally)
}



 
\item{...}{
Additional graphical parameters, to be passed specifically to \link{plot_dot_column}, used internally.
}
}

\author{
Thomas Braschler
}

\keyword{ misc }
