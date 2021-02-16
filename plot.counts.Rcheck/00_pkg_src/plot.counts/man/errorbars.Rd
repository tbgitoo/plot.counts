\name{errorbars}
\alias{errorbars}

\title{
errorbars
}
\description{
Add errorbars to a plot
}
\usage{
errorbars(x, y, sd_y = 0, angle = 90, code = 3, horiz = FALSE, ...)
}

\arguments{
  \item{x}{
The x-values 
}
  \item{y}{
The nominal y-values
}
  \item{sd_y}{
Standard deviation of the y-values
}
  \item{angle}{
Angle for the line ending the errorbars (if a value other than 90 is chosen, it makes arrows)
}
  \item{code}{
Which errorbars should be drawn: \code{code=1} means only the lower errorbars (at \code{y-sd_y}), \code{code=2} means only the upper errorbars (at \code{y+sd_y}), \code{code=3} means both errorbars (from \code{y-sd_y} to \code{y+sd_y})
}
  \item{horiz}{
Logical flag indicating whether horizontal errobars should be drawn horizontal instead of vertical. They will be centered on the same points, but extending horizontally instead of vertically
}
  \item{\dots}{
Additional graphical parameters to pass down to the underlying \code{\link{arrows}} function
}
}

\author{
Thomas Braschler
}

\keyword{ misc }

