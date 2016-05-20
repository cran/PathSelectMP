\name{mod}
\alias{mod}
\title{Simple Modular arithmetic}
\description{mod operator created for syntax reasons}
\usage{
mod(x, m)
}
\arguments{
  \item{x}{number}
  \item{m}{modulus m number}
}
\details{helper function created for syntax reasons}
\value{number which is x mod m}
\references{no references}
\author{R help}
\note{no notes}
\examples{
\dontshow{
function (x, m) 
{
    t1 <- floor(x/m)
    return(x - t1 * m)
  }
}
mod(10,3)
}

\keyword{helper}

