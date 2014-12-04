\name{RStars-package}
\alias{RStars-package}
\alias{RStars}
\docType{package}
\title{
RStars
}
\description{
 Access to the Digital Universe Data set API pulls data on exoplants, stars, and galaxies.
}
\details{
\tabular{ll}{
Package: \tab RStars\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-03\cr
License: \tab GPL-3\cr
}
}
\author{
William Kyle Hamilton <kyle.hamilton@gmail.com>

Maintainer: William Kyle Hamilton <kyle.hamilton@gmail.com>

}

\keyword{ package }

\examples{
\dontrun{
 #Return Infomation about the local group galaxy 
 localgroupname("IC 10")
 #Return Infomation about the local group galaxy 
 localgroupname("WLM")
 #Return Infomation about all local group galaxies in the system
 localgroupname("")
 #Return Infomation about the exoplanet 11 Com
 exoplantname("11 Com")
 #Return Infomation about the exoplanet Kepler-9
 exoplantname("Kepler-9")
 #Return Infomation about all exoplants in the system
 exoplanetname("")
 #Return Infomation about the Sun
 starname("Sun")
 #Return Infomation about the star HIP1 HD224700 Gli
 starname("HIP1 HD224700 Gli")
 #Return Infomation about all of the stars in the system
 starname("")
}
}