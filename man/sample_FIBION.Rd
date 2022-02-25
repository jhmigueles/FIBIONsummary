\name{sample_FIBION.csv}
\alias{sample_FIBION.csv}
\docType{data}
\title{
Sample csv exported from FIBION website
}
\description{
Sample csv exported from FIBION website
}
\usage{data("sample_FIBION.csv")}
\format{
  A data frame with 768 observations (i.e., each observation represent a 15-min interval) on the following 13 variables.
  \describe{
    \item{\code{utc}}{a character vector with timestamps in ISO8601 format (UTC)}
    \item{\code{local}}{a character vector with timestamps in ISO8601 format (local time)}
    \item{\code{unixts}}{timestamp in unixts format}
    \item{\code{general.nodata.time}}{minutes in current epoch in which the device was not worn}
    \item{\code{activity.lying.sitting.time}}{minutes in current epoch in which the participant was sitting or lying}
    \item{\code{activity.upright_stand.time}}{minutes in current epoch in which the participant was standing}
    \item{\code{activity.upright_sporadic_walk.time}}{minutes in current epoch in which the participant was walking sporadically}
    \item{\code{activity.upright_walk.time}}{minutes in current epoch in which the participant was walking continously}
    \item{\code{activity.upright_run.time}}{minutes in current epoch in which the participant was running}
    \item{\code{activity.cycling.time}}{minutes in current epoch in which the participant was cycling}
    \item{\code{activity.sit2stand.count}}{number of times in current epoch in which the participant stood up from a sitting/lying position}
    \item{\code{activity.steps.count}}{number of continuous steps in current epoch}
    \item{\code{activity.steps2.count}}{number of sporadic steps in current epoch}
  }
}
\examples{
utils::read.csv(sample_FIBION.csv)
}
\keyword{datasets}
