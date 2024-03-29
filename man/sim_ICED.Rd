% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ICED.R
\name{sim_ICED}
\alias{sim_ICED}
\title{simulates data based on ICED model structure and list of variances}
\usage{
sim_ICED(
  structure,
  means = NULL,
  variances,
  n,
  growth = FALSE,
  int_slope_covariance = FALSE,
  check_recovery = FALSE
)
}
\arguments{
\item{structure}{data.frame describing the structure of the data, with each variable covering a design aspect - see example. Note: currently the first variable must be time and include a different value for each repeated measure.}

\item{variances}{list of variances corresponding to each latent variable specified in strucutre}

\item{n}{number of participants to simulate}

\item{check_recovery}{runs run_ICED to extract variance components in order to check the variance parameter recovery}
}
\value{
list including simulated data
}
\description{
sim_ICED simulates n x p data frame based on ICED model structure,
selected variance components, and specified n
}
\examples{
# compare recovery of variance parameters

# ICED structure
struc <- data.frame(time = c("T1", "T2", "T3", "T4"),
day = c("day1","day1","day2","day2"),
session = c("ses1", "ses1","ses2", "ses3"))

sim_ICED(struc,
variances = list(time = 10,
                 day = 2,
                 session = 1,
                 error = 3),
n = 20,
check_recovery = TRUE)

sim_ICED(struc,
         variances = list(time = 10,
                          day = 2,
                          session = 1,
                          error = 3),
         n = 2000,
         check_recovery = TRUE)


}
