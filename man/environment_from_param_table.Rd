% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{environment_from_param_table}
\alias{environment_from_param_table}
\title{Creates an environment from a parameter table}
\usage{
environment_from_param_table(param_table)
}
\arguments{
\item{param_table}{Data frame with one row and a column per environment variable}
}
\value{
list of environment parameters
}
\description{
Creates an environment from a parameter table
}
\examples{
init_env <- tibble::tibble(
  weather = 'rain',
  temperature = 15,
  )

new_agents <- agents_from_param_table(init_env)
}
