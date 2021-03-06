% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{parse_history}
\alias{parse_history}
\title{Get records from an agentSim history}
\usage{
parse_history(sim, area = "agents")
}
\arguments{
\item{sim}{An agentSim simulation}
}
\value{
a tibble
}
\description{
Get records from an agentSim history
}
\examples{
#' #Example simulation in which each agent prints its own id to the console
init_pop <- tibble::tibble(
  id = c(1:50),
  x = runif(50, 0, 1),
  y = runif(50, 0, 1)
  )

agent_task <- create_agent_task({
  print(agent$id)
  })

sim <- agents_from_param_table(init_pop) \%>\%
  set_task(agent_task) \%>\%
  agent_sim(10) \%>\%
  parse_history()
}
