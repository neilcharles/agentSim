#' @title Define an agent
#'
#' @param params Named list of parameters that define the agent
#'
#' @return An 'agent' S3 object
#' @export
#'
#' @examples
#' new_agent <- agent(id = 1)
agent <-
  function(params) {

    me <- params

    #Empty task, do nothing and return unaltered agent
    me$task <- function(agent, population) me

    me$active <- TRUE

    class(me) <- append(class(me), "agent")

    return(me)

  }

#' @title Set the task for an agent or a list of agents
#'
#' @description Set the task for a single agent, or for a list of agents,
#' which will be executed for each time step when the simulation is run.
#' The task function must take the arguments \code{agent} and \code{population}
#' and must return \code{agent}
#'
#' @param x An agent or a list of agents
#' @param task A function to be executed in each time step.
#' You must pass \code{agent} and \code{population} as arguments to the function
#' (see example below)
#'
#' @return The original agent or agent list 'x', with tasks appended
#' @export
#'
#' @examples
#' #Build an example simulation in which each agent moves and then prints its coordinates to the console in each time step
set_task <- function(x, task, agent_type = "agent"){
  UseMethod('set_task', x)
}

#' @export
set_task.agent <- function(x, task){
  x$task <- task

  x
}

#' @export
set_task.default <- function(x, task){

  for(a in 1:length(x)){
    x[[a]]$task <- task
  }

  x
}

#' Creates a set of agents from a parameter table
#'
#' @param param_table Data frame with one row per agent. Each column will become
#' a named property of the created agents.
#'
#' @return list of agents
#' @export
#'
#' @examples
#' init_pop <- tibble::tibble(
#'   id = c(1:50),
#'   x = runif(50, 0, 1),
#'   y = runif(50, 0, 1)
#'   )
#'
#' new_agents <- agents_from_param_table(init_pop)
agents_from_param_table <- function(param_table){
  param_table %>%
    dplyr::mutate(temp_agent_number = dplyr::row_number()) %>%
    tidyr::nest(data = -temp_agent_number) %>%
    dplyr::mutate(data = purrr::map(data, as.list)) %>%
    dplyr::select(-temp_agent_number) %>%
    dplyr::mutate(agents = purrr::map(data, agent)) %>%
    dplyr::pull(agents)
}
