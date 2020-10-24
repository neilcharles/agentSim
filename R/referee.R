#' @title Define the referee
#'
#' @description
#' The referee is a special class of agent with no parameters, whose function runs
#' after agents have completed their own functions in each time step.
#' You can think of the referee as being the environment's agent.
#' E.g. when simulating a game of soccer, the agents will be players and the environment
#' holds values such as the score. The referee is needed to increment the score and to reset
#' play after a team scores a goal.
#'
#' Agents themselves could be given the referee's tasks but it's simpler to have a separate function that
#' runs at the end of each time step, after all of the agents have completed their tasks.
#'
#' The referee has access to the entire simulation object and can modify any part of it,
#' including agents, the environment and the referee itself.
#'
#' @return A 'referee' S3 object
#' @export
#'
#' @examples
#' new_referee <- referee()
referee <-
  function() {

    me <- list()

    #Empty placeholder task, do nothing and return the input
    me$task <- function(x) x

    class(me) <- append(class(me), "referee")

    return(me)

  }

#' @title Create a task to be executed by agents
#'
#' @description
#' Agents executing a task have access to three objects:
#' * \code{self} is the executing agent's S3 object in the current time step
#' * \code{population} holds parameters for the entire agent population at time t-1
#' * \code{environment} holds parameters for the environment at time t-1
#'
#' \code{population} and \code{environment} return parameters for the previous time step, to allow
#' all agents to execute their tasks using the same parameter set, before time moves to
#' the next 't' and the population and environment are updated.
#'
#' @param f code to be run by agents at each time step.
#'
#' @return A function to be run during simulations
#' @export
#'
#' @examples
#' agent_task <- create_agent_task({
#'   print(self$id)
#' })
create_referee_task <- function(f){

  referee_expression <- substitute(f)

  task <- function(x){
    eval(referee_expression)
    x
  }

  task
}
