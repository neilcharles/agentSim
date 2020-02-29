#' @title Define an agent
#'
#' @param id An ID for the agent. Can be a number or character name.
#' @param type Character string describing the agent's type e.g. 'predator' or 'prey'
#' @param location_y Agent's starting y coordinate
#' @param location_x Agent's starting x coordinate
#'
#' @return An 'agent' S3 object
#' @export
#'
#' @examples
#' new_agent <- agent(id = 1)
agent <-
  function(id = NULL,
           type = "agent",
           location_y = NULL,
           location_x = NULL) {

    me <- list(
      id = id,
      type = type,
      location_y = location_y,
      location_x = location_x,
      task = function() invisible()
    )

    class(me) <- append(class(me), "agent")

    return(me)

  }

#' Set the agent's location using x,y coordinates
#'
#' @param x Agent
#' @param newLocation Location vector as a y,x (lon, lat) pair
#'
#' @return Agent
#' @export
#'
#' @examples
#' new_agent <- agent(id = 1, location_y = 1, location_x = 2)
#'
#' #Move the agent to coordinates y = 2, x = 3
#' new_agent <- setLocation(new_agent, c(2,3))
setLocation <- function(x, newLocation){
  UseMethod('setLocation', x)
}

#' @export
setLocation.agent <- function(x, newLocation){
  x$location_y <- newLocation[1]
  x$location_x <- newLocation[2]
  x
}

#' @title Set an agent's task
#'
#' @description Set the task for a single agent, or for a set of agents within a population,
#' which will be executed for each time step when the simulation is run. The function you define
#' must take the arguments \code{agent} and \code{population} and must return \code{agent}
#'
#' @param x An agent or a population of agents
#' @param task A function to be executed in each time step. You must pass \code{agent} and \code{population} as arguments to the function (see example below)
#' @param agent_type If x is a population, which agent type(s) in the population should run this task?
#'
#' @return agent or population
#' @export
#'
#' @examples
#' #Build an example simulation in which each agent moves and then prints its coordinates to the console in each time step
#'
#' #A parameter table to define 3 agents placed at random coordinates
#' init_pop <- tibble::tibble(
#'   id = c(1:3),
#'   type = "agent",
#'   location_x = runif(3, 0, 1),
#'   location_y = runif(3, 0, 1)
#'   )
#'
#' #Define the task function, which will be run by an agent in each time step.
#' #You must pass 'agent' and 'population' as arguments to the function and must
#' #return 'agent' so that the agent can see itself and other agents when it
#' #runs its task and so that it will update in the next time step.
#' move_and_report <- function(agent, population){
#'
#'   #Move the agent
#'   agent$location_y <- agent$location_y + runif(1, -0.5, 0.5)
#'   agent$location_x <- agent$location_x + runif(1, -0.5, 0.5)
#'
#'   #Print the agent's position to the console
#'   print(
#'     paste0("Agent '", agent$id, "' position: ",
#'     agent$location_y, ", ", agent$location_x)
#'   )
#'
#'   agent
#' }
#'
#' #Create agents and assign tasks
#' agents_with_tasks <- makePopulation(init_pop) %>%
#'   setTask(move_and_report)
setTask <- function(x, task, agent_type = "agent"){
  UseMethod('setTask', x)
}

#' @export
setTask.agent <- function(x, task){
  x$task <- task

  x
}

#' @export
setTask.population <- function(x, task, agent_type = "agent"){

  for(a in 1:length(x)){

    if(x[[a]]$type %in% agent_type){
      x[[a]]$task <- task
    }
  }

  x
}
