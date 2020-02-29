#' Set up an agentSim simulation
#'
#' @param agent_population A population of agents
#' @param time_total Number of time ticks to simulate
#'
#' @return a simulation
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
#' #Define the task function, which will be run in each time step.
#' #You must pass 'agent' and 'population' as arguments to the function
#' #so that the agent can see itself and other agents when it runs its task.
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
#' #Set up the simulation
#' simulation <- makePopulation(init_pop) %>%
#'   setTask(move_and_report) %>%
#'   agentSim(10)
agentSim <- function(agent_population = NULL,
           time_total = NULL) {

    me <- list(
      agent_population = agent_population,
      time_total = time_total,
      time_cur = 1,
      history = tibble::tibble(
        t = seq(1:time_total),
        log = NA
      )
    )

    me$history$log[1] <- list(agent_population)

    class(me) <- append(class(me), "simulation")

    return(me)

  }

#' @title Run the next time step in a simulation
#'
#' @description Usually only called as part of runSim()
#'
#' @param x A simulation
#'
#' @return A simulation
#' @export
#'
#' @examples
iterateSim <- function(x){
  UseMethod("iterateSim", x)
}

#' @export
iterateSim.simulation <- function(x){
  if(x$time_cur < x$time_total){
    x$history$log[x$time_cur] <- list(x$agentPopulation)
    x$time_cur <- x$time_cur
    return(x)
  } else {
    stop("You can't iterate this sim, it's already complete")
  }
}

#' Run a simulation
#'
#' @param x A simulation created with agentSim()
#' @param debug Optionally, print calculation step info to the console to aid in
#' debugging task errors
#'
#' @return The original simulation, with a fully populated simulation$history$log
#' @export
#'
#' @examples
#' #' #Build an example simulation in which each agent moves and then prints its coordinates to the console in each time step
#'
#' #A parameter table to define 3 agents placed at random coordinates
#' init_pop <- tibble::tibble(
#'   id = c(1:3),
#'   type = "agent",
#'   location_x = runif(3, 0, 1),
#'   location_y = runif(3, 0, 1)
#'   )
#'
#' #Define the task function, which will be run in each time step.
#' #You must pass 'agent' and 'population' as arguments to the function
#' #so that the agent can see itself and other agents when it runs its task.
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
#' #Run the simulation with 10 time steps
#' makePopulation(init_pop) %>%
#'   setTask(move_and_report) %>%
#'   agentSim(10) %>%
#'   runSim()
runSim <- function(x, debug = FALSE){
  UseMethod("runSim", x)
}

#' @export
runSim.simulation <- function(x, debug = FALSE){

  pb = utils::txtProgressBar(min = 2, max = x$time_total, initial = 2)

  for(t in 2:x$time_total){

    if(debug) print(glue::glue("time: {t}"))

    for(a in 1:length(x$agent_population)){
      if(debug) print(glue::glue("agent: {a$id}"))

      #Run task using t-1 parameters so that agents act simultaneously
      x$agent_population[[a]] <-
        x$agent_population[[a]]$task(agent = x$agent_population[[a]],
                                     population = x$history$log[[t - 1]])
    }

    utils::setTxtProgressBar(pb, t)

    #Store updated agent parameters
    x$history$log[[t]] <- x$agent_population

  }

  x
}
