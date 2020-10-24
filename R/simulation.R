#' Set up an agentSim simulation
#'
#' @param time_total Number of time ticks to simulate
#' @param agents
#'
#' @return a simulation
#' @export
#'
#' @examples
#' #Example simulation in which each agent prints its own id to the console
#' init_pop <- tibble::tibble(
#'   id = c(1:50),
#'   x = runif(50, 0, 1),
#'   y = runif(50, 0, 1)
#'   )
#'
#' agent_task <- create_agent_task({
#'   print(agent$id)
#'   })
#'
#' sim <- agents_from_param_table(init_pop) %>%
#'   set_task(agent_task) %>%
#'   agent_sim(10)
agent_sim <- function(agents = NULL,
           time_total = NULL, environment = list(), referee = NULL) {

    me <- list(
      agents = agents,
      time_total = time_total,
      time_cur = 1,
      environment = environment,
      referee = referee,
      history = tibble::tibble(t = 1:time_total,
                               agents = rep(NA, time_total),
                               environment = rep(NA, time_total))
    )

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
iterate_sim <- function(x){
  UseMethod("iterate_sim", x)
}

#' @export
iterate_sim.simulation <- function(x){
  if(x$time_cur < x$time_total){
    x$history[x$time_cur] <- list(x$agents)
    x$time_cur <- x$time_cur
    return(x)
  } else {
    stop("You can't iterate this sim, it's already complete")
  }
}

#' Run a simulation
#'
#' @param x A simulation created with agent_sim()
#' @param debug Optionally, print calculation step info to the console to aid in
#' debugging task errors
#'
#' @return The original simulation, with a populated simulation$history$log
#' @export
#'
#' @examples
#' #Example simulation in which each agent prints its own id to the console
#' init_pop <- tibble::tibble(
#'   id = c(1:50),
#'   x = runif(50, 0, 1),
#'   y = runif(50, 0, 1)
#'   )
#'
#' agent_task <- function(agent, population){
#'   print(agent$id)
#'   agent
#'   }
#'
#' sim <- agents_from_param_table(init_pop) %>%
#'   set_task(agent_task) %>%
#'   agent_sim(100) %>%
#'   run_sim()
run_sim <- function(x, debug = FALSE){
  UseMethod("run_sim", x)
}

#' @export
run_sim.simulation <- function(x, debug = FALSE){

  pb = utils::txtProgressBar(min = 2, max = x$time_total, initial = 2)

  x$history$agents[[1]] <- list(x$agents)
  x$history$environment[[1]] <- list(x$environment)

  for(t in 2:x$time_total){

    if(debug) print(glue::glue("time: {t}"))

    for(a in 1:length(x$agents)){
      if(debug) print(glue::glue("agent: {a$id}"))

      #Run task using t-1 parameters so that agents act simultaneously
      x$agents[[a]] <-
        x$agents[[a]]$task(self = x$agents[[a]],
                          population = x$history$agents[[t - 1]],
                           environment = x$history$environment[[t - 1]])
    }

    #Run the referee task
    x <- x$referee$task(x)

    utils::setTxtProgressBar(pb, t)

    #Store updated agent and environment parameters
    x$history$agents[[t]] <- list(x$agents)
    x$history$environment[[t]] <- list(x$environment)
  }

  x
}
