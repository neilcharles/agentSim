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
#' agent_task <- function(agent, population){
#'   print(agent$id)
#'   agent
#'   }
#'
#' sim <- agents_from_param_table(init_pop) %>%
#'   set_task(agent_task) %>%
#'   agent_sim(100)
agent_sim <- function(agents = NULL,
           time_total = NULL) {

    me <- list(
      agents = agents,
      time_total = time_total,
      time_cur = 1,
      history = tibble::tibble(
        t = seq(1:time_total),
        log = NA
      )
    )

    me$history$log[1] <- list(agents)

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
    x$history$log[x$time_cur] <- list(x$agents)
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

  for(t in 2:x$time_total){

    if(debug) print(glue::glue("time: {t}"))

    for(a in 1:length(x$agents)){
      if(debug) print(glue::glue("agent: {a$id}"))

      #Run task using t-1 parameters so that agents act simultaneously
      x$agents[[a]] <-
        x$agents[[a]]$task(agent = x$agents[[a]],
                                     population = x$history$log[[t - 1]])
    }

    utils::setTxtProgressBar(pb, t)

    #Store updated agent parameters
    x$history$log[[t]] <- x$agents

  }

  x
}
