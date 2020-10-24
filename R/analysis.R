#' Get records from an agentSim history
#'
#' @param sim An agentSim simulation
#'
#' @return a tibble
#' @export
#'
#' @examples
#' #' #Example simulation in which each agent prints its own id to the console
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
#'   agent_sim(10) %>%
#'   parse_history()
parse_history <- function(sim, area = 'agents'){
  if(area=='agents'){
    sim$history %>%
      tidyr::unnest_longer('agents') %>%
      tidyr::unnest_longer('agents') %>%
      tidyr::unnest_wider('agents') %>%
      dplyr::select(-task, -environment)
  } else if (area=='environment') {
    sim$history %>%
      dplyr::select(-agents) %>%
      tidyr::unnest(cols = c(environment)) %>%
      tidyr::unnest(cols = c(environment))
  }
}
