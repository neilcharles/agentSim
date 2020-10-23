#' @title Define a sim environment
#'
#' @param params Named list of parameters that define the environment
#'
#' @return An 'environment' S3 object
#' @export
#'
#' @examples
#' new_environment <- sim_environment(weather = 'rain')
sim_environment <-
  function(params) {
    me <- params

    class(me) <- append(class(me), "sim_environment")

    return(me)

  }


#' Creates an environment from a parameter table
#'
#' @param param_table Data frame with one row and a column per environment variable
#'
#' @return list of environment parameters
#' @export
#'
#' @examples
#' init_env <- tibble::tibble(
#'   weather = 'rain',
#'   temperature = 15,
#'   )
#'
#' new_agents <- agents_from_param_table(init_env)
environment_from_param_table <- function(param_table) {
  param_table %>%
    dplyr::mutate(temp_time = dplyr::row_number()) %>%
    tidyr::nest(data = -temp_time) %>%
    dplyr::mutate(data = purrr::map(data, as.list)) %>%
    dplyr::select(-temp_time) %>%
    dplyr::mutate(environment = purrr::map(data, sim_environment)) %>%
    dplyr::pull(environment)
}
