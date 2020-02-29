#' @title Make a population of individual agents
#'
#' @description Takes a table of parameters describing a population and creates
#' the individual agents required.
#'
#' @param init_pop A table of parameters describing agents (one row per agent)
#'
#' @return A population
#' @export
#'
#' @examples
#' #A parameter table to define 3 agents placed at random coordinates
#' init_pop <- tibble::tibble(
#'   id = c(1:3),
#'   type = "agent",
#'   location_x = runif(3, 0, 1),
#'   location_y = runif(3, 0, 1)
#'   )
#'
#' population <- makePopulation(init_pop)
makePopulation <- function(init_pop) {

  population <- init_pop %>%
    dplyr::mutate(agents = purrr::pmap(list(id, type, location_y, location_x), agent))

  x <- population$agents

  class(x) <- append(class(x), "population")

  x
}
