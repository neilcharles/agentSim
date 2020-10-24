
# agentSim

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

agentSim is an experimental agent based modelling framework for R. You probably don't want to download it yet. 

## Installation

``` r
remotes::install_github("neilcharles/agentSim")
```
## Concepts

The following components make up an 'agentSim' simulation:

* **Agents** make decisions and interact with each other and their environment.
* **Time** passes in dicrete steps. In each time step, each agent will run their decision making function once.
* **Environment** keeps track of high level parameters that are not related to single agents.
* **Referee** is a special class of agent, whose decision making function runs at the end of each time step, once all agents have completed their own functions.
* **History** tracks everything that has happened in the simulation at each time step and can be analysed once the sim has finished.

For example, when simulating a game of soccer, the **agents** will be the players and the **environment**
holds values such as the dimensions of the pitch and the current score. After all players have completed their function in each **time** step, the **referee** is needed to check for a goal, and if one has been scored, to increment the environment variable tracking the scoreline and to reset play.


## Minimal Example

In this minimal example, agents don't do anything other than print 'hello' to the console so that you can see how to set up the components needed for a sim and when each agent's function runs.

First, set up a one-row parameter table that will define the environment

``` r
library(agentSim)

total_time <- 10

init_env <- tibble::tibble(
  weather = 'rain',
  temperature = 15,
)

```

The starting environment was just a one row tibble, but agents are more complex. agentSim has a function to make agents from a parameter table.

``` r
init_pop <- tibble::tibble(
  id = c(1:5),
  x = runif(5, 0, 1),
  y = runif(5, 0, 1)
)

agent_population <- agents_from_param_table(init_pop)
```

For anything to happen during the simulation, agents need a function. This example task simply makes an agent print 'hello' to the console.

Agents have access to the objects 'self', which is the agent running the action, 'population', which captures the status of all of the agents in the simulation at time t-1 (one step before the current time step), and 'environment', which is the status of the environment at time t-1.

*Note that agents can only directly affect themselves.* Agents can read parameters from the environment and from other agents but they cannot directly change them. Changing the environment is the role of the referee.

If you want one agent to affect another agent, the mechanism is to record that the agent is *attempting* to affect another. The referee decides at the end of the time step what the outcome of this attempt was and makes any necessary changes. For an example of how this works, see 'Agents vs. Zombies' below. (coming soon)

``` r
agent_task <- create_agent_task({
  print(glue::glue("hello, I'm agent {self$id}"))
})

agent_population <- agent_population %>%
  set_task(agent_task)
```

Optionally, your sim can also have a referee. The referee doesn't have any parameters but they do have a task, which in this minimal example, is again just to say 'hello'.

``` r
referee_task <- create_referee_task({
  print("hello, I'm the referee")
})

referee <- referee() %>% 
  set_task(referee_task)
```

Now we're ready to run the sim!

``` r
sim <- agent_population %>% 
  agent_sim(time_total = total_time, environment = init_env, referee = referee) %>% 
  run_sim()
```

And we can use parse_history() to access details about what was happening to the agents and environment in each time step.

``` r
sim %>% 
  parse_history('agents')

sim %>% 
  parse_history('environment')
```
