library(tidyverse)
library(rjson)
library(stringr)

url_simulation_data <- "https://projects.fivethirtyeight.com/trump-biden-election-map/simmed-maps.json"

simulations_data <- fromJSON(file = url_simulation_data)

states <- simulations_data$states
n_simulations <- length(simulations_data$maps)

trump_victory_maps <- tibble(
  scenario = c(),
  n = c()
)

for (i in 1:n_simulations){
  if (simulations_data$maps[[i]][1]) {
    scenario <- paste(
      states,
      tail(simulations_data$maps[[i]], 59 - 3) > 0,
      sep = ":",
      collapse = ","
      )
    
    if (!(scenario %in% trump_victory_maps$scenario)) {
      trump_victory_maps <- add_row(trump_victory_maps, scenario = scenario, n = 1)
    } else {
      trump_victory_maps$n[trump_victory_maps$scenario == scenario] <- trump_victory_maps$n[trump_victory_maps$scenario == scenario] + 1
    }
  }
}

trump_victory_maps <- trump_victory_maps %>%
  separate(
    -n,
    states,
    sep = ","
  ) %>%
  mutate_at(vars(-n), ~ as.logical(gsub(".*:", "", .x))) %>%
  arrange(desc(n))

trump_wins_probability_by_state <- trump_victory_maps %>%
  summarize_at(
    vars(-n),
    ~ weighted.mean(.x, n)
    )
