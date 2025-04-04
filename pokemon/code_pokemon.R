library(tidyverse)

if (!file.exists("pokemon/dat_pokemon.csv")) {
  source("pokemon/get_data_pokemon.R")
}

dat <- read_csv(
  file = "pokemon/dat_pokemon.csv",
  col_types = cols(
    pokemon = col_character(),
    id = col_double(),
    from = col_character(),
    to = col_character(),
    gen_id = col_double(),
    evo_id = col_double(),
    url_icon = col_character(),
    url_image = col_character()
  )
)


dat

