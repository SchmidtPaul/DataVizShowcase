library(tidyverse)
library(ggimage)
library(glue)

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

dat <- dat %>% 
  mutate(evo_id = as.factor(evo_id))

dat <- dat %>% 
  mutate(
    id_is_new = id > 151
  ) %>% 
  group_by(evo_id) %>%
  mutate(
    evo_id_has_new = any(id_is_new)
  ) %>%
  ungroup()

dat %>% 
  filter(evo_id_has_new) %>% 
  arrange(evo_id) %>% glimpse()

dat <- dat %>% 
  filter(evo_id_has_new)


# Plot --------------------------------------------------------------------
p <- ggplot(dat, aes(x = stage, y = evo_id)) +
  facet_wrap(facets = vars(evo_id), ncol = 3, scales = "free_y") +
  geom_point(
    data = dat %>% filter(id_is_new),
    size = 10,
    color = "red"
  ) +
  geom_image(
    data = dat,
    aes(image = path_icon), size = 0.05
  ) +
  theme_minimal() +
  labs(
    title = "Pokemon Evolution Chains",
    x = "Evolution Stage",
    y = "Evolution Chain ID"
  )


out_path <- "pokemon/test.pdf"

ggsave(
  filename = out_path,
  plot = p,
  width = 5,
  height = 7,
  dpi = 300
)

system(glue('open "{out_path}"'))

