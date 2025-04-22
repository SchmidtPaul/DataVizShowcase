library(broom)
library(camcorder)
library(conflicted)
library(scales)
library(tidyverse)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

dat <- datasets::penguins %>%
  as_tibble() %>%
  drop_na(body_mass) %>% 
  select(species, body_mass = flipper_len)

agg <- dat %>% 
  summarise(
    body_mass = mean(body_mass),
    .by = "species"
  ) %>% 
  mutate(
    mean_lab = number(body_mass, accuracy = 1)
  )

model <- lm(
  body_mass ~ species,
  data = dat
)

mod <- tidy(model) %>% 
  filter(!str_detect(term, "Intercept")) %>%
  transmute(
    species = str_remove(term, "species"),
    val = estimate,
    lab = number(
      x = estimate, 
      accuracy = 1,
      style_positive = "plus"
    )
  ); mod

# plot setup --------------------------------------------------------------
camcorder::gg_record(
  device = "png",
  width = 6, 
  height = 4,
  units = "in", 
  dpi = 300
)

theme_set(theme_minimal(base_size = 15))

theme_update(
  panel.grid.major = element_line(color = "grey92", size = .4),
  panel.grid.minor = element_blank(),
  axis.title.x = element_text(color = "grey30", margin = margin(t = 7)),
  axis.title.y = element_text(color = "grey30", margin = margin(r = 7)),
  axis.text = element_text(color = "grey50"),
  axis.ticks =  element_line(color = "grey92", size = .4),
  axis.ticks.length = unit(.6, "lines"),
  legend.position = "top",
  plot.title = element_text(
    hjust = 0,
    color = "black",
    # family = "Neutraface 2 Display Titling",
    size = 21,
    margin = margin(t = 10, b = 35)
  ),
  plot.subtitle = element_text(
    hjust = 0,
    face = "bold",
    color = "grey30",
    # family = "Neutraface Text Book Italic",
    size = 14,
    margin = margin(0, 0, 25, 0)
  ),
  plot.title.position = "plot",
  plot.caption = element_text(
    color = "grey50",
    size = 10,
    hjust = 1,
    # family = "Neutraface Display Medium",
    lineheight = 1.05,
    margin = margin(30, 0, 0, 0)
  ),
  plot.caption.position = "plot",
  plot.margin = margin(rep(20, 4))
)

pal <- c("#FF8C00", "#A034F0", "#159090")

p <- ggplot(data = dat) +
  aes(y = body_mass) +
  scale_y_continuous(
    name = "Body Mass",
    label = label_number(accuracy = 1),
  ) +
  theme(
    axis.title.x = element_blank()
  )


# Plot 1 ------------------------------------------------------------------
p1 <- p +
  geom_point(
    mapping = aes(x = "Penguins"),
    color = "grey30",
    position = position_jitter(
      width = 0.1,
      height = 0,
      seed = 42
    )
  ); p1


# Plot 2 ------------------------------------------------------------------
p2 <- p +
  geom_point(
    mapping = aes(x = species, color = species), 
    position = position_jitter(
      width = 0.1,
      height = 0,
      seed = 42
  )) +
  scale_color_manual(
    guide = "none",
    values = pal
  ) +
  scale_fill_manual(
    guide = "none",
    values = pal
  ); p2


p2 +
  geom_point(
    data = agg,
    aes(x = species, color = species),
    size = 10,
    shape = 95,
    position = position_nudge(x = 0.225)
  ) +
  geom_hline(
    yintercept = 190,
    linetype = "dashed"
  ) +
  geom_text(
    data = agg,
    aes(x = species, color = species, label = mean_lab),
    size = 4,
    hjust = 0,
    position = position_nudge(x = 0.325)
  ) +
  geom_text(
    data = mod,
    aes(x = species, y = 190, label = lab),
    size = 4,
    vjust = 1.2,
    hjust = 0.5,
    position = position_nudge(x = 0.225)
  )
