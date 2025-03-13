# packages ----------------------------------------------------------------
# Install packages if not already installed
if (!require("conflicted")) install.packages("conflicted")
if (!require("ggtext")) install.packages("ggtext")
if (!require("glue")) install.packages("glue")
if (!require("scales")) install.packages("scales")
if (!require("showtext")) install.packages("showtext")
if (!require("tidyverse")) install.packages("tidyverse")

# Load packages
library(conflicted)
library(ggtext)
library(glue)
library(scales)
library(showtext)
library(tidyverse)

conflicts_prefer(dplyr::filter)

# Font setup
showtext_opts(dpi = 300)
showtext_auto()
font_add_google(name = "Roboto", family = "Roboto")

# Data import & prep ------------------------------------------------------
dat <- read_csv(
  file = "facet_heatmap/dat_facet_heatmap.csv",
  col_types = cols(
    partner = col_character(),
    domain = col_character(),
    n = col_double(),
    y_group = col_character(),
    y_group_order = col_double(),
    x_group = col_character(),
    x_group_order = col_double(),
    hmm = col_double()
  )
)

Q90 <- round(as.numeric(quantile(dat$n, 0.9)))
MAX <- max(dat$n)
col_vec <- c("#bce2cc", "#00923f", "#ad0000")

the_subtitle <- glue(
  "The plot is organized with 
  partners (rows) grouped by categories (right labels) and
  domains (columns) grouped by categories (top labels).
  White tiles indicate that no sources were identified.
  Only 10% of cases exceed
  <span style='color:{col_vec[[2]]}; font-weight:bold'>{Q90}</span>
  sources, with the maximum being
  <span style='color:{col_vec[[3]]}; font-weight:bold'>{MAX}</span>."
)


# ggplot ------------------------------------------------------------------
p <- ggplot(data = dat) +
  aes(x = domain, y = partner) +
  facet_grid(
    rows = vars(y_group),
    cols = vars(x_group),
    scales = "free",
    space = "free"
  ) +
  geom_tile(aes(fill = n)) +
  scale_fill_gradientn(
    guide = "none",
    name = "Anzahl",
    colors = col_vec,
    values = rescale(c(1, Q90, MAX)),
    breaks = c(1, Q90, MAX),
    na.value = "white"
  ) +
  labs(
    title = "Number of sources identified by domain and partner",
    subtitle = the_subtitle,
    caption = "Source: Modified, anonymized dataset",
    x = "Domain",
    y = "Partner"
  ) +
  geom_text(aes(label = n), color = "white", size = rel(2.8)) +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  theme(
    text = element_text(family = "Kanit"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_text(hjust = 0, face = "bold"),
    axis.title.y = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_markdown(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    strip.text.y = element_textbox(
      color = "grey60",
      vjust = 0,
      hjust = 0,
      orientation = "left-rotated"
    ),
    strip.text.x = element_textbox(
      color = "grey60",
      vjust = 0.5,
      hjust = 0,
      orientation = "left-rotated"
    )
  )


# Export ------------------------------------------------------------------
ggsave(
  filename = "facet_heatmap/facet_heatmap.png", 
  plot = p,
  width = 17, 
  height = 22, 
  units = "cm", 
  dpi = 300
)

# system('open "facet_heatmap/facet_heatmap.png"')
