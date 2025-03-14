# packages ----------------------------------------------------------------
# Install packages if not already installed
if (!require("conflicted")) install.packages("conflicted")
if (!require("ggdist")) install.packages("ggdist")
if (!require("glue")) install.packages("glue")
if (!require("grid")) install.packages("grid")
if (!require("ggstar")) install.packages("ggstar")
if (!require("ggtext")) install.packages("ggtext")
if (!require("magick")) install.packages("magick")
if (!require("patchwork")) install.packages("patchwork")
if (!require("scales")) install.packages("scales")
if (!require("showtext")) install.packages("showtext")
if (!require("tidyverse")) install.packages("tidyverse")

# Load packages
library(conflicted)
library(ggdist)
library(glue)
library(ggstar)
library(ggtext)
library(grid)
library(patchwork)
library(scales)
library(showtext)
library(tidyverse)

# function conflicts
conflicts_prefer(dplyr::filter)

set.seed(43)

# Font setup
showtext_opts(dpi = 300)
showtext_auto()
font_add_google(name = "Fira Sans", family = "Fira")

colors <- c(brown = "#492726", yellow = "#fabb00", grey = "#f0f1f1")

# Data import & prep ------------------------------------------------------
dat <- read_csv(
  file = "beekeeper/dat_beekeeper.csv",
  col_types = cols(
    id = col_character(),
    age_now = col_double(),
    age_start = col_double(),
    year_start = col_double(),
    duration = col_double()
  )
)


## Plot on the left -------------------------------------------------------
dat_left <- dat %>%
  select(id, age_now, age_start) %>%
  pivot_longer(
    cols = c(age_start, age_now),
    names_to = "time_point",
    values_to = "age"
  ) %>%
  mutate(time_point = factor(
    time_point,
    levels = c("age_start", "age_now"),
    labels = c("When they started", "Currently")
  )) %>%
  na.omit()

summary_left <- dat_left %>%
  summarize(mean = mean(age), .by = time_point) %>%
  mutate(mean_label = glue("{round(mean)} years"))

lab_left <- dat_left %>%
  filter(time_point == "Currently") %>%
  summarize(perc = percent(mean(age >= 70), accuracy = 1)) %>% 
  pull(perc)

arrow_left <- tibble(
  x = 1.5,
  y = 85,
  xend = 1.75,
  yend = 75,
  label = glue("{lab_left} of all beekeepers<br>are at least 70 years old.")
)


p_left <- ggplot(dat_left) +
  aes(y = age, x = time_point) +
  # Hexagons
  geom_star(
    aes(fill = "all"),
    color = colors[["brown"]],
    position = position_jitter(width = 0.2),
    starshape = "hexagon",
    alpha = 0.9,
    size = 2
  ) +
  # Legend
  scale_fill_manual(
    name = NULL,
    values = c("all" = colors[["yellow"]]),
    labels = c("all" = "Beekeeper")
  ) +
  # Vertical Distribution
  stat_halfeye(
    fill = colors[["yellow"]],
    alpha = 0.66,
    adjust = .5,
    width = .2,
    justification = -1.3,
    .width = 0,
    point_colour = NA
  ) +
  # Mean Bar
  geom_errorbar(
    data = summary_left,
    aes(x = time_point, ymax = mean, ymin = mean),
    linewidth = 0.5,
    color = colors[["brown"]],
    linetype = "longdash",
    inherit.aes = FALSE,
    width = 0.95
  ) +
  # Mean label pt.1
  geom_richtext(
    data = summary_left,
    aes(x = time_point, y = mean, label = mean_label),
    hjust = 0, vjust = -0.1,
    nudge_x = -0.48,
    family = "Fira",
    size = 10 * 0.35,
    color = colors[["brown"]],
    fill = "white",
    label.color = NA,
    label.padding = unit(0.02, "lines")
  ) +
  # Mean label pt.2
  geom_richtext(
    data = summary_left,
    aes(x = time_point, y = mean, label = "on average"),
    hjust = 0, vjust = 1.2,
    nudge_x = -0.48,
    family = "Fira",
    size = 10 * 0.35,
    color = colors[["brown"]],
    fill = "white",
    label.color = NA,
    label.padding = unit(0.02, "lines")
  ) +
  # Arrow Labels
  geom_richtext(
    data = arrow_left,
    mapping = aes(
      x = 1.75, 
      y = 92.5,
      label = label
    ),
    hjust = 1, nudge_x = 0,
    vjust = 1, nudge_y = 0,
    family = "Fira",
    size = 9 * 0.35,
    color = colors[["brown"]],
    fill = colors[["grey"]],
    label.color = NA,
    label.padding = unit(0.25, "lines")
  ) +
  # Arrows
  geom_curve(
    data = arrow_left,
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    linewidth = 0.5,
    curvature = 0.5,
    color = colors[["brown"]],
    arrow = arrow(30L, unit(0.075, "inches"),
                  "last", "closed"),
    inherit.aes = FALSE
  ) +
  # y-Axis text
  geom_richtext(
    data = tibble(x = -Inf, y = 90, label = "**years of age**"),
    aes(x = x, y = y, label = label),
    family = "Fira",
    hjust = 0.05,
    vjust = 0.575,
    size = 12*0.35,
    color = colors[["brown"]],
    fill = "white",
    label.color = NA
  ) +
  # Format y-axis
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 90, 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  # Format x-axis
  scale_x_discrete(
    name = NULL,
    labels =
      c("age_start" = "How old were you when you started beekeeping?", 
        "age_now" = "How old are you?")
  ) +
  coord_cartesian(clip = 'off') +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = "Fira", color = colors[["brown"]], size = 12, face = "bold"),
    axis.text.x = element_text(family = "Fira", color = colors[["brown"]], size = 12),
    panel.grid.major.y = element_line(linetype = "dotted", color = colors[["brown"]], linewidth = 0.3),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Fira", color = colors[["brown"]], size = 12),
    legend.position = c(1, 0.011),
    legend.justification = c(1, 0),
    legend.margin = margin(t = 0, r = 5, b = 5, l = 0),
    legend.background = element_rect(
      fill = colors[["grey"]],
      colour = NA,
      size = 0.1
    )
  )


# Plot on the right -------------------------------------------------------
dat_right <- dat %>% 
  na.omit() %>% 
  arrange(year_start)

dat_right <- dat_right %>%
  mutate(
    start_num = ceiling(year_start / 5) * 5,
    start_fac = start_num %>%
      paste0("\U2264 ", .) %>%
      as.factor() %>%
      fct_reorder(start_num)
  ) 

summary_right <- dat_right %>%
  group_by(start_fac) %>%
  count() %>%
  ungroup() %>%
  mutate(p = n / sum(n),
         perc = percent(p, accuracy = 0.1, suffix = " %"))

arrow_right <- tibble(
  x = 110,
  y = 13,
  xend = 52.5,
  yend = 11.75,
  label = "During the period of German reunification,<br>significantly fewer people temporarily started beekeeping."
)

p_right <-
  ggplot(data = summary_right) +
  aes(x = n, y = start_fac, label = perc) +
  # Bars
  geom_bar(
    stat = "identity",
    color = colors[["brown"]],
    fill = colors[["yellow"]],
    show.legend = FALSE
  ) +
  # Percent Labels
  geom_richtext(
    hjust = 0,
    vjust = 0.5,
    nudge_x = 1,
    family = "Fira",
    size = 10 * 0.35,
    color = colors[["brown"]],
    fill = "white",
    label.color = NA,
    label.padding = unit(0.1, "lines")
  ) +
  # Arrow Labels
  geom_richtext(
    data = arrow_right,
    mapping = aes(
      x = x, 
      y = y,
      label = label
    ),
    hjust = 0, nudge_x = 1,
    vjust = 1, nudge_y = 0.25,
    family = "Fira",
    size = 9 * 0.35,
    color = colors[["brown"]],
    fill = colors[["grey"]],
    label.color = NA,
    label.padding = unit(0.25, "lines")
  ) +
  # Arrows
  geom_curve(
    data = arrow_right,
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    size = 0.5,
    curvature = 0.15,
    color = colors[["brown"]],
    arrow = arrow(30L, unit(0.075, "inches"),
                  "last", "closed"),
    inherit.aes = FALSE
  ) +
  # Format y-axis
  scale_y_discrete(
    name = "When did you start beekeeping?*",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  # Format x-axis
  scale_x_continuous(
    name = "Number of beekeepers",
    expand = expansion(mult = c(0, 0.1)),
    breaks = seq(0, 250, 50)
  ) +
  theme_classic() +
  theme(
    plot.margin = margin(l = 10, unit = "pt"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      face = "bold",
      family = "Fira",
      color = colors[["brown"]],
      size = 12
    ),
    axis.text.y = element_text(
      face = "bold",
      family = "Fira",
      color = colors[["brown"]],
      size = 12
    ),
    axis.title.x = element_text(
      family = "Fira",
      color = colors[["brown"]],
      size = 14
    ),
    axis.title.y = element_text(
      family = "Fira",
      color = colors[["brown"]],
      size = 14
    ),
    panel.grid.major.x = element_line(
      linetype = "dotted",
      color = colors[["brown"]],
      linewidth = 0.3)
  )


# Import and add bee image ------------------------------------------------
bee_img <- image_read("beekeeper/honeybee.png") %>% 
  image_rotate(-90) %>% 
  rasterGrob()

p_right <- p_right +
  annotation_custom(
    grob = bee_img,
    xmin = 180,
    xmax = 260,
    ymin = -Inf,
    ymax = 10
  )


# Gesamt ------------------------------------------------------------------
title <- "Beekeeping in Mecklenburg-Vorpommern, Germany"
subtitle <- "The 'Landesamt für Landwirtschaft, Lebensmittelsicherheit
und Fischerei' conducted a survey on the situation of beekeeping in Mecklenburg-Vorpommern at the end of 2015. Over 1,200 beekeepers in the state provided information on various aspects of beekeeping using a questionnaire."
caption <- "*The year category labels are simplified: The category '\U2264 2005' for example only refers to the years 2001-2005.\nSource: Beekeeper Survey MV 2015 Results (BioMath, 2016): https://www.lallf.de/fileadmin/media/PDF/tiergseuchendia/epidem/Final__Imkerumfrage_MV_2015_Ergebnisse.pdf; Figure: https://www.dlf.pt/ddetail/hxRRJRJ_transparent-honey-bee-png-bee-drawing-png-png/"

p <- p_left + p_right +
  plot_annotation(
    title = title,
    subtitle = str_wrap(subtitle, 150),
    caption = caption,
    theme = theme(
      plot.title = element_text(
        size = 24,
        family = "Fira",
        face = "bold",
        color = colors[["brown"]]
      ),
      plot.subtitle = element_text(
        size = 12,
        family = "Fira",
        color = colors[["brown"]]
      ),
      plot.caption = element_text(
        size = 7,
        family = "Fira",
        color = colors[["brown"]]
      )
    )
  )


# Export ------------------------------------------------------------------
ggsave(
  filename = "beekeeper/beekeeper.png", 
  plot = p,
  width = 32, 
  height = 18, 
  units = "cm", 
  dpi = 300
  )

# system('open "beekeeper/beekeeper.png"')
