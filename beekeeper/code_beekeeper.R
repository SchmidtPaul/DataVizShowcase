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

# Font setup
showtext_auto()
font_add_google(name = "Fira Sans", family = "Fira")

set.seed(43)
colors <- c(grey = "#492726", yellow = "#fabb00", beige = "#f0f1f1", blue = "#124E78", red = "#6e0e0a", orange = "#F19143")

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
  pivot_longer(-id) %>%
  mutate(
    age_now = as.factor(name) %>%
      fct_recode(!!!c("When they started" = "age_start", "Currently" = "age_now")) %>%
      fct_relevel(c("When they started", "Currently")),
    name = NULL
  ) %>% 
  na.omit()

summary_left <- dat_left %>% 
  summarize(mean = mean(value), .by = age_now) %>% 
  mutate(mean_label = glue("{round(mean)} years"))

lab_left <- dat_left %>% 
  mutate(min70 = value>=70) %>% 
  group_by(age_now) %>% 
  count(min70) %>% 
  mutate(p = n/sum(n),
         perc = percent(p, accuracy = 1, suffix = " %")) %>% 
  ungroup()

arrow_left <- tibble(
  x = 1.5,
  y = 83,
  xend = 1.75,
  yend = 75,
  label = glue("{lab_left[4, 'perc']} of all beekeepers<br>are at least 70 years old.")
)


p_left <- ggplot(dat_left,
                aes(y = value, x = age_now)) +
  # Hexagons
  geom_star(
    aes(fill = "all"),
    color = colors[["grey"]],
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
    aes(x = age_now, ymax = mean, ymin = mean),
    linewidth = 0.5,
    color = colors[["grey"]],
    linetype = "longdash",
    inherit.aes = FALSE,
    width = 0.95
  ) +
  # Mean label pt.1
  geom_richtext(
    data = summary_left,
    aes(x = age_now, y = mean, label = mean_label),
    hjust = 0, vjust = -0.1,
    nudge_x = -0.48,
    family = "Fira",
    size = 10 * 0.35,
    color = colors[["grey"]],
    fill = "white",
    label.color = NA,
    label.padding = unit(0.02, "lines")
  ) +
  # Mean label pt.2
  geom_richtext(
    data = summary_left,
    aes(x = age_now, y = mean, label = "on average"),
    hjust = 0, vjust = 1.2,
    nudge_x = -0.48,
    family = "Fira",
    size = 10 * 0.35,
    color = colors[["grey"]],
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
    color = colors[["grey"]],
    fill = colors[["beige"]],
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
    color = colors[["grey"]],
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
    color = colors[["grey"]],
    fill = "white",
    label.color = NA
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 90, 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_discrete(
    name = NULL,
    labels =
      c("age_start" = "How old were you when you started beekeeping?", "age_now" = "How old are you?")
  )+
  coord_cartesian(clip = 'off') +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = "Fira", color = colors[["grey"]], size = 12, face = "bold"),
    axis.text.x = element_text(family = "Fira", color = colors[["grey"]], size = 12),
    panel.grid.major.y = element_line(linetype = "dotted", color = colors[["grey"]], linewidth = 0.3),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Fira", color = colors[["grey"]], size = 12),
    legend.position = c(1, 0.011),
    legend.justification = c(1, 0),
    legend.margin = margin(t = 0, r = 5, b = 5, l = 0),
    legend.background = element_rect(
      fill = colors[["beige"]],
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
  x = 125,
  y = 13,
  xend = 52.5,
  yend = 11.75,
  label = "During the period of German reunification, significantly fewer people temporarily started beekeeping."
) %>% mutate(label = str_wrap(label, 50) %>% str_replace_all("\n", "<br>"))

p_right <-
  ggplot(summary_right, aes(x = n, y = start_fac, label = perc)) +
  geom_bar(
    stat = "identity",
    color = colors[["grey"]],
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
    color = colors[["grey"]],
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
    color = colors[["grey"]],
    fill = colors[["beige"]],
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
    color = colors[["grey"]],
    arrow = arrow(30L, unit(0.075, "inches"),
                  "last", "closed"),
    inherit.aes = FALSE
  ) +
  scale_y_discrete(name = "When did you start beekeeping?*",
                   expand = expansion(mult = c(0.01, 0.01))) +
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
      color = colors[["grey"]],
      size = 12
    ),
    axis.text.y = element_text(
      face = "bold",
      family = "Fira",
      color = colors[["grey"]],
      size = 12
    ),
    axis.title.x = element_text(
      family = "Fira",
      color = colors[["grey"]],
      size = 14
    ),
    axis.title.y = element_text(
      family = "Fira",
      color = colors[["grey"]],
      size = 14
    ),
    panel.grid.major.x = element_line(
      linetype = "dotted",
      color = colors[["grey"]],
      linewidth = 0.3)
  )

bee_img <- image_read("beekeeper/honeybee.png") %>% 
  image_rotate(-90) %>% 
  rasterGrob()

p_right <- p_right +
  annotation_custom(bee_img, xmin = 180, xmax = 260, ymin = -Inf, ymax = 10)

# Gesamt ------------------------------------------------------------------
title <- "Beekeeping in Mecklenburg-Vorpommern, Germany"
subtitle <- "The Epidemiological Service/Animal Disease Control Service of the State Office for Agriculture, Food Safety and Fisheries (LALLF) MV conducted a survey on the current situation of beekeeping in Mecklenburg-Vorpommern at the end of 2015. Over 1,200 beekeepers in the state provided information on various aspects of beekeeping using a questionnaire."
caption <- "*The year category labels are simplified: The category '\U2264 2005' for example only refers to the years 2001-2005.\nSource: Beekeeper Survey MV 2015 Results (BioMath, 2016): https://www.lallf.de/fileadmin/media/PDF/tiergseuchendia/epidem/Final__Imkerumfrage_MV_2015_Ergebnisse.pdf; Figure: https://www.dlf.pt/ddetail/hxRRJRJ_transparent-honey-bee-png-bee-drawing-png-png/"

p <- p_left + p_right +
  plot_annotation(title = title,
                  subtitle = str_wrap(subtitle, 150),
                  caption = caption,
                  theme = theme(
                    plot.title = element_text(
                      size = 24,
                      family = "Calibri",
                      face = "bold",
                      color = colors[["grey"]]
                    ),
                    plot.subtitle = element_text(
                      size = 12,
                      family = "Calibri",
                      color = colors[["grey"]]
                    ),
                    plot.caption = element_text(
                      size = 7,
                      family = "Calibri",
                      color = colors[["grey"]]
                    )
                  ))

BioMathR::gg_export(
  plot_obj = p,
  file_name = "Imker",
  width_cm = 32,
  height_cm = 18,
  pdf = "open",
  png = "none",
  svg = "none"
)
