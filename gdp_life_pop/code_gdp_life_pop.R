# packages & setup --------------------------------------------------------
# Install packages if not already installed
if (!require("conflicted")) install.packages("conflicted")
if (!require("gapminder")) install.packages("gapminder")
if (!require("ggh4x")) install.packages("ggh4x")
if (!require("grid")) install.packages("grid")
if (!require("ggtext")) install.packages("ggtext")
if (!require("scales")) install.packages("scales")
if (!require("showtext")) install.packages("showtext")
if (!require("tidyverse")) install.packages("tidyverse")

# Load packages
library(conflicted)
library(gapminder)
library(ggh4x)
library(ggtext)
library(scales)
library(showtext)
library(tidyverse)

# function conflicts
conflicts_prefer(dplyr::filter)

# font setup
showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Kanit", "kanit")

# Data import & prep ------------------------------------------------------
dat <- gapminder::gapminder %>%
  filter(year == 1952 | year == 2007) %>%
  filter(
    country %in% c(
      "Canada",
      "Germany",
      "Japan",
      "Netherlands",
      "Nigeria",
      "Vietnam",
      "Zimbabwe"
    )
  ) %>%
  mutate(year = as.factor(year)) %>%
  droplevels()

sorted_countries <- dat %>%
  filter(year == "2007") %>%
  arrange(lifeExp) %>%
  pull(country) %>%
  as.character()

dat <- dat %>% 
  mutate(country = fct_relevel(country, sorted_countries))

dat_long <- dat %>%
  pivot_longer(
    cols = c(lifeExp, pop, gdpPercap),
    names_to = "statistic",
    values_to = "value"
  )

dat_all_wide <- dat_long %>% 
  pivot_wider(names_from = year, values_from = value, names_prefix = 'year_') %>% 
  mutate(
    max_x = pmax(year_2007, year_1952),
    diff = year_2007 - year_1952,
    diff_lab = sprintf("%+d", round(diff))
  )

dat_long <- dat_long %>% 
  mutate(value_lab = case_when(
    statistic == "lifeExp"   ~ number(value, accuracy = 1),
    statistic == "pop"       ~ number(value, accuracy = 1,   scale = 1/1000000, suffix = "m"),
    statistic == "gdpPercap" ~ number(value, accuracy = 0.1, scale = 1/1000,    suffix = "k")
  ))

dat_all_wide <- dat_all_wide %>%
  mutate(
    diff_lab = case_when(
      statistic == "lifeExp"   ~ number(
        diff,
        style_positive = "plus",
        style_negative = "minus",
        accuracy = 1
      ),
      statistic == "pop"       ~ number(
        diff,
        style_positive = "plus",
        style_negative = "minus",
        accuracy = 1,
        scale = 1 / 1000000,
        suffix = "m"
      ),
      statistic == "gdpPercap" ~ number(
        diff,
        style_positive = "plus",
        style_negative = "minus",
        accuracy = 0.1,
        scale = 1 / 1000,
        suffix = "k"
      )
    ),
    x_pos_lab = case_when(
      statistic == "lifeExp"   ~ max_x + 3,
      statistic == "pop"       ~ max_x + 10000000,
      statistic == "gdpPercap" ~ max_x + 3000
    )
  )

# ggplot ------------------------------------------------------------------
theme_nature <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "kanit"),
      plot.title.position = "plot",
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = ggtext::element_textbox_simple(size = 10, margin = margin(0, 0, 10, 0)),
      axis.line.y = element_blank(),
      axis.text.x = element_text(color = "#AAAAAA"),
      axis.ticks.x = element_line(color = "#AAAAAA", linewidth = 0.4),
      axis.ticks.length.x = unit(4, "pt"),
      axis.line.x = element_line(color = "black", linewidth = 0.6),
      legend.position = "top",
      legend.box.just = "left",
      legend.justification = "left",
      legend.margin = margin(0, 0, 0,-57, unit = "pt"),
      legend.title = element_text(face = "bold"),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        linetype = "dotted",
        color = "#AAAAAA",
        linewidth = 0.3
      )
    )
}

year_colors <- c("1952" = "#F7AA59", "2007" = "#37A9E1")

another_long_subtitle <- paste0("In <b style='color:", year_colors[["2007"]], ";'>2007</b>, compared to <b style='color:", year_colors[["1952"]],";'>1952</b>, life expectancy, Gross Domestic Product (GDP) per capita, and population data collectively reflect significant advancements in health, economic prosperity, and demographic trends, illustrating an overall enhanced quality of life.")

facet_labels <- c(lifeExp = "Life Expectancy [years]", pop = "Population", gdpPercap = "GDP per Capita [$]")

p <- ggplot(data = dat_long) +
  aes(x = value, y = country, color = fct_rev(year)) +
  geom_segment(
    data = dat_all_wide,
    aes(x = year_1952, xend = year_2007, y = country, yend = country),
    color = "#AAAAAA",
    linewidth = 1
  ) +
  geom_point(size = 3) +
  geom_text(
    mapping = aes(label = value_lab, vjust = if_else(year == "1952", 2, -1)),
    size = 2.5,
    family = "kanit"
  ) +
  geom_text(
    data = dat_all_wide,
    mapping = aes(x = x_pos_lab, label = diff_lab),
    size = 2.5,
    hjust = 0,
    color = "#AAAAAA",
    family = "kanit"
  ) + 
  facet_wrap(
    facets = ~ statistic,
    scales = "free_x",
    labeller = labeller(statistic = facet_labels)
  ) +
  scale_color_manual(
    name = "Year",
    limits = c("1952", "2007"), 
    values = year_colors,
    guide = "none"
  ) +
  scale_y_discrete(
    name = NULL,
  ) + 
  scale_x_continuous(
    name = NULL,
  ) +
  labs(
    title = "GDP, LIFE EXPECTANCY & POPULATION",
    subtitle = another_long_subtitle
  ) +
  theme_nature() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = NA, color = "black")
  ) +
  facetted_pos_scales(
    x = list(
      statistic == "lifeExp" ~ scale_x_continuous(
        limits = c(35, 90),
        breaks = breaks_width(20),
        labels = number_format(accuracy = 1)
      ),
      statistic == "pop" ~ scale_x_continuous(
        limits = c(0, 150000000),
        expand = expansion(mult = c(0.05, 0.2)),
        breaks = breaks_width(50000000),
        labels = number_format(
          accuracy = 1,
          scale = 1 / 1000000,
          suffix = "m"
        )
      ),
      statistic == "gdpPercap" ~ scale_x_continuous(
        limits = c(0, 50000),
        expand = expansion(mult = c(0.075, 0.075)),
        breaks = breaks_width(25000),
        labels = number_format(
          accuracy = 1,
          scale = 1 / 1000,
          suffix = "k"
        )
      )
    )
  ) + 
  xlab(NULL) # apparently necessary?!

# Export ------------------------------------------------------------------
# ggsave(
#   filename = "gdp_life_pop/gdp_life_pop.png", 
#   plot = p,
#   width = 15, 
#   height = 11, 
#   units = "cm", 
#   dpi = 300
# )

# somehow ggsave is messing up the subtitle,
# so I go with png() instead:

png(
  filename = "gdp_life_pop/gdp_life_pop.png", 
  width = 15, 
  height = 11, 
  units = "cm", 
  res = 300
)
print(p)
dev.off()

system('open "gdp_life_pop/gdp_life_pop.png"')
