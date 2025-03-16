# packages ----------------------------------------------------------------
# Install packages if not already installed
if (!require("conflicted")) install.packages("conflicted") # Manages package conflicts
if (!require("emmeans")) install.packages("emmeans")       # For estimated marginal means analysis
if (!require("ggtext")) install.packages("ggtext")         # For rich text annotations in ggplot2
if (!require("glue")) install.packages("glue")             # For string interpolation
if (!require("multcomp")) install.packages("multcomp")     # For multiple comparison procedures
if (!require("multcompView")) install.packages("multcompView") # For compact letter display
if (!require("patchwork")) install.packages("patchwork")   # For combining plots
if (!require("scales")) install.packages("scales")         # For scale transformations
if (!require("tidyverse")) install.packages("tidyverse")   # Collection of data science packages

# Load packages
library(conflicted)
library(emmeans)
library(ggtext)
library(glue)
library(multcomp)
library(multcompView)
library(patchwork)
library(scales)
library(tidyverse)

# Explicitly resolve function conflicts from different packages
conflicts_prefer(dplyr::filter)         # Use dplyr's filter function
conflicts_prefer(readr::col_factor)     # Use readr's col_factor function

# Data import & prep ------------------------------------------------------
# Read the CSV file containing variety and yield data
# Specifying column types: variety as a factor and yield as a double
dat <- read_csv(file = "treatment_means_comparison/dat_treatment_means_comparison.csv",
                col_types = cols(variety = col_factor(), yield = col_double()))

# model & anova -----------------------------------------------------------
# Fit a linear model with yield as the response and variety as the predictor
mod <- lm(yield ~ variety, data = dat)
# anova(mod)

# emmean comparison -------------------------------------------------------
# Get 95% confidence intervals and compact letter display using Fisher's LSD test
# The compact letter display groups varieties that are not significantly different
emm_95_cld <- emmeans(mod, specs = "variety", level = 0.95) %>% # Calculate estimated marginal means with 95% CI
  cld(adjust = "none", Letters = letters) %>%  # Add compact letter display (no adjustment = Fisher's LSD)
  as_tibble() %>%  # Convert to tibble for easier manipulation
  transmute(
    variety,
    emmean,
    cld = glue("{round(emmean, 1)}<sup>{str_trim(.group)}</sup>"), # Format for plot: mean with letter superscript
    lower_95 = lower.CL,  # Lower bound of 95% CI
    upper_95 = upper.CL   # Upper bound of 95% CI
  )

# Get 80% confidence intervals
# Often used in agricultural research to visualize trends
emm_80 <- emmeans(mod, specs = "variety", level = 0.80) %>%
  as_tibble() %>%
  transmute(variety, emmean, lower_80 = lower.CL, upper_80 = upper.CL)

# Join the 95% CI data with the 80% CI data
emm <- full_join(x = emm_95_cld,
                 y = emm_80,
                 by = join_by(variety, emmean))

# Create a function to add core plot elements -----------------------------
# This function adds raw data points, boxplots, confidence intervals, and means to the plot
add_plot_elements <- function(p, data_df, emm_df, nudge_box = -0.15, nudge_ci = 0.15, 
                              nudge_text = 0.25) {
  p <- p +
    # Black dots representing the individual raw data points
    geom_point(
      data = data_df,
      aes(y = yield),
      alpha = 0.5
    ) +
    # Boxplots showing distribution of raw data
    geom_boxplot(
      data = data_df,
      aes(y = yield),
      width = 0.1,
      position = position_nudge(x = nudge_box) # Shift boxplots slightly left
    ) +
    # 95% confidence interval of the adjusted means (light green)
    geom_linerange(
      data = emm_df,
      aes(ymin = lower_95, ymax = upper_95),
      color = "#bce2cc",
      linewidth = 4,
      position = position_nudge(x = nudge_ci) # Shift CIs slightly right
    ) +
    # 80% confidence interval of the adjusted means (dark green)
    geom_linerange(
      data = emm_df,
      aes(ymin = lower_80, ymax = upper_80),
      color = "#00923f",
      linewidth = 4,
      position = position_nudge(x = nudge_ci) # Shift CIs slightly right
    ) +
    # Green diamonds representing the adjusted means
    geom_point(
      data = emm_df,
      aes(y = emmean),
      shape = 23, # Diamond shape
      size = 2,
      fill = "#00923f",
      position = position_nudge(x = nudge_ci) # Shift means slightly right
    ) +
    # Green letters for compact letter display (statistical grouping)
    geom_richtext(
      data = emm_df,
      aes(y = emmean, label = cld),
      color = "#00923f",
      fill = NA,  # No background fill
      label.color = NA, # No border
      label.padding = unit(c(0, 0, 0, 0), "lines"), # No padding
      position = position_nudge(x = nudge_text), # Shift text right
      hjust = 0 # Left-align
    )
  
  return(p)
}

# Add legend annotations ---------------------------------------------------
# This function adds explanatory annotations to the legend
add_legend_annotations <- function(p, data_df) {
  info_color <- "grey60" # Color for all annotation text and arrows
  
  # Boxplot annotations - explains the elements of the boxplot
  p <- p +
    # Minimum value annotation
    annotate(
      "text", 
      x = 0.775, y = quantile(data_df$yield, probs = 0)-1,
      label = "Minimum",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrow pointing to minimum
    annotate(
      "curve",
      x = 0.8,
      y = quantile(data_df$yield, probs = 0)-1,
      xend = 0.85,
      yend = quantile(data_df$yield, probs = 0)-0.2,
      linewidth = 0.33,
      curvature = 0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    # First quartile (Q25) annotation
    annotate(
      "text", 
      x = 0.775, y = quantile(data_df$yield, probs = 0.25),
      label = "Q25",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Median annotation
    annotate(
      "text", 
      x = 0.775, y = median(data_df$yield, probs = 0.5),
      label = "Median",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Third quartile (Q75) annotation
    annotate(
      "text", 
      x = 0.775, y = quantile(data_df$yield, probs = 0.75),
      label = "Q75",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Maximum value annotation
    annotate(
      "text", 
      x = 0.775, y = quantile(data_df$yield, probs = 1)+1,
      label = "Maximum",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrow pointing to maximum
    annotate(
      "curve",
      x = 0.8,
      y = quantile(data_df$yield, probs = 1)+1,
      xend = 0.85,
      yend = quantile(data_df$yield, probs = 1)+0.2,
      linewidth = 0.33,
      curvature = -0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    # 95% Confidence Interval annotation
    annotate(
      "text", 
      x = 0.94, y = 36,
      label = "95 % Conf. Int.",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrow pointing to 95% CI
    annotate(
      "curve",
      x = 0.95,
      y = 36,
      xend = 1.15,
      yend = 33.8,
      linewidth = 0.33,
      curvature = -0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    # 80% Confidence Interval annotation
    annotate(
      "text", 
      x = 0.94, y = 25,
      label = "80 % Conf. Int.",
      hjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrow pointing to 80% CI
    annotate(
      "curve",
      x = 0.95,
      y = 25,
      xend = 1.15,
      yend = 27.4,
      linewidth = 0.33,
      curvature = 0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    # Estimated marginal mean annotation
    annotate(
      "text", 
      x = 1.4, y = 33.5,
      label = "Model-based mean\n(emmean; lsmean)",
      hjust = 0,
      vjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrows pointing to estimated marginal mean
    annotate(
      "curve",
      x = 1.39,
      y = 33.2,
      xend = 1.28,
      yend = 30.8,
      linewidth = 0.33,
      curvature = 0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    annotate(
      "curve",
      x = 1.39,
      y = 33.2,
      xend = 1.15,
      yend = 30.7,
      linewidth = 0.33,
      curvature = 0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    ) +
    # Compact letter display annotation
    annotate(
      "text", 
      x = 1.4, y = 27,
      label = "Compact letter display\n(details in caption)",
      hjust = 0,
      vjust = 1,
      size = 2,
      color = info_color,
    ) +
    # Arrow pointing to compact letter display
    annotate(
      "curve",
      x = 2,
      y = 27.5,
      xend = 1.65,
      yend = 30.3,
      linewidth = 0.33,
      curvature = 0.5,
      color = info_color,
      arrow = arrow(angle = 30L, length = unit(0.025, "inches"), ends = "last", type = "closed")
    )
  
  return(p)
}

# Main Plot ---------------------------------------------------------------
# Define caption text explaining the plot elements
cap <- glue("Raw data shown as black points with boxplots. \\
 Green diamonds represent model-based means with 95% (light green) and \\
 80% (dark green) confidence intervals. \\
 Statistical groupings shown via compact letter display notation: \\
 Means followed by a common letter are not significantly \\
 different according to Fisher's LSD-Test (\u03B1=0.05)."
)

# Create base plot with consistent settings for the main visualization
p <- ggplot() +
  aes(x = variety) +
  # Set x-axis labels for varieties
  scale_x_discrete(
    name = "Variety",
    labels = c("v1" = "Variety 1", "v2" = "Variety 2", "v3" = "Variety 3", "v4" = "Variety 4")
  ) +
  # Configure y-axis with proper units
  scale_y_continuous(
    name = "Yield [kg/ha]",
    limits = c(0, NA),  # Start at 0, no upper limit
    expand = expansion(mult = c(0, 0.05))  # Add 5% padding at top
  ) +
  # Add title and caption
  labs(
    title = "Comparison of Yields by Variety",
    caption = cap
  ) +
  # Use classic theme as base
  theme_classic() +
  # Customize theme elements
  theme(
    panel.grid.major.y = element_line(
      linewidth = 0.2,
      linetype = "dotted",
      color = "grey"
    ),
    plot.caption = element_textbox_simple(
      margin = margin(t = 10),
      size = 8,
      width = 0.53,
      hjust = 1,
      halign = 0
    ),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    axis.title.x = element_blank()
  )

# Add the plot elements to the base plot
main_plot <- add_plot_elements(p = p, data_df = dat, emm_df = emm)

# Custom Legend -----------------------------------------------------------
# Filter data for legend - using only variety v4 data for the legend example
legend_data <- dat %>% filter(variety == "v4")
legend_emm <- emm %>% filter(variety == "v4")

# Create a base plot for the legend with void theme (no axes or background)
legend_plot <- ggplot() +
  aes(x = variety) +
  theme_void() 

# Add the same plot elements and annotations to the legend plot
legend_plot <- legend_plot %>% 
  add_plot_elements(data_df = legend_data, emm_df = legend_emm) %>% 
  add_legend_annotations(data_df = legend_data) +
  # Set the coordinate system to focus on the legend area
  coord_cartesian(ylim = c(20, 40), xlim = c(0.7, 2))

# Combine plots -----------------------------------------------------------
# Add the legend as an inset to the main plot
final_plot <- main_plot +
  inset_element(legend_plot,
                left = 0.03,    # Position from left edge
                bottom = -0.1,  # Position below bottom edge
                right = 0.5,    # Width (to right edge)
                top = 0.3,      # Height (to top edge)
                align_to = 'full')  # Align relative to full plot area


# Export ------------------------------------------------------------------
# # Save the plot as a PDF file
# ggsave(
#   filename = "treatment_means_comparison/treatment_means_comparison.png", 
#   plot = final_plot,
#   width = 14, 
#   height = 12, 
#   units = "cm", 
#   dpi = 300
# )
# 
# # Open the PDF file automatically (works on macOS)
# system('open "treatment_means_comparison/treatment_means_comparison.png"')