# Load packages -----------------------------------------------------------

library(dplyr)       # CRAN v1.0.2
library(tidyr)       # CRAN v1.1.2
library(forcats)     # CRAN v0.5.0
library(stringr)     # CRAN v1.4.0
library(ggplot2)     # CRAN v3.3.2
library(slider)      # CRAN v0.1.5
library(jabr)        # CRAN v0.1.2
library(sf)          # CRAN v0.9-5
library(scales)      # CRAN v1.1.1
library(hrbrthemes)  # CRAN v0.8.0
library(gghighlight) # CRAN v0.3.0
library(patchwork)   # CRAN v1.0.1

# Load data ---------------------------------------------------------------

jabar_basemap <- jabr_basemap(level = "district")
load("data/jabar_risk.rda")
load("data/jabar_mobility.rda")
load("data/jabar_stayput.rda")

# Mobility ----------------------------------------------------------------

jabar_mobility_plot <-
  jabar_mobility %>% 
  mutate(
    across(retail_and_recreation:residential, ~ slide_dbl(.x, mean, .before = 3, .after = 3))
  ) %>% 
  pivot_longer(
    cols = c(retail_and_recreation:residential),
    names_to = "category",
    values_to = "pct_changes"
  ) %>% 
  drop_na() %>% 
  mutate(
    category = fct_recode(
      category,
      "Residential" = "residential", 
      "Grocery And Pharmacy" = "grocery_and_pharmacy", 
      "Workplaces" = "workplaces", 
      "Transit Stations" = "transit_stations", 
      "Retail And Recreation" = "retail_and_recreation", 
      "Parks" = "parks"
    ),
    category = fct_reorder2(category, date, pct_changes)
  ) %>% 
  ggplot(aes(date, pct_changes, group = category)) +
  facet_wrap(~category) +
  geom_line(colour = "#982649", size = 1.1) +
  gghighlight(
    unhighlighted_params = list(
      colour = "#ACBEA3",
      alpha = 0.5,
      size = 0.8
    ),
    use_direct_label = FALSE
  ) +
  geom_text(
    data = ~ .x %>% 
      group_by(category) %>% 
      slice_tail(n = 1) %>% 
      ungroup(),
    aes(label = percent(pct_changes)), 
    nudge_x = 3, 
    hjust = "left",
    family = "Public Sans"
  ) +
  scale_y_percent() +
  labs(
    x = NULL,
    y = NULL,
    title = "Movement changes during COVID-19 pandemic",
    caption = "Source: Google Mobility Report"
  ) +
  coord_cartesian(clip = "off") +
  theme_ipsum_pub(grid = "Y")

jabar_mobility_plot

# Risk map ----------------------------------------------------------------

jabar_risk_plot <-
  jabar_basemap %>%
  left_join(jabar_risk) %>%
  ggplot(aes(fill = risk)) +
  geom_sf(colour = "grey40",
          size = 0.15,
          show.legend = FALSE) +
  geom_sf_text(
    data = ~ .x %>%
      mutate(label = if_else(
        risk == "high", str_to_title(name_kemendagri), NA_character_
      )),
    aes(label = label),
    colour = "gray30",
    family = "Public Sans"
  ) +
  scale_fill_manual(
    values = c(
      "low" = "#04724D",
      "medium" = "#FFDD4A",
      "high" = "#994636"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "COVID-19 risk map",
    caption = "Source: covid19.go.id"
  ) +
  theme_ipsum_pub()

jabar_risk_plot

# Stayput -----------------------------------------------------------------

jabar_stayput_plot <-
  jabar_stayput %>% 
  left_join(
    jabar_risk %>% 
      transmute(
        district = str_to_title(name_kemendagri),
        risk
      )
  ) %>% 
  ggplot(aes(date, stayput, colour = risk)) +
  geom_jitter(size = 0.8, alpha = 0.8, show.legend = FALSE) +
  geom_line(
    data = ~ .x %>% 
      group_by(date) %>% 
      summarise(
        movement_changes = median(movement_changes),
        stayput = median(stayput)
      ) %>% 
      mutate(
        roll = slide_dbl(stayput, mean, .before = 1, .after = 1)
      ),
    aes(y = roll), 
    colour = "#FF0055", 
    size = 1.2) +
  scale_x_date(
    breaks = "14 days",
    guide = guide_axis(check.overlap = TRUE),
    labels = label_date(format = "%e %b"),
    expand = c(0.005, 0.005)
  ) +
  scale_y_percent(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "low" = "#04724D",
      "medium" = "#FFDD4A",
      "high" = "#994636"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Proportion of people who stay-at-home",
    caption = "Source: Facebook Movement Range"
  ) +
  theme_ipsum_pub(grid = FALSE)

jabar_stayput_plot

jabar_basemap %>% 
  left_join(
    jabar_stayput %>% 
      filter(date == max(date)) %>% 
      mutate(district = str_to_upper(district)),
    by = c("name_kemendagri" = "district")
  ) %>% 
  ggplot(aes(fill = stayput)) +
  geom_sf(colour = "grey40",
          size = 0.15,
          show.legend = TRUE) +
  scale_fill_viridis_c(labels = percent_format()) +
  labs(
    x = NULL,
    y = NULL,
    fill = "% stay-at-home",
    title = "Stayput map",
    caption = "Source: Facebook Movement Range Maps"
  ) +
  theme_ipsum_pub()

# Composing plot ----------------------------------------------------------

jabar_risk_plot + jabar_stayput_plot
jabar_risk_plot | jabar_stayput_plot
jabar_risk_plot / jabar_stayput_plot

jabar_mobility_plot + (jabar_risk_plot / jabar_stayput_plot)

jabar_risk_plot + jabar_stayput_plot +
  plot_layout(widths = c(2, 1))

jabar_mobility_plot + (jabar_risk_plot / jabar_stayput_plot) +
  plot_layout(widths = c(2, 1))

wrap_plots(jabar_risk_plot, jabar_stayput_plot, nrow = 1)

jabar_risk_plot + jabar_stayput_plot &
  theme_modern_rc(grid = FALSE)

bareplot <- 
  jabar_risk_plot + jabar_stayput_plot &
  theme_modern_rc(grid = FALSE) 

bareplot +
  plot_annotation(
    title = "How are we doing?",
    subtitle = "Workshop",
    caption = "ggplot2"
  )
