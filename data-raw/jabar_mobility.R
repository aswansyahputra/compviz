library(tidyverse)

global_mobility <-
  read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=e0c5a582159f5662",
    col_types = cols(
      country_region_code = col_character(),
      country_region = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      date = col_date(format = ""),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double()
    )
  )

jabar_mobility <-
  global_mobility %>%
    filter(
      country_region == "Indonesia",
      sub_region_1 == "West Java"
    ) %>% 
    rename_with(~ str_remove(.x, "_percent_change_from_baseline")) %>% 
    select(
      date:residential
    ) %>% 
  mutate(across(retail_and_recreation:residential, ~ .x / 100))

usethis::use_data(jabar_mobility, overwrite = TRUE)
