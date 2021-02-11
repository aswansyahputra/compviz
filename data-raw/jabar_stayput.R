library(rvest)
library(curl)
library(tidyverse)

stayput_download <- 
  read_html("https://data.humdata.org/dataset/movement-range-maps") %>% 
  html_node("li.resource-item:nth-child(1) > div:nth-child(4) > a:nth-child(1)") %>% 
  html_attr("href") %>% 
  paste0("https://data.humdata.org", .)

stayput_filename <- tempfile(fileext = ".zip")

if (!file.exists(stayput_filename)) {
  curl_download(
    url = stayput_download,
    destfile = stayput_filename,
    quiet = FALSE
  )
}

global_stayput <- 
  read_tsv(
    file = unzip(
      zipfile = stayput_filename,
      files = unzip(
        zipfile = stayput_filename,
        list = TRUE
      ) %>% 
        filter(str_detect(Name, "movement-range")) %>% 
        pull(Name)
    )
  )

jabar_stayput <-
  global_stayput %>% 
  filter(country == "IDN") %>% 
  filter(
    str_extract(polygon_id, "IDN.\\d{1}") == "IDN.9"
  ) %>% 
  transmute(
    district = polygon_name,
    district = recode(district, Cimahi = "Kota Cimahi", Banjar = "Kota Banjar", Depok = "Kota Depok"),
    district = case_when(
      str_detect(district, "Kota", negate = TRUE) ~ paste("Kab.", district),
      TRUE ~ district
    ),
    date = ds,
    movement_changes = all_day_bing_tiles_visited_relative_change,
    stayput = all_day_ratio_single_tile_users
  )

basename(stayput_download) %>% 
  str_remove("data-") %>% 
  str_replace("zip", "txt") %>% 
  file.remove()

usethis::use_data(jabar_stayput, overwrite = TRUE)
