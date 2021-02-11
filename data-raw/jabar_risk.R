library(httr)
library(tidyverse)

resp <- GET("https://data.covid19.go.id/public/api/skor.json")

jabar_risk <-
  content(resp, as = "parsed", simplifyVector = TRUE) %>% 
  purrr::pluck("data") %>% 
  filter(kode_prov == 32) %>% 
  as_tibble() %>%
  transmute(
    name_bps = kota,
    code_bps = kode_kota,
    name_kemendagri = case_when(
      str_detect(name_bps, "KOTA", negate = TRUE) ~ paste("KAB.", name_bps),
      TRUE ~ name_bps
    ),
    risk = hasil %>% 
      str_remove("RESIKO ") %>% 
      fct_recode(low = "RENDAH", medium = "SEDANG", high = "TINGGI")
  )

usethis::use_data(jabar_risk, overwrite = TRUE)
