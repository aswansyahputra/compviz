library(jabr)

jabar_basemap <- jabr_basemap(level = "district")

usethis::use_data(jabar_basemap, overwrite = TRUE)
