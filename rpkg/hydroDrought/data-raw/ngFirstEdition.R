ngFirstEdition <- read_rds("~/Documents/boku/hd-book/Hydrolocical-Drought_CD/Data/global.rds") %>%
  filter(River == "Ngaruroro") %>%
  select(discharge) %>%
  unnest(discharge) %>%
  sanitize_ts()

usethis::use_data(ngFirstEdition, internal = TRUE, overwrite = TRUE)
