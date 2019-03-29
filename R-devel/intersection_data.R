
# write the data into the data folder
dt_map_1870_1860 <- "data-raw/county_%d_%d.csv" %>%
  sprintf(1870, 1860) %>%
  read_csv(col_names = TRUE, col_types = cols())


year_grid <- expand.grid(y1 = seq(1790, 1990, by = 10),
                         y2 = seq(1790, 1990, by = 10),
                         stringsAsFactors = FALSE) %>%
  as_tibble()

dt_map <- map2_dfr(year_grid$y1, year_grid$y2, ~
                     "data-raw/county_%d_%d.csv" %>%
                     sprintf(.y, .x) %>%
                     fread())

usethis::use_data(dt_map, internal = TRUE, overwrite = TRUE)

# this is huge...
