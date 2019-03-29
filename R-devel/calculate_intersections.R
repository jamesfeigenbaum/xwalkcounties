#calculate_intersections.R
#feigenbaum
#26aug2018

# load the nhgis county maps
# calculate the area of intersection between counties in different years
# store the data out

library(tidyverse)
library(sf)
library(ipumsr)
library(USAboundaries)
library(purrr)
library(furrr)

plan(multiprocess)

# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-geography.html

nhgis_path <- "G:/Dropbox/Data/Mapping Data/NHGIS"

# load a census year from NHGIS
load_nhgis_shp <- function(year) {

  year %>%
    sprintf("%s/%d Census/US_county_%d.shp", nhgis_path, ., .) %>%
    st_read() %>%
    # keep many of the ids but we don't need all of them (the inteter and string duplicates, eg)
    janitor::clean_names() %>%
    select(nhgisnam, nhgisst, nhgiscty, icpsrst, icpsrcty, icpsrnam, statenam, gisjoin) %>%
    mutate(decade = year) %>%
    # calculate the area of the county
    mutate(area = st_area(geometry)) %>%
    return()

}

intersect_shps <- function(y1, y2, small_intersection = 1000) {

  out_path <- sprintf("data-raw/county_%d_%d.csv", y1, y2)

  # check if already intersected
  if (out_path %>% file.exists()) {

    return("next")

  }

  temp_y1 <- load_nhgis_shp(y1)
  temp_y2 <- load_nhgis_shp(y2)

  # intersect
  temp_intersection <-
    st_intersection(temp_y1, temp_y2) %>%
    mutate(area_intersection = st_area(geometry)) %>%
    # only calculate the intersections of reasonable size (1 sq km is the default)
    filter((area_intersection %>% as.numeric()) > small_intersection) %>%
    mutate(weight1 = (area_intersection %>% as.numeric()) / (area %>% as.numeric()),
           weight2 = (area_intersection %>% as.numeric()) / (area.1 %>% as.numeric())) %>%
    select(year_1 = decade, year_2 = decade.1,
           nhgisnam_1 = nhgisnam, nhgisst_1 = nhgisst, nhgiscty_1 = nhgiscty,
           icpsrst_1 = icpsrst, icpsrcty_1 = icpsrcty, icpsrnam_1 = icpsrnam,
           statenam_1 = statenam, gisjoin_1 = gisjoin,
           nhgisnam_2 = nhgisnam.1, nhgisst_2 = nhgisst.1, nhgiscty_2 = nhgiscty.1,
           icpsrst_2 = icpsrst.1, icpsrcty_2 = icpsrcty.1, icpsrnam_2 = icpsrnam.1,
           statenam_2 = statenam.1, gisjoin_2 = gisjoin.1,
           area_1 = area, area_2 = area.1, area_intersection,
           weight_value = weight1, weight_count = weight2) %>%
    st_set_geometry(NULL)

  temp_intersection %>%
    data.table::fwrite(file = out_path)

}

# create all the intersections
# year_cross <- cross2(seq(1850, 1930, by = 10), seq(1790, 1810, by = 10))

# create all the intersections
for (y in seq(1790, 1990, by = 10)) {

  print(y)

  future_map(seq(1790, 1990, by = 10), intersect_shps, y2 = y)

}
