library(data.table)
library(tidyverse)
library(haven)

# start with a simple thing, make 1860 into 1870
dt_map <- "R-devel/intersections/county_1870_1860.csv" %>%
  fread()

dt_census <- "G:/Dropbox/data/ICPSR/ICPSR_02896/DS0009/02896-0009-Data.dta" %>%
  read_dta() %>%
  filter(level == 1) %>%
  select(state, statefip, county, fips, name, totpop, whtot, farmval, acimp, acunimp) %>%
  mutate(white_share = 100 * whtot / totpop,
         farmval_acre = farmval / (acimp + acunimp))

dt_1870 <- "G:/Dropbox/data/ICPSR/ICPSR_02896/DS0011/02896-0011-Data.dta" %>%
  read_dta() %>%
  filter(level == 1) %>%
  select(state, county, fips, name, totpop, whtot, farmval) %>%
  mutate(white_share = 100 * whtot / totpop)

dt_map %>% head()

# situation in 1860
dt_map %>% filter(icpsrnam_2 %in% c("BALDWIN", "CONECUH", "ESCAMBIA") & icpsrst_2 == 41)
# situation in 1870
dt_map %>% filter(icpsrnam_1 %in% c("BALDWIN", "CONECUH", "ESCAMBIA") & icpsrst_1 == 41)
# these are the same, obviously

# from 1860 to 1870, baldwin, AL cedes 15% of its land to escambia, AL
# escambia, AL is made from part of baldwin and part of conecuh
# conecuh in 1860 to 1870 cedes 45% of land to new escambia, AL
# non escambia parts of baldwin and conecuh remain in the previously named counties
# and neither get land from anywhere else

# so... to recap
# in 1860 there is baldwin and conecuh, AL
# each cede some land between 1860 and 1870 to create escambia

# we are starting with 1860 data
# we want to standardize to 1870 borders (for whatever reason)

# we have two types of variables in the 1860 census
# counts like total population or total farmvalue
# values like share white or farm value per acre
    # (ignore that I constructed these from counts for now)
    # a better example (but not in the census) would be days of rain last year
    # clearly adding that up is wrong, need to take some weighted average

dt_map_bce <- dt_map %>%
  filter(icpsrnam_2 %in% c("BALDWIN", "CONECUH", "ESCAMBIA") & icpsrst_2 == 41)

dt_census_bce <- dt_census %>%
  filter(name %in% c("BALDWIN", "CONECUH", "ESCAMBIA") & state == 41)

dt_census_bce %>%
  inner_join(dt_map_bce %>%
               select(ends_with("_1"), starts_with("weight"), "icpsrst_2", "icpsrcty_2"),
             by = c("state" = "icpsrst_2", "county" = "icpsrcty_2")) %>%
  mutate_at(c("totpop", "whtot", "farmval", "acimp", "acunimp"), ~ .x * weight_count) %>%
  mutate_at(c("white_share", "farmval_acre"), ~ .x * weight_value) %>%
  group_by(year_1, icpsrst_1, icpsrcty_1) %>%
  summarize_at(c("totpop", "whtot", "farmval", "acimp", "acunimp", "white_share", "farmval_acre"),
               ~ sum(.x))

merge_key <- c("state", "county")

var_count <- c("totpop", "whtot", "farmval", "acimp", "acunimp")
var_value <- c("white_share", "farmval_acre")

dt_census %>%
  rename("icpsrst_2" = merge_key[1]) %>%
  rename("icpsrcty_2" = merge_key[2]) %>%
  inner_join(dt_map %>%
               select(ends_with("_1"), starts_with("weight"), "icpsrst_2", "icpsrcty_2"),
             by = c("icpsrst_2", "icpsrcty_2")) %>%
  mutate_at(var_count, ~ .x * weight_count) %>%
  mutate_at(var_value, ~ .x * weight_value) %>%
  group_by(year_1, icpsrst_1, icpsrcty_1) %>%
  summarize_at(c(var_count, var_value), ~ sum(.x))
