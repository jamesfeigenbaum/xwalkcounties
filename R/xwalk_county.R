#' @title Collapse County-Level Data to New Baseline Borders
#'
#' @description Take county data, identify variables of interest, standardize borders to baseline
#'
#' @param data county level data (likely from ICPSR 2896)
#' @param y0 initial year of data
#' @param yb baseline year to transform data towards
#' @param merge_key vector identifying the ICPSR state and county code
#'     if using ICPSR 2896, this is just state and county
#'     like so: c("state", "county")
#' @param var_count variables that are counts like population or number of farms
#'     as a vector like c("totpop", "urb25")
#' @param var_value variables that are values like average temperature or average wage
#'     as a vector like c("avgtemp", "avgwage")
#'
#' @import magrittr
#' @importFrom readr read_csv
#' @importFrom dplyr rename inner_join select mutate_at group_by summarize_at ungroup
#' @importFrom tidyselect ends_with starts_with
#'
#' @export xwalk_county

xwalk_county <- function(data, y0, yb, merge_key = c("state", "county"), var_value = NULL, var_count = NULL) {

  # turning data from y0 into yb (y_baseline)

  # TODO store data properly...
  dt_map <- "R-devel/intersections/county_%d_%d.csv" %>%
    sprintf(yb, y0) %>%
    read_csv(col_names = TRUE, col_types = cols())

  # TODO allow non ICPSR state and county ids
  # we could do either gisjoin or nhgis ids as both are in mapping data

  data %>%
    rename("icpsrst_2" = merge_key[1]) %>%
    rename("icpsrcty_2" = merge_key[2]) %>%
    inner_join(dt_map %>%
                 select(ends_with("_1"), starts_with("weight"), "icpsrst_2", "icpsrcty_2"),
               by = c("icpsrst_2", "icpsrcty_2")) %>%
    mutate_at(var_count, ~ .x * weight_count) %>%
    mutate_at(var_value, ~ .x * weight_value) %>%
    group_by(year_1, icpsrst_1, icpsrcty_1) %>%
    summarize_at(c(var_count, var_value), ~ sum(.x)) %>%
    ungroup() %>%
    return()

  # TODO what is inner_join complaining about attributes about?

}
