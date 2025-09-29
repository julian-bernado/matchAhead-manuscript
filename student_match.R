library(dplyr)
library(readr)

match_students <- function(df, trt_school, ctrl_school) {
  vars_of_interest <- c(
    "schoolid_nces_enroll",
    "", #whatever student id is
    "" #prognostic score
  )
  sub_df <- df |>
  filter(schoolid_nces_enroll %in% c(trt_school, ctrl_school)) |>
  select(all_of(vars_of_interest))

  
}