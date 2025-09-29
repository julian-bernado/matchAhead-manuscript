library(dplyr)
library(readr)

subset_data <- function(grade, subject, N = 100) {
  df <- read_csv(paste0("data/2019_", grade, "_", subject, "_df.csv"))
  unique_schools <- df |> pull(schoolid_state_enroll_p0) |> unique()
  print(length(unique_schools))
}

grades <- c(3, 4, 5)
subs <- c("glmath", "readng")

for (grade in grades) {
  for (sub in subs) {
    print(paste0("Grade: ", grade, "\n Subject: ", sub))
    subset_data()
  }
}