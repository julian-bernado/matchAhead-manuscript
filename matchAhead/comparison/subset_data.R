library(dplyr)
library(readr)

subset_data <- function(grade, subject, N = 250) {
  df <- read_csv(paste0("data/2019_", grade, "_", subject, "_df.csv"))
  unique_schools <- df |> pull(schoolid_state_enroll_p0) |> unique()
  subset_schools <- sample(unique_schools, N)
  sub_df <- df |> filter(schoolid_state_enroll_p0 %in% subset_schools)
  write_csv(sub_df, paste0("data/2019sub_", grade, "_", subject, "_df.csv"))
  df2 <- read_csv(paste0("data/2022_", grade, "_", subject, "_df.csv"))
  sub_df2 <- df2 |> filter(schoolid_state_enroll_p0 %in% subset_schools)
  write_csv(sub_df2, paste0("data/2022sub_", grade, "_", subject, "_df.csv"))

}

grades <- c(3, 4, 5)
subs <- c("glmath", "readng")

for (grade in grades) {
  for (sub in subs) {
    print(paste0("Grade: ", grade, "\n Subject: ", sub))
    subset_data(grade, sub)
  }
}