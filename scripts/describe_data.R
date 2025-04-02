library(readr)
library(dplyr)
df <- read_csv("data/2019_3_glmath_df.csv")
for(col in colnames(df)){
  print(col)
  print(df %>% pull(col) %>% summary())
}