# Setting up times
checkpoints <- c("file started", "libraries loaded", "data loaded")
times <- c(Sys.time())

# Calling libraries
library(haven)
library(dplyr)
library()
times <- c(times, Sys.time()) # second checkpoint

# Reading in data
year <- "2019"
data_path = file.path("/home", "tea", "data", "current", paste0("TX", year, "_DRV_UMICH", ".dta"))
df <- read_dta(data_path, n_max = 100)
times <- c(times, Sys.time()) # third
df %>% colnames() %>% print()
print("Total rows:")
df %>% nrow() %>% print()

# Now, let's filter the dataframe to just be 3 through 5
print("Rows now:")
df <- df %>% filter(gradelevel %in% c(3, 4, 5))
print("Total rows now:")
df %>% nrow() %>% print()


# Times report
print("Times Report:")
for(i in 2:length(checkpoints)){
    print(checkpoints[i])
    print(times[i] - times[i-1])
}