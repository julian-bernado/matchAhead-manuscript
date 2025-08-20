library(readr)
library(dplyr)
library(ggplot2)

MA_FILENAME <- "our_output.csv"
PIMENTEL_FILENAME <- "keele_output.csv"

ma_results_df <- read_csv(paste0("outputs/", MA_FILENAME))
pimentel_results_df <- read_csv(paste0("outputs/", PIMENTEL_FILENAME))
school_df <- read_csv("data/2022_3_glmath_df.csv")

# All we care about from school_df (which is at the student level) is schoolid_state_enroll_p0
# We should make a new df with one column being schoolid_state_enroll_p0
# The other column should be the size of that school, and that should be the entire df called school_sizes
school_sizes <- school_df %>% dplyr::count(schoolid_state_enroll_p0, name = "size")

id_cols <- c("treatment_group", "control_group")

df <- dplyr::inner_join(
  ma_results_df, pimentel_results_df,
  by = id_cols,
  suffix = c(".ma", ".pim")
)

# Now, we nede to create two new columns in df
# One should be treatment school size and one should be control school size
# the column treatment_group indicates the schoolid_state_enroll_p0 for the treatment school
# similarly for control_group
# So the new columns trt_size and ctrl_size should just report the size of the treatment and control schools for the given pairing
# Afterward, we should calculate a ratio, trt_size/ctrl_size and call that column ratio

df <- df %>%
  dplyr::left_join(school_sizes, by = c("treatment_group" = "schoolid_state_enroll_p0")) %>%
  dplyr::rename(trt_size = size) %>%
  dplyr::left_join(school_sizes, by = c("control_group" = "schoolid_state_enroll_p0")) %>%
  dplyr::rename(ctrl_size = size) %>%
  dplyr::mutate(
    ratio = trt_size / ctrl_size,
    elapsed_ratio = elapsed.pim / elapsed.ma
  )

# First, CPU vs. Elapsed
# I want to make and save two scatterplots using ggplot2
# The scatterplot should have the "elapsed.{method}" column of df on the x-axis
# and the "cpu_time.{method}" column on the y-axis
# where method = pim or ma
# First we should plot elapsed.ma vs. cpu_time.ma
# Then we should plot elapsed.pim vs. cpu_time.pim

p_scatter_ma <- ggplot(df, aes(x = elapsed.ma, y = cpu_time.ma)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "MA: CPU Time vs Elapsed", x = "Elapsed (MA)", y = "CPU Time (MA)")

p_scatter_pim <- ggplot(df, aes(x = elapsed.pim, y = cpu_time.pim)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "PIM: CPU Time vs Elapsed", x = "Elapsed (PIM)", y = "CPU Time (PIM)")

# Next, shared density plot
# Density plot of "elapsed.ma" and "elapsed.pim"
# It's all one plot, we're just doing one density for ma and one for pim
# They should be different colors

p_density_elapsed <- ggplot() +
  geom_density(data = df, aes(x = elapsed.ma, color = "MA"), size = 1) +
  geom_density(data = df, aes(x = elapsed.pim, color = "PIM"), size = 1) +
  scale_color_manual(values = c(MA = "#1b9e77", PIM = "#d95f02"), name = "Method") +
  theme_minimal() +
  labs(title = "Elapsed Time Density: MA vs PIM", x = "Elapsed", y = "Density")

# Finally, 3 more scatter plots
# On the y-axis we should have the ratio of elapsed.pim/elapsed.ma
# One plot for each x-axis between: trt_size, ctrl_size, and ratio

p_ratio_vs_trt <- ggplot(df, aes(x = trt_size, y = elapsed_ratio)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Elapsed Ratio (PIM/MA) vs Treatment Size", x = "Treatment Size", y = "Elapsed Ratio (PIM/MA)")

p_ratio_vs_ctrl <- ggplot(df, aes(x = ctrl_size, y = elapsed_ratio)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Elapsed Ratio (PIM/MA) vs Control Size", x = "Control Size", y = "Elapsed Ratio (PIM/MA)")

p_ratio_vs_size_ratio <- ggplot(df, aes(x = ratio, y = elapsed_ratio)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Elapsed Ratio (PIM/MA) vs Size Ratio (Trt/Ctrl)", x = "Size Ratio (Trt/Ctrl)", y = "Elapsed Ratio (PIM/MA)")

# PLACEHOLDER TO SAVE EVERY PLOT AS A PNG
dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/plots/scatter_ma.png", p_scatter_ma, width = 6, height = 4, dpi = 300)
ggsave("outputs/plots/scatter_pim.png", p_scatter_pim, width = 6, height = 4, dpi = 300)
ggsave("outputs/plots/density_elapsed.png", p_density_elapsed, width = 6, height = 4, dpi = 300)
ggsave("outputs/plots/ratio_vs_trt_size.png", p_ratio_vs_trt, width = 6, height = 4, dpi = 300)
ggsave("outputs/plots/ratio_vs_ctrl_size.png", p_ratio_vs_ctrl, width = 6, height = 4, dpi = 300)
ggsave("outputs/plots/ratio_vs_size_ratio.png", p_ratio_vs_size_ratio, width = 6, height = 4, dpi = 300)
