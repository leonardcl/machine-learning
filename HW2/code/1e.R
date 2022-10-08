# Perform Wilcoxon Signed-Rank Test

algo_a <- c(93.99, 91.36, 71.68, 97.28, 85.94, 97.81, 77.47, 77.1, 85.81, 86.39, 83.7, 89.03, 95.68, 92.02, 87.89, 91.23, 72.84, 85.81, 50.15, 97.48, 99.04, 95.36, 66.67, 90.11, 64.04)
algo_b <- c(76.17, 91.04, 72.38, 95.99, 84.49, 93.17, 69.92, 71.96, 84.49, 83.33, 81.85, 79.35, 92.29, 90.31, 79.72, 91.23, 77.71, 81.08, 29.5, 92.88, 88.94, 52.04, 69.62, 90.57, 63.92)

library(tidyverse)
library(rstatix)
library(ggpubr)
library(coin)

shapiro.test(algo_a)
shapiro.test(algo_b)

df <- data.frame(algo_a, algo_b)
head(df, 3)
df <- df %>% gather(key="algo", value="score", algo_a, algo_b) %>% convert_as_factor(algo)
head(df, 3)

df %>% group_by(algo) %>% get_summary_stats(score, type="median_iqr")

# Visualize BoxPlot
bxp <- ggpaired(df, x = "algo", y = "score", 
                order = c("algo_a", "algo_b"),
                ylab = "Weight", xlab = "Groups")
bxp


# Test
stat.test <- df  %>%
  wilcox_test(score ~ algo, paired = TRUE) %>%
  add_significance()
stat.test

df  %>%
  wilcox_effsize(score ~ algo, paired = TRUE)