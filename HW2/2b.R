algo_a <- c(93.99, 91.36, 71.68, 97.28, 85.94, 97.81, 77.47, 77.1, 85.81, 86.39, 83.7, 89.03, 95.68, 92.02, 87.89, 91.23, 72.84, 85.81, 50.15, 97.48, 99.04, 95.36, 66.67, 90.11, 64.04)
algo_b <- c(76.17, 91.04, 72.38, 95.99, 84.49, 93.17, 69.92, 71.96, 84.49, 83.33, 81.85, 79.35, 92.29, 90.31, 79.72, 91.23, 77.71, 81.08, 29.5, 92.88, 88.94, 52.04, 69.62, 90.57, 63.92)
algo_c <- c(97.77, 89.6, 71.68, 97, 87.54, 98.09, 76.04, 78.97, 84.82, 86.05, 84.07, 89.68, 95.86, 93.45, 91.24, 82.46, 85.2, 85.81, 49.56, 97.51, 99.04, 96.21, 73.52, 94.25, 64.02)
algo_d <- c(97.44, 89.6, 72.03, 97, 87.1, 97.81, 76.04, 78.97, 85.48, 85.37, 83.7, 89.68, 95.89, 93.45, 91.27, 85.96, 85.2, 86.49, 50.15, 97.38, 99.04, 95.67, 73.52, 94.25, 64.02)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)

library(tidyverse)
library(rstatix)
library(ggpubr)

df <- data.frame(id, algo_a, algo_b, algo_c, algo_d)
head(df, 3)
df <- df %>% gather(key="algo", value="score", algo_a, algo_b, algo_c, algo_d) %>% convert_as_factor(id, algo)
head(df, 3)

df %>% group_by(algo) %>% get_summary_stats(score, type="common")


ggboxplot(df, x = "algo", y = "score", add = "jitter")

res.fried <- df %>% friedman_test(formula=score ~ algo|id)
res.fried

df %>% friedman_effsize(score ~ algo|id)

pwc <- df %>%
  wilcox_test(score ~ algo, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "algo")
ggboxplot(df, x = "algo", y = "score", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )