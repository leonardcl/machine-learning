a <- c(19.119, 17.773, 19.78, 16.841, 18.782)
b <- c(17.2, 16.389, 17.715, 16.367, 16.82)
c <- c(22.692, 23.267, 24.193, 23.162, 23.21)
d <- c(18.927, 17.695, 18.776, 16.824, 18.779)
e <- c(17.199, 16.381, 17.715, 16.367, 16.796)
id <- c(1, 2, 3, 4, 5)

library(tidyverse)
library(rstatix)
library(ggpubr)

df <- data.frame(id, a, b, c, d, e)
head(df, 3)
df <- df %>% gather(key="experiment", value="score", a, b, c, d, e) %>% convert_as_factor(id, experiment)
head(df, 3)

df %>% group_by(experiment) %>% get_summary_stats(score, type="common")


ggboxplot(df, x = "experiment", y = "score", add = "jitter")

res.fried <- df %>% friedman_test(formula=score ~ experiment|id)
res.fried

df %>% friedman_effsize(score ~ experiment|id)

pwc <- df %>%
  wilcox_test(score ~ experiment, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "experiment")
ggboxplot(df, x = "experiment", y = "score", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )