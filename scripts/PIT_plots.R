library(tidyverse)
library(glue)

results <- read.csv("./output/intervals_and_percentiles.csv")

ages <- c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
bmis <- c(-3, -2, -1, 0, 1, 2)
width <- 0.1
y_max <- 2.5

p <- ggplot(results, aes(x = percentiles / 100)) +
  geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  xlab("Percentiles full model") +
  labs(
    x = "Percentiles full model",
    title = glue("All ages")
  ) +
  ylim(0, y_max) +
  xlim(0, 1)
print(p)
ggsave(glue("./outputs/plots/PITplot_full_model_all_ages.pdf"), p)

for (age in ages) {
  p <- ggplot(results %>% filter(age_1a_v_2 >= age * 12, age_1a_v_2 < (age + 1) * 12), aes(x = percentiles / 100)) +
    geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
    xlab("Percentiles full model") +
    labs(
      x = "Percentiles full model",
      title = glue("{age} year olds")
    ) +
    ylim(0, y_max)
  print(p)
  ggsave(glue("./outputs/plots/PITplot_full_model_age_{age}.pdf"), p)
}

for (bmi in bmis) {
  p <- ggplot(results %>% filter(bmi_z >= bmi, bmi_z < (bmi + 1)), aes(x = percentiles / 100)) +
    geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
    xlab("Percentiles full model") +
    labs(
      x = "Percentiles full model",
      title = glue("BMI {bmi} to {bmi + 1}")
    ) +
    ylim(0, y_max)
  print(p)
  ggsave(glue("./outputs/plots/PITplot_full_model_bmi_{bmi}_{bmi+1}.pdf"), p)
}

## Simple model
p <- ggplot(results, aes(x = percentiles_simple / 100)) +
  geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  xlab("Percentiles full model") +
  labs(
    x = "Percentiles simple model",
    title = glue("All ages")
  ) +
  ylim(0, y_max)
print(p)
ggsave(glue("./outputs/plots/PITplot_simple_model_all_ages.pdf"), p)

for (age in ages) {
  p <- ggplot(results %>% filter(age_1a_v_2 >= age * 12, age_1a_v_2 < (age + 1) * 12), aes(x = percentiles_simple / 100)) +
    geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
    xlab("Percentiles full model") +
    labs(
      x = "Percentiles simple model",
      title = glue("{age} year olds")
    ) +
    ylim(0, y_max)
  print(p)
  ggsave(glue("./outputs/plots/PITplot_simple_model_age_{age}.pdf"), p)
}

for (bmi in bmis) {
  p <- ggplot(results %>% filter(bmi_z >= bmi, bmi_z < (bmi + 1)), aes(x = percentiles_simple / 100)) +
    geom_histogram(aes(y = ..density..), breaks = seq(0, 1, by = width)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
    xlab("Percentiles simple model") +
    labs(
      x = "Percentiles simple model",
      title = glue("BMI {bmi} to {bmi + 1}")
    ) +
    ylim(0, y_max)
  print(p)
  ggsave(glue("./outputs/plots/PITplot_simple_model_bmi_{bmi}_{bmi+1}.pdf"), p)
}
