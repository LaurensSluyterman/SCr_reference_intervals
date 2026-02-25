library(ggplot2)
library(dplyr)
library(scales)
library(svglite)
library(ggfortify)

# % Load the fitted models and predictions on the original dataset
results <- read.csv("./outputs/predicted_intervals_and_percentiles.csv")
model <- readRDS("./outputs/models/yourfittedmodel.rds")
model_simple <- readRDS("./outputs/models/yoursimplerfittedmodel.rds")

# Read in the pre-processed dataset (the gamlss model needs access to the original dataset ot work)
df <- read.csv("./data/yourdata.csv")

# A simple visualisation to observe how the simple model behaves with age
bmi_z <- 1
bsa <- 1.1
ages <- seq(96, 217, length.out = 100)
new_data <- data.frame(
  "bmi_z" = rep(bmi_z, 100), "gender" = rep("MALE", 100),
  "age_1a_v_2" = ages,
  "BSA" = rep(bsa, 100)
)
mus <- predict(model_simple, newdata = new_data, what = "mu")
sigmas <- exp(predict(model_simple, newdata = new_data, what = "sigma"))
taus <- exp(predict(model_simple, newdata = new_data, what = "tau"))
nus <- predict(model_simple, newdata = new_data, what = "nu")

q025_new_male <- qBCPE(0.025, mu = mus, sigma = sigmas, nu = nus, tau = taus)
q975_new_male <- qBCPE(0.975, mu = mus, sigma = sigmas, nu = nus, tau = taus)

new_data_f <- data.frame(
  "bmi_z" = rep(bmi_z, 100), "gender" = rep("FEMALE", 100),
  "age_1a_v_2" = ages,
  "BSA" = rep(bsa, 100)
)
mus_f <- predict(model_simple, newdata = new_data_f, what = "mu")
sigmas_f <- exp(predict(model_simple, newdata = new_data_f, what = "sigma"))
taus_f <- exp(predict(model_simple, newdata = new_data_f, what = "tau"))
nus_f <- predict(model_simple, newdata = new_data_f, what = "nu")

q025_new_female <- qBCPE(0.025, mu = mus_f, sigma = sigmas_f, nu = nus_f, tau = taus_f)
q975_new_female <- qBCPE(0.975, mu = mus_f, sigma = sigmas_f, nu = nus_f, tau = taus_f)


plot_df <- data.frame(
  age = rep(ages, 2),
  q025 = c(q025_new_female, q025_new_male),
  q975 = c(q975_new_female, q975_new_male),
  gender = factor(rep(c("FEMALE", "MALE"), each = length(ages)))
)

scatter_df <- data2 %>%
  mutate(
    gender = factor(gender),
    color = ifelse(gender == "MALE", "#0072B2", "#D55E00"),
    shape = ifelse(gender == "MALE", 17, 19)
  )

p_SCr <- ggplot() +
  geom_jitter(
    data = scatter_df,
    aes(
      x = age_1a_v_2,
      y = Scr,
      color = gender,
      shape = gender
    ),
    width = 0.5, alpha = 0.5, size = 1.5
  ) +
  geom_line(
    data = plot_df,
    aes(x = age, y = q975, color = gender),
    linewidth = 1
  ) +
  geom_line(
    data = plot_df,
    aes(x = age, y = q025, color = gender),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("FEMALE" = "#D55E00", "MALE" = "#0072B2"),
    labels = c("Female", "Male")
  ) +
  scale_shape_manual(
    values = c("FEMALE" = 19, "MALE" = 17),
    labels = c("Female", "Male")
  ) +
  scale_x_continuous(
    name = "Age (years)",
    limits = c(96, 217),
    breaks = seq(8 * 12, 18 * 12, by = 24),
    labels = function(x) x / 12,
    expand = expansion(mult = 0.01),
  ) +
  scale_y_continuous(
    name = "Serum creatinine (mg/dL)",
    limits = c(0.2, 1.2),
    breaks = seq(0.2, 1.2, by = 0.2),
    expand = expansion(mult = 0.02),
  ) +
  labs(
    title = "95% reference interval based on sex and age",
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.title = element_blank(),
    plot.margin = margin(5, 5, 5, 5, unit = "mm")
  )

# automatic save plot function
save_a4_svg <- function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot,
    width = 210,
    units = "mm",
    device = "svg"
  )
}

save_a4_svg(
  plot = p_SCr,
  filename = file.path("./outputs/plots/behaviour_simple_model_with_age.svg")
)
# %% Comppare the average pinball loss of the full model and a simpler model
q025 <- qBCPE(
  0.025, fitted(model, "mu"), fitted(model, "sigma"),
  fitted(model, "nu"), fitted(model, "tau")
)
q975 <- qBCPE(
  0.975, fitted(model, "mu"), fitted(model, "sigma"),
  fitted(model, "nu"), fitted(model, "tau")
)

pinball_loss <- function(observation, quantile, alpha) {
  u <- observation - quantile
  loss <- ifelse(u >= 0, alpha * u, -(1 - alpha) * u)
  return(mean(loss))
}
pinball_loss(df$Scr, q975, 0.975) / pinball_loss(df$Scr, q975_simple, 0.975)
pinball_loss(df$Scr, q025, 0.025) / pinball_loss(df$Scr, q025_simple, 0.025)


# Checking individual values
i <- 3
patient_data <- df[i, ][c("gender", "age_1a_v_2", "bmi_z", "BSA", "Scr")]
print(" ----- Patient characteristics ----- ")
print(df[i, ][c("gender", "age_1a_v_2", "bmi_z", "BSA", "Scr")])
print(glue("RI = [{round(q025[i], 2)}, {round(q975[i], 2)}]"))
print(" ----- ---------- ----- ")

# Calculate for sampled individuals RI scr and eGFR_schwarz
df2 <- df %>%
  mutate(height_m = bodylength_cm_all_m_1.5 / 100)

df2 <- df2 %>%
  mutate(eGFR_CKiD = 0.413 * data2$bodylength_cm_all_m_1.5 / data2$Scr)

print(df2[i, ][c("gender", "age_1a_v_2", "bmi_z", "BSA", "Scr", "bodylength_cm_all_m_1.5", "eGFR")])
print(glue("RI = [{round(q025[i], 2)}, {round(q975[i], 2)}]"))
print(glue("RI = [{round(q025_simple[i], 2)}, {round(q975_simple[i], 2)}]"))

# Calculate eGFR [RI] (schwarz2009) per person as comparison
eGFR_schwarz_q025 <- round(0.413 * data2$bodylength_cm_all_m_1.5 / q975, 2)
eGFR_schwarz_q975 <- round(0.413 * data2$bodylength_cm_all_m_1.5 / q025, 2)
print(glue("eGFR Schwarz 2009 RI = [{eGFR_schwarz_q025[i]}, {eGFR_schwarz_q975[i]}]"))


eGFR_CKiDU25_q025 <- round((height_m * k) / q975, 2)
eGFR_CKiDU25_q975 <- round((height_m * k) / q025, 2)
print(glue("eGFR CKiD U25 RI = [{eGFR_CKiDU25_q025[i]}, {eGFR_CKiDU25_q975[i]}]"))
