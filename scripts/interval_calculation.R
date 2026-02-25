library(dplyr)
library(ggplot2)
library(tidyr)
library(gamlss)

# Load dataset
df <- read.csv("./data/your_dataset")

# Select the relevant columns
df <- df[c("Scr", "bmi_z", "BSA", "gender", "age_1a_v_2", "bodylength_cm_all_m_1.5")]

# %% Load the fitted model
model <- readRDS("./outputs/models/yourfittedmodel.rds")
model_simple <- readRDS("./outputs/models/yoursimplerfittedmodel.rds")

# Compute the distributional parameters
mus <- predict(model, newdata = df, what = "mu")
sigmas <- exp(predict(model, newdata = df, what = "sigma"))
taus <- exp(predict(model, newdata = df, what = "tau"))
nus <- predict(model, newdata = df, what = "nu")

# Calcualte the reference interval and the percentile scores of the observed SCr values
lowerbounds <- qBCPE(0.025, mu = mus, sigma = sigmas, nu = nus, tau = taus)
upperbounds <- qBCPE(0.975, mu = mus, sigma = sigmas, nu = nus, tau = taus)
percentiles <- round(pBCPE(filtered_hero$Scr, mu = mus, sigma = sigmas, nu = nus, tau = taus) * 100, 1)
