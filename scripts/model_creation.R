library(gamlss)
library(Metrics)
library(ggplot2)
library(dplyr)

# Read in the pre-processed dataset
data_raw <- read.csv("./data/yourdata")
data2 <- data_raw[c("Scr", "bmi_z", "BSA", "gender", "age_1a_v_2", "bodylength_cm_all_m_1.5")]

# Fit the GAMLSS model
model <- gamlss(Scr ~ pb(bmi_z) + pb(age_1a_v_2) + gender + pb(BSA) + gender * pb(age_1a_v_2) + gender * pb(BSA) + gender * pb(bmi_z),
  sigma.formula = ~ pb(bmi_z) + pb(age_1a_v_2) + pb(BSA) + gender,
  family = BCPE, data = data2, sigma.link = "log"
)

# Save the model
saveRDS(model, file = "./outputs/models/yourfittedmodel.rds")
summary(model)

# Fit a simpler model that does not use bmi_z and bsa
model_simple <- gamlss(Scr ~ pb(age_1a_v_2) + gender + pb(age_1a_v_2) * gender,
  sigma.formula = ~ pb(age_1a_v_2) + gender + gender * pb(age_1a_v_2),
  tau.formula = ~1,
  family = BCPE, data = data2, sigma.link = "log"
)

saveRDS(model_simple, file = "./outputs/models/yoursimplerfittedmodel.rds")
summary(model_simple)


# Save all the predicted intervals and percentiles (for validation purposes)
mus <- fitted(model_simple, "mu")
sigmas <- fitted(model_simple, "sigma")
taus <- fitted(model_simple, "tau")
nus <- fitted(model_simple, "nu")
percentiles_simple <- round(pBCPE(data2$Scr, mu = mus, sigma = sigmas, nu = nus, tau = taus) * 100, 1)
lowerbounds_simple <- qBCPE(0.025, mu = mus, sigma = sigmas, nu = nus, tau = taus)
upperbounds_simple <- qBCPE(0.975, mu = mus, sigma = sigmas, nu = nus, tau = taus)
data2["percentiles_simple"] <- percentiles_simple
data2["lowerbounds_simple"] <- lowerbounds_simple
data2["upperbounds_simple"] <- upperbounds_simple


percentiles <- round(pBCPE(data2$Scr,
  mu = fitted(model, "mu"), sigma = fitted(model, "sigma"),
  nu = fitted(model, "nu"), tau = fitted(model, "tau")
) * 100, 1)
lowerbounds <- qBCPE(0.025,
  mu = fitted(model, "mu"), sigma = fitted(model, "sigma"),
  nu = fitted(model, "nu"), tau = fitted(model, "tau")
)
upperbounds <- qBCPE(0.975,
  mu = fitted(model, "mu"), sigma = fitted(model, "sigma"),
  nu = fitted(model, "nu"), tau = fitted(model, "tau")
)
data2["lowerbounds"] <- lowerbounds
data2["upperbounds"] <- upperbounds
data2["percentiles"] <- percentiles

write.csv(data2, "./outputs/predicted_intervals_and_percentiles.csv")
