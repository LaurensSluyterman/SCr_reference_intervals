# Personalised SCr reference intervals

This repository contains an implementation of the approach presented in "*Personalized serum creatinine reference intervals for kidney function screening:  a proof-of-concept methodology*" that aims
to construct personalised reference interval for SCr measurements. In our paper, we used
the paid Lifelines dataset (https://www.lifelines-biobank.com), which is not publically available. Therefor, the data folder is empty and no fitted models are shared. 
Nevertheless, this repository can be used to easily fit and validate a model for a new dataset
of interest. To do so, use the following steps:

1) The file 'scripts/model_creation.R' fits a simple and more extensive gamlss model. Both fitted 
models are saved to the directory 'outputs/models'. Additionally, the predicted reference intervals
and the percentiles of the measured SCr values are saved to the 'outputs' directory. 
2) The file 'scripts/model_checks.R' makes various visualisations to better understand the behaviour
of both models. It also compares the pinball loss of the boundaries of the intervals of both models.
3) The file 'scripts/PIT_plots.R makes multiple PIT plots for both models. These models check if
the model is calibrated by checking, for various subgroups, if the percentiles of the observed
SCr values follow a uniform distribution. 
4) The file 'interval_calculation' calculates reference intervals and percentile scores for new
observations. 

