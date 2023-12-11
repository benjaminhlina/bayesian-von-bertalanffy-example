# ---- load packages ----
{
  library(dplyr)
  library(FSAdata)
  library(ggplot2)
  library(ggmcmc)
  library(here)
  library(jagsUI)
  library(patchwork)
  library(postpack)
  library(readr)
  library(tidyr)
}

# ---- grab example dataset ----
# Lake Erie walleye dataset from FSAdata
dat <- WalleyeErie2
glimpse(dat)

summary(dat)

pred_age <- seq(0, 20, 0.5) # adjust for plotting this is like tidy::crossing()

jags_data <- list(
  n_obs = nrow(dat),
  length = dat$tl,
  age = dat$age,
  pred_age = pred_age,
  n_pred = length(pred_age)
)


# ---- Specify JAGS model code -----
# here you will create your priors based on previous known data for Lake Eire walleye
# from the literature to super von B. parameters

jags_model <- function() {
  # priors

  linf ~ dunif(100, 800)
  k ~ dunif(0, 0.5)
  t0 ~ dunif(-1.25, 0.5)
  sig ~ dunif(0, 2) # using a log likelihood so log back transformed

  # Likelihood

  for (i in 1:n_obs) {

    length[i] ~ dlnorm(log(length_hat[i]), 1 / sig ^ 2)

    length_hat[i] <- (linf) * (1 - exp(-k *(age[i] - t0)))
  }
  # Derived Quantities
  # this makes it easier to plot
  for (i in 1:n_pred){
    # expected length @ age
    pred_length[i, 1] <- linf *  (1 - exp(-k *(pred_age[i] - t0)))

    # sample random residuals to obtain random predictions
    # when summarized this will represent the length @ age distr
    # that we can use to look at population density for length @ age
    #
    epi[i] ~ dlnorm(0, 1 / sig ^ 2)
    rand_length[i, 1] <- epi[i] * pred_length[i, 1]
  }
}

# ---- create file path ----
jags_file <- here("JAGS models",
                  "von_b_bays_estimate_lake_erie_walleye.txt")


