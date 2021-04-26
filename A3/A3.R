library(tidyverse)
library(xts)
library(plm)


## Exercise 1 Links to the datasets

population <- read.csv("https://www.dropbox.com/s/s38cde88670y5mw/population.csv?dl=1")
crime_long <- read.csv("https://www.dropbox.com/s/t3vushurhm3s5my/crime_long.csv?dl=1")
officers <- read.csv("https://www.dropbox.com/s/8q2fpdb7phy86m8/officers.csv?dl=1")


## Exercise 2 Data Manipulation

### Calculate total crime per month and plot the time series of crime

#### total crime per month
crime_monthly <- crime_long %>%
  group_by(crime_month) %>%
  summarize(crime_by_month = sum(crimes))

#### plot time series of crime
crime_monthly_xts <- xts(crime_monthly$crime_by_month, as.Date(crime_monthly$crime_month, format='%Y-%m-%d'))
plot(crime_monthly_xts,type = 'l',main=' ')

### Merge the two datasets by districts-units and period

crime_population <- merge(crime_long, population,by.x = c("district","crime_month"), by.y = c("district","month"), all = TRUE)

### Construct a panel data of unit over time with the following variables

# – Total crimes per resident
# – Violent crimes per resident
# – Property crimes per resident
# – Median income
# – Share of black, Hispanic, and white residents

panel_data <- crime_population %>%
  mutate(
    violent_crimes = case_when(
      crime_type == "violent" ~ crimes,
      TRUE ~ 0L
    ),
    property_crimes = case_when(
      crime_type == "property" ~ crimes,
      TRUE ~ 0L
    )
  ) %>%
  group_by(district,crime_month) %>%
  summarize(
    total_crimes = sum(crimes),
    violent_crimes = sum(violent_crimes),
    property_crimes = sum(property_crimes),
    median_income = p50_inc,
    share_of_black = tot_black/tot_pop,
    share_of_hisp = tot_hisp/tot_pop,
    share_of_white = tot_white/tot_pop
  ) %>%
  distinct()


## Exercise 3 Panel Data: Introduction

df <- merge(officers, panel_data, by.x = c("month", "unit"), by.y=c("crime_month", "district"), all.x=TRUE)
panel_df <- pdata.frame(df,index=c("NUID","month"))

# use lm
lm_pooled <- lm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white - 1,
  data = df
)
# estimators
# beta
lm_pooled$coefficients[1]
# gamma
lm_pooled$coefficients[2:6]

# check 
pooled <- plm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white - 1,
  data = panel_df,
  model = "pooling"
)
# estimator
# beta
pooled$coefficients[1]
# gamma
pooled$coefficients[2:6]


## Exercise 4 Panel Data: More controls

# use lm
fe1_ols <- lm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white + factor(unit) + factor(month) - 1,
  data = df
)
# estimators
# beta
fe1_ols$coefficients[1]
# gamma
fe1_ols$coefficients[2:6]
# psi
fe1_ols$coefficients[7:31]
# kappa
fe1_ols$coefficients[32:length(fe1_ols$coefficients)]

# check
fe1 <- plm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white + factor(unit) - 1,
  effect = "time",
  data = panel_df,
  model = "within"
)
# estimators
# beta
fe1$coefficients[1]
# gamma
fe1$coefficients[2:6]
# psi
fe1$coefficients[7:30]
# kappa
fixef(fe1)


## Exercise 5 Panel Data: Individual fixed effects

### Implement a within, between, and first difference estimator for the parameter beta. Then, compare the estimated values.

# within 
fe2 <- plm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white + factor(unit) - 1,
  effect = "twoway",
  data = panel_df,
  model = "within"
)

# between 
fe3 <- plm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white + factor(unit) + factor(month) - 1,
  effect = "individual",
  data = panel_df,
  model = "between"
)

# fd
fe4 <- plm(
  formula = arrest ~ tenure + total_crimes + median_income + share_of_black + share_of_hisp + share_of_white + factor(unit) + factor(month) - 1,
  effect = "individual",
  data = panel_df,
  model = "fd"
)

# compare beta
est_betas <- c(fe2$coefficients[1], fe3$coefficients[1], fe4$coefficients[1])
names(est_betas) <- c("within", "between", "fd")
est_betas

#within and between estimated beta estimator are both negative, first difference estimated beta is positive.

### Use a GMM approach to estimate all parameters (including fixed effects) in one step.

# one-step GMM: beta = [X'Z(Z'Z)^(-1)Z'X]^{-1}X'Z(Z'Z)^(-1)Z'y
# if X = Z, it is just estimator of OLS.

