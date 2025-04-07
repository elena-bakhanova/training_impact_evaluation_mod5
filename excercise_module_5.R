################
# This code is related to the exercises as part of EdX course on Impact Evaluation Methods with applications in low and middle-income countries
# The data set is taken from: Thornton, R.L. (2008). The Demand for, and Impact of, Learning HIV Status. American Economic Review, 98 (5), 1829-63
################


# Install packages
# install.packages("tidyverse")
# install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
library(haven)

# Define the root directory path
current_dir <- getwd()

# Data must be downloaded from https://www.openicpsr.org/openicpsr/project/113268/version/V1/view
data_file <- "data/Thornton HIV Testing Data.dta"

# Load data
full_path <- file.path(current_dir, data_file)
hiv_data <- read_dta(full_path)

## Regression analysis
# Tabulate the variable `tinc` to see what range of incentives were offered
table(hiv_data$tinc)

# Substitute NA in 'tinc' to zero
hiv_data$tinc[is.na(hiv_data$tinc)] <- 0

# Create a factor variable 'treatment' that indicates whether a participant got fin incentive (1) or not (0)
hiv_data <- hiv_data %>%
  mutate(treatment = if_else(tinc > 0, 1, 0),
         treatment = factor(treatment,
                    levels = c(0,1),
                    labels = c("Control", "Treatment")))

# Double check that NAs were correctly shown
print(treatment)

# Note: variable 'got' indicates whether or not a participant received the result of the test
# Substitute NA in 'got' to zero
hiv_data$got[is.na(hiv_data$got)] <- 0

# Make regression to analyse the treatment effect of being offered a monetary incentive
reg_model <- lm(got ~ treatment, data = hiv_data)
summary(reg_model)

# Run a regression of 'got' on 'treatment' with robust standards errors
# Install additional packages for robust standard errors
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

# Calculate robust standard errors
coeftest(reg_model, vcov = vcovHC(reg_model, type = "HC0"))

# Test for heteroskedasticity
bptest(reg_model)

# Calculate the mean of `got` among those that were treated and were not treated
hiv_data %>%
  group_by(treatment) %>%
  summarise(mean_got = mean(got, na.rm = TRUE))

# Add the control variable `age` to the regression of `got` on `treatment`
reg_model_age <- lm(got ~ treatment + age, data = hiv_data)
coeftest(reg_model_age, vcov = vcovHC(reg_model_age, type = "HC0"))


## Regress `got` on `treatment`, `male`, and an interaction between `treatment` and `male`
reg_model_male <- lm(got ~ treatment + male + treatment:male, data = hiv_data)
coeftest(reg_model_male, vcov = vcovHC(reg_model_male, type = "HC0"))

# Regress `got` on `treatment`, `age`, and an interaction between `treatment` and `age`
reg_model_inter_age <- lm(got ~ treatment + age + treatment:age, data = hiv_data)
coeftest(reg_model_inter_age, vcov = vcovHC(reg_model_inter_age, type = "HC0"))
