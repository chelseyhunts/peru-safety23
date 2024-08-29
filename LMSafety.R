##################Code created by Chelsey February 16 2024

# LMs/LMEMs for hypothesis testing Safety cues 2023 data
# Model selection to answer questions for number of species/individuals 
# Net distance change pre to post-stimulus

# Clear the environment
rm(list = ls())

# Specify the required packages
package_list <- c("MuMIn", "lme4", "tidyverse", "lmerTest", "car", "boot")

# Install and load the packages
install.packages(package_list)
library(tidyverse) #for use of dplyr and ggplot
library(lme4) #for modelling
library(MuMIn) #for model selection
library(lmerTest) #to calculate p-values from model summary
library(car) #Test for equal variance
library(boot) #For bootstrapping to predict model values


#Run to install packages needed and load libraries
for (package in package_list) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


# Setup Data --------------------------------------------------------------

#Read in data frame
net.dist23 <- read.csv("net.dist23.csv", stringsAsFactors = TRUE)
head(net.dist23)
#reorder the groups order : I change the order of the factor data$names to order plot
net.dist23$Tr.Type <- factor(net.dist23$Tr.Type , 
                             levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", 
                                      "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))
#Replace NAs with CTRL
which(is.na(net.dist23$Tr.Type))
net.dist23 <- net.dist23 %>%
  mutate(Tr.Type = replace_na(Tr.Type, "CTRL"))

# Hypotheses --------------------------------------------------------------

#1. MSF species provide better safety information to the larger bird community 
#relative to other forest birds (grouped treatments) -- birds move closer when MSFs heard
#2. The behavioral response of the avian community changes based on the individual 
#species providing safety information (key informants, individual treatments) 
#Don't include treatment in this model, just treatment type 
#3. Safety cues are driven by group size (individual vs grouped treatments) 
#individual gives better safety info, or competition or dilution of the effect


# Visualize Response Data No. Individuals ----------------------------------------------

#Histogram to show distribution of response
hist(net.dist23$Net.Dist) #Normal ish

#####plot net dist by treatment
ggplot(net.dist23, aes(Treatment, Net.Dist)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) + 
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")


#####plots net dist by treatment type
ggplot(net.dist23, aes(Tr.Type, Net.Dist)) + geom_boxplot(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) + 
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

# Multivariate Linear Regression  -----------------------------------------------------------

#In this section, we conducted statistical modeling and analysis to evaluate the relationship between two predictor variables and net distance change in position to five playback treatments. I utliized the lmer() and lm() function from the lme4 package in R to fit linear models and linear mixed-effects models. 
#
# We assessed the goodness-of-fit of the models by evaluating the AICc values due to small sample size. Lower AICc values indicate better model fit with fewer parameters, suggesting a more parsimonious model. The model summaries provided estimates, standard errors, z-values, and p-values for the fixed effects, allowing us to evaluate the significance of the predictors.
# 
# To further evaluate the models, we generated residual plots to examine the standardized and Pearson residuals using the DHARMa package. These plots provide insights into the distribution of residuals and help identify any patterns or deviations from the model assumptions.
# 
# Finally, we compared the models based on their AICc values to determine the most appropriate model. This evaluation step helps us select the model that best balances goodness-of-fit and model complexity, providing insights into the relationship between the predictor variables and number of individuals recruited.
# 
# Overall, this modeling and analysis approach allows us to investigate the effects of multiple predictor variables on number of individuals recruited, the distribution of the data, assess model fit, and draw meaningful conclusions about the relationship between these variables and the observed number of individuals recruited in our study.

# Model Selection No.Individuals ---------------------------------------------------------

## LM Models -----------------------------------------

# Intercept Only Model 
LN <- lm(Net.Dist~1, data = net.dist23)

# Single Fixed-effect Model
# These models consider one predictor at a time to gauge their individual impacts on the dependent variable.
L1 <- lm(Net.Dist~Treatment, data = net.dist23)
L2 <- lm(Net.Dist~Forest.type, data = net.dist23)

# Two covariates Models
L3 <- lm(Net.Dist~Treatment+Forest.type, data = net.dist23)

# Two covariates with interaction term
L4 <- lm(Net.Dist~Treatment+Forest.type+Forest.type*Treatment, data = net.dist23)

# Model selection LM Net Distance using AIC because sample size is relatively large
best.mod <- model.sel(L1,L2,L3,L4,LN, rank = "AIC") 
best.mod

## LMEMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
LN2 <- lmer(Net.Dist~1+(1|Point), net.dist23)

# Single Fixed-effect Model
L5 <- lmer(Net.Dist~Treatment+(1|Point),data = net.dist23)
L6 <- lmer(Net.Dist~Forest.type+(1|Point),data = net.dist23)

# Two covariates Models
L7 <- lmer(Net.Dist~Treatment+Forest.type+(1|Point),data = net.dist23)

# Two covariates with interaction term
L8 <- lmer(Net.Dist~Treatment+Forest.type+Forest.type*Treatment+(1|Point),data = net.dist23)

## Model Selection with all Models -----------------------------------------
# Here I compare models from all distribution types and with all combinations of covariates to determine the best model

best.mod <- model.sel(L1,L2,L3,L4,LN, L5,L6,L7,L8,LN2, rank = "AIC") 
best.mod

summary(L8)

# Check Model Assumptions -------------------------------------------------

#Iterative process
#1st run analysis, 2nd check assumptions
#If assumptions ok, analyze raw data
#If assumptions badly violated, consider data transformation, or another approach
#s.a nonparametric tests, etc...
#Check assumptions using resid()
#Will return residuals around the regression line/linear model
#Plot histogram
#hist(resid(m1), probability = TRUE)
hist(resid(L8), probability = TRUE)
mean(resids)

curve(dnorm(x, mean=mean(resids), sd=sd(resids) ),from=-50,to=50,add=TRUE)
#Normalish

qqnorm(resid(L8))
qqline(resid(L8))
#Not super normal...

#check goodness of fit test to a normal distribution 
shapiro.test(resids)
#p-val<0.05, not normalish

#Are the variances equal enough at all levels of X? 
#Assess via scatterplot, using temp as X, and resids as Y
plot(net.dist23$Treatment, resids, xlab = "Treatment")
#from scatter plot -- we can see variances are fairly equal

#Variance among groups using Formal test 
leveneTest(resids, net.dist23$Treatment) #p-value=0.4
#After running a formal test, we see that the variances are equal, 

## Goodness of Fit Number of Individuals -----------------------------------

#To test the goodness of fit of the top model candidate, I plotted model residuals against the predicted line, and plotted a histogram of simulated residuals against fitted residuals 
qqnorm(resid(L8))
qqline(resid(L8))

gsimOut <- simulateResiduals(L8)
plot(L8)
testDispersion(L8)

# Predict Model Values ----------------------------------------------------

#Need to use a bootstrapping technique as there is an interaction term in the top model

# Function to predict values
predict_fun <- function(fit) {
  predict(fit, newdata = net.dist23, re.form = NA)
}

# Perform bootstrapping
set.seed(123)  # For reproducibility
boot_results <- bootMer(L8, predict_fun, nsim = 1000, use.u = TRUE)

#Calculate confidence intervals
# Calculate the predicted values
predicted_vals <- predict(L8, newdata = net.dist23, re.form = NA)

# Calculate the confidence intervals
conf_int <- apply(boot_results$t, 2, function(x) quantile(x, c(0.025, 0.975)))

# Add the predicted values and confidence intervals to the dataframe
predicted_data <- data.frame(
  Trial = net.dist23$Trial,  # Replace with your actual column name if different
  Observed = net.dist23$Net.Dist,
  Predicted = predicted_vals,
  Treatment = net.dist23$Treatment,
  lower_ci = conf_int[1, ],
  upper_ci = conf_int[2, ]
)

# Print the dataframe to see the results
print(predicted_data)

# Aggregate predicted values and confidence intervals by treatment
agg_data <- predicted_data %>%
  group_by(Treatment) %>%
  summarise(
    Observed = mean(Observed),
    Predicted = mean(Predicted),
    lower_ci = mean(lower_ci),
    upper_ci = mean(upper_ci)
  )

# Print the aggregated data to check
print(agg_data)

## Plot Predictions --------------------------------------------------------

# Plot using ggplot predicted values with confidence intervals
ggplot(predicted_data, aes(x = Treatment, y = Observed)) +
  geom_jitter(aes(colour = Treatment), width = 0.1) +  # Add observed values offset 
  geom_point(data = agg_data, aes(x = Treatment, y = Predicted), colour = "black", size = 4) +  # Predicted mean
  geom_errorbar(data = agg_data, aes(x = Treatment, ymin = lower_ci, ymax = upper_ci), width = 0.1) +  # CIs
  labs(x = "Treatment", y = "Net Distance") +  # Axis labels
  theme_classic() +  # Customize the theme
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(angle = 00, size = 12, hjust = 1),  # Rotate x-axis text
    axis.text.y = element_text(size = 15),
    legend.position = "none"
  )

# Model Treatment Type  ---------------------------------------------------

Ltt <- lmer(Net.Dist~Tr.Type+Forest.type+Forest.type*Tr.Type+(1|Point),data = net.dist23)
summary(Ltt)

#Need to use a bootstrapping technique as there is an interaction term in the top model

# Function to predict values
predict_fun <- function(fit) {
  predict(fit, newdata = net.dist23, re.form = NA)
}

# Perform bootstrapping
set.seed(123)  # For reproducibility
boot_results <- bootMer(Ltt, predict_fun, nsim = 1000, use.u = TRUE)

#Calculate confidence intervals
# Calculate the predicted values
predicted_vals <- predict(Ltt, newdata = net.dist23, re.form = NA)

# Calculate the confidence intervals
conf_int <- apply(boot_results$t, 2, function(x) quantile(x, c(0.025, 0.975)))

# Add the predicted values and confidence intervals to the dataframe
predicted_data <- data.frame(
  Trial = net.dist23$Trial,  # Replace with your actual column name if different
  Observed = net.dist23$Net.Dist,
  Predicted = predicted_vals,
  Treatment = net.dist23$Tr.Type,
  lower_ci = conf_int[1, ],
  upper_ci = conf_int[2, ]
)

# Print the dataframe to see the results
print(predicted_data)

# Aggregate predicted values and confidence intervals by treatment
agg_data <- predicted_data %>%
  group_by(Treatment) %>%
  summarise(
    Observed = mean(Observed),
    Predicted = mean(Predicted),
    lower_ci = mean(lower_ci),
    upper_ci = mean(upper_ci)
  )

# Print the aggregated data to check
print(agg_data)

## Plot Predictions --------------------------------------------------------

# Plot using ggplot predicted values with confidence intervals
ggplot(predicted_data, aes(x = Treatment, y = Observed)) +
  geom_jitter(aes(colour = Treatment), width = 0.1) +  # Add observed values offset 
  geom_point(data = agg_data, aes(x = Treatment, y = Predicted), colour = "black", size = 4) +  # Predicted mean
  geom_errorbar(data = agg_data, aes(x = Treatment, ymin = lower_ci, ymax = upper_ci), width = 0.1) +  # CIs
  labs(x = "Treatment", y = "Net Distance") +  # Axis labels
  theme_classic() +  # Customize the theme
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(angle = 90, size = 12, hjust = 1),  # Rotate x-axis text
    axis.text.y = element_text(size = 15),
    legend.position = "none"
  )
