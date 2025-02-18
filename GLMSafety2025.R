##################Code created by XX May 16 2024, updated Jan 21 2025

# GLMs/GLMMs for hypothesis testing Safety cues 2023 data
# Model selection to answer questions for number of species/individuals
# Comparing groups, comparing individuals, comparing individuals to groups

# Clear the environment
rm(list = ls())


# Load Libraries ----------------------------------------------------------

# Specify the required packages
package_list <- c("MuMIn", "lme4", "tidyverse", "ggeffects", "emmeans", "flextable", "vegan", "MASS", "DHARMa", "broom")

# Install and load the packages
install.packages(package_list)
library(MuMIn) #used for model selection
library(lme4) #used to run models
library(tidyverse) #used for plotting predictions
library(ggeffects) #used for predicting response values from model
library(emmeans) #used to run contrasts
library(flextable) #used for making table of model selection
library(vegan) #used to run Bray-Curtis dissimilarity analysis
library(MASS) #used for glm.nb -- negative binomial glm
library(DHARMa) #used to test goodness of fit
library(broom) #used to create data frame of model summaries


#Run to install packages needed and load libraries
for (package in package_list) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


# Setup Data --------------------------------------------------------------

#Read in data frame
Safety23 <- read.csv("Safety23.csv", stringsAsFactors = TRUE)
head(Safety23) 
#Response is Total.Recs, Treatment/Treatment type is covariate1, Forest.type is covariate2 (TF, VA)

#reorder the groups for treatment type : 
Safety23$Tr.Type <- factor(Safety23$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", 
                                                       "MYSC", "THAR", "TUOC", "NF", 
                                                       "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))


# Hypotheses --------------------------------------------------------------

#1. MSF species provide better safety information to the larger bird community 
#relative to other forest birds (grouped treatments)
#2. The behavioral response of the avian community changes based on the individual 
#species providing safety information (key informants, individual treatments) 
#Don't include treatment in this model, just treatment type 
#3. Safety cues are driven by group size (individual vs grouped treatments) 
#individual gives better safety info, or competition or dilution of the effect


# Visualize Response Data No. Individuals----------------------------------------------------------

#Histogram to show distribution of response
hist(Safety23$Total.Recs)
#Poisson or negative binomial

#Means with standard error
summary_stats <- Safety23 %>%
  group_by(Treatment) %>%
  summarize(
    Mean = mean(Total.Recs),
    SD = sd(Total.Recs),
    SE = sd(Total.Recs) / sqrt(n())
  )

ggplot(summary_stats, aes(Treatment, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="Treatment", y="Mean No. Recruits")


# Multivariate Linear Regression  -----------------------------------------------------------

#In this section, I conducted statistical modeling and analysis to evaluate the relationship between two predictor variables and number of individuals recruited to five playback treatments. I utliized the glmer(), glm() and glmer.nb() function from the lme4 package in R to fit generalized linear models and generalized linear mixed-effects models, and the glm.nb() function from the MASS package. 
#The models were estimated using Poisson and negative binomial families, as the response variable was a discrete count. 
# GLMER with logit link 'binomial' or 'Poisson'
#
# We assessed the goodness-of-fit of the models by evaluating the AICc values due to small sample size. Lower AICc values indicate better model fit with fewer parameters, suggesting a more parsimonious model. The model summaries provided estimates, standard errors, z-values, and p-values for the fixed effects, allowing us to evaluate the significance of the predictors.
# 
# To further evaluate the models, we generated residual plots to examine the standardized and Pearson residuals using the DHARMa package. These plots provide insights into the distribution of residuals and help identify any patterns or deviations from the model assumptions.
# 
# Finally, we compared the models based on their AICc values to determine the most appropriate model. This evaluation step helps us select the model that best balances goodness-of-fit and model complexity, providing insights into the relationship between the predictor variables and number of individuals recruited.
# 
# Overall, this modeling and analysis approach allows us to investigate the effects of multiple predictor variables on number of individuals recruited, the distribution of the data, assess model fit, and draw meaningful conclusions about the relationship between these variables and the observed number of individuals recruited in our study.

# Model Selection No.Individuals ---------------------------------------------------------

## Poisson Distribution GLM Models -----------------------------------------

# Intercept Only Model 
glm.pN1<-glm(Total.Recs~1,family="poisson",data=Safety23)

# Single Fixed-effect Model
# These models consider one predictor at a time to gauge their individual impacts on the dependent variable.
glm.p1<-glm(Total.Recs~Treatment,family="poisson",data=Safety23) 


# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.pN1, glm.p1, rank = "AICc") 
best.mod
summary(glm.p1)


## Poisson Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.pN1 <- glmer(Total.Recs~1+(1|Point), family = "poisson", data = Safety23)

# Single Fixed-effect Model
glmer.p1<-glmer(Total.Recs~Treatment + (1|Point), family="poisson", data=Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.pN1, glmer.p1, rank = "AICc") 
best.mod
summary(glmer.p1)

## Negative Binomial Distribution GLMs -----------------------------------------------------------------

# Intercept Only Model 
glm.nbN1 <- glm.nb(Total.Recs~1, data = Safety23)

# Single Fixed-effect Model
glm.nb1<-glm.nb(Total.Recs~Treatment, data=Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.nbN1, glm.nb1, rank = "AICc") 
best.mod
summary(glm.nb1)

## Negative Binomial Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.nbN1 <- glmer.nb(Total.Recs~1+(1|Point), data = Safety23) #Boundary singular

# Single Fixed-effect Model
glmer.nb1<-glmer.nb(Total.Recs~Treatment + (1|Point), data=Safety23) #Boundary singular

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.nbN1, glmer.nb1, rank = "AICc") 
best.mod
summary(glmer.nb1)


## Model Selection with all Models -----------------------------------------
# Here I compare models from all distribution types to determine the best model
best.mod <- model.sel(glm.pN1, glm.p1, glmer.pN1, glmer.p1, glm.nbN1, glm.nb1, glmer.nbN1, glmer.nb1, rank = "AICc") 
best.mod
# Top model is GLM with negative binomial distribution and treatment as covariate, 
# Next is GLMM with negative binomial distribution, treatment and trial location random effect
summary(glm.nb1)
#Determine confidence intervals for each treatment
confint(glm.nb1)
#Run log likelihood
logLik(glm.nb1)
logLik(glmer.nb1) #errors

## Goodness of Fit Number of Individuals -----------------------------------

#To test the goodness of fit of the top model candidate, I plotted model residuals against the predicted line, and plotted a histogram of simulated residuals against fitted residuals 
qqnorm(resid(glm.nb1))
qqline(resid(glm.nb1))
       
gsimOut <- simulateResiduals(glm.nb1)
plot(gsimOut)
testDispersion(glm.nb1) #expected dispersion under model
testDispersion(glm.p1) #over dispersed 

## Model Selection Table ---------------------------------------------------

# Make model selection table to data frame
#include model terms, random var, intercept, k, df, logLik, delta AICc, weight
best.mod.table <- as.data.frame(best.mod)
#Name models
best.mod.table$Model <- 1:8


#Create column with model terms:
# Column names to check for the plus sign
columns_to_check <- c("Treatment")

# Initialize the new column with empty strings
best.mod.table$Model_Terms <- ""

# Iterate over the rows of the dataframe
for (i in 1:nrow(best.mod.table)) {
  # Initialize an empty vector to hold the names of the terms for this row
  terms <- c()
  
  # Check each column for the plus sign and add its name to the terms vector if present
  for (column_name in columns_to_check) {
    # Check if the value is not NA before comparing to "+"
    if (!is.na(best.mod.table[i, column_name]) && best.mod.table[i, column_name] == "+") {
      terms <- c(terms, column_name)
    }
  }
  
  # Concatenate the terms with plus signs and assign to the new column
  best.mod.table$Model_Terms[i] <- paste(terms, collapse = " + ")
}

#Print updated df
best.mod.table

#Add terms for null model 
best.mod.table["glm.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmer.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmer.pN1", "Model_Terms"] <- "Null Model"
best.mod.table["glm.pN1", "Model_Terms"] <- "Null Model"

#Update terms suitable for print
best.mod.table <- best.mod.table %>%
  mutate(class = str_replace(class, "glmerMod", "GLMM")) %>% 
  mutate(class = str_replace(class, "glm", "GLM")) %>% 
  rename("Model_Type"=class) %>% 
  mutate(Family = str_extract(family, "^[A-Za-z]+\\s[A-Za-z]+|^[A-Za-z]+")) %>%
  mutate(Model_Type = str_replace(Model_Type, "negbin", "GLM")) %>%
  mutate(Family = str_replace(Family, "poisson", "Poisson"))
  
print(best.mod.table)
library(dplyr)
#Remove unnecessary columns
best.mod.table <- best.mod.table %>% 
  select_at(vars(12,13,14,4,8,9,10,11)) %>% #Keep only desired columns -- Model type, logLik, AICc, delta, weight, model, model terms, Family
  mutate(across(where(is.numeric), ~round(., 2))) %>% #Reduce number of decimal points
  mutate(k = c(2,2,1,1,2,1,2,1)) %>%  #Add column for parameters
  select_at(vars(1, 2, 3, 4, 9, everything())) %>% #rearrange to put k after model terms
  rename("∆ AICc"=delta) %>% #Rename delta AICc column
  rename("Model Terms"=Model_Terms, "Model Type"=Model_Type, "Weight"=weight)

####Make table of model selection
###Create table to use in report
#Create flextable
ft <- flextable::qflextable(best.mod.table)
ft <- flextable::colformat_num(ft, columns = "Values", big.mark = "") %>% 
  flextable::width(width =1.1) %>% flextable::align(align = "center", part = "all") 

#Add flextable to word doc
print(ft, preview = "docx")

## Coefficients Table for Top Two Models -----------------------------------

#To show the effects of forest type are negligible even though the second top model includes it, we created a table showing coefficients for both two top models, including model term, estimate, and p values

summary(glm.nb1)
confint.nb1 <- as.data.frame(confint(glm.nb1)) #create data frame of CIs
confint.nb1 <- confint.nb1 %>% rename(lwr="2.5 %", upr="97.5 %") %>% #rename columns
  mutate(lwr = round(lwr, 3), upr = round(upr, 3)) #round to three decimals
confint.nb1$range <- paste0("(", confint.nb1$lwr, ",", confint.nb1$upr, ")") #create range column for CIs
confint.nb1 <- confint.nb1 %>% dplyr::select(range) #remove upr and lrw columns

sum.glm.nb1 <- as.data.frame(tidy(glm.nb1)) #create data frame of model summary
sum.glm.nb1 <- cbind(sum.glm.nb1, confint.nb1) #combine model summary with CI data frame
sum.glm.nb1 <- sum.glm.nb1 %>% dplyr::select(-std.error, -statistic) %>% #remove columns
  mutate(model_number = rep(1)) %>% #add model number
  mutate(estimate = round(estimate, 3)) %>% #round values to three decimals
  mutate(p.value = round(p.value, 3)) %>% #round values to three decimals
  dplyr::select(model_number, everything()) #move model number to first column

#Repeat for second best model
summary(glmer.nb1)
confint.rnb1 <- as.data.frame(confint(glmer.nb1))
confint.rnb1 <- confint.rnb1 %>% rename(lwr="2.5 %", upr="97.5 %") %>% 
  mutate(lwr = round(lwr, 3), upr = round(upr, 3))
confint.rnb1$range <- paste0("(", confint.rnb1$lwr, ",", confint.rnb1$upr, ")")
confint.rnb1 <- confint.rnb1 %>% dplyr::select(range)

sum.glm.nb3 <- as.data.frame(tidy(glm.nb3))
sum.glm.nb3 <- cbind(sum.glm.nb3, confint.nb3)
sum.glm.nb3 <- sum.glm.nb3 %>% dplyr::select(-std.error, -statistic) %>% 
  mutate(model_number = rep(2)) %>% 
  mutate(estimate = round(estimate, 3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  dplyr::select(model_number, everything())

row.names(sum.glm.nb1) <- NULL
#row.names(sum.glm.nb3) <- NULL
#coef.table <- rbind(sum.glm.nb1, sum.glm.nb3)
coef.table <- sum.glm.nb1 %>% rename(Estimate=estimate, "p-value"=p.value, "Model"=model_number, "95% CI"=range) %>% mutate(term = str_replace_all(term, fixed("(Intercept)"), "Intercept")) %>% mutate(term = if_else(term == "TreatmentMSF", "Grouped MSF", term))  %>% 
  mutate(term = if_else(term == "TreatmentMSFSOLO", "MSF Solo", term))  %>% 
  mutate(term = if_else(term == "TreatmentNF", "Grouped NF", term)) %>% 
  mutate(term = if_else(term == "TreatmentNFSOLO", "NF Solo", term))  %>% 
  mutate(term = if_else(term == "Forest.typeVA", "Varzea", term)) %>% 
  rename("Model Term"=term) 

####Make table of model selection
###Create table to use in report
#Create flextable
ct <- flextable::qflextable(coef.table)

#Add flextable to word doc
print(ct, preview = "docx")


# Rename treatments -------------------------------------------------------

Safety23  <- Safety23 %>%
  mutate(Treatment_Full = case_when(
    Treatment == "CTRL" ~ "Control",
    Treatment == "MSF" ~ "MSF Group",
    Treatment == "NF" ~ "NF Group",
    Treatment == "MSFSOLO" ~ "MSF Individual",
    Treatment == "NFSOLO" ~ "NF Individual",
    TRUE ~ Treatment  # This keeps any other values unchanged
  ))
head(Safety23)

Safety23  <- Safety23 %>%
  mutate(Tr.Type_Full = case_when(
    Tr.Type == "CTRL" ~ "Control",
    Tr.Type == "MSF" ~ "MSF Group",
    Tr.Type == "NF" ~ "NF Group",
    TRUE ~ Tr.Type  # This keeps any other values unchanged
  ))
head(Safety23)

#reorder the groups for treatment type : 
Safety23$Tr.Type_Full <- factor(Safety23$Tr.Type_Full , levels=c("Control", "MSF Group", "HARU", "MYLO", 
                                                       "MYSC", "THAR", "TUOC", "NF Group", 
                                                       "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))
## Plot Model Predictions --------------------------------------------------

### Predict Model Values --------------------------------------------------
# Predict model values for glm.nb1 top model, use type=link as it is on the negative binomial scale
#pred <- predict(model, type="")
predicted_link <- predict(glm.nb1, type = "link", se.fit = TRUE)

# Calculate the confidence intervals on the link scale
link_fit <- predicted_link$fit
link_se <- predicted_link$se.fit

z_value <- qnorm(0.975)  # 97.5 percentile point for a normal distribution (for 95% CI)
link_lower <- link_fit - z_value * link_se
link_upper <- link_fit + z_value * link_se

# Transform the confidence intervals back to the response scale
response_lower <- exp(link_lower)
response_upper <- exp(link_upper)

# Predict on the response scale
predicted_vals <- predict(glm.nb1, type = "response")

# Create a dataframe with observed values, predicted values, and confidence intervals
predicted_data <- data.frame(
  Trial = Safety23$Trial,
  Observed = Safety23$Total.Recs,
  Predicted = predicted_vals,
  Treatment = Safety23$Treatment_Full,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)

# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)
head(Safety23)
# Plot using ggplot predicted values with confidence intervals 
# Define the colors
treatment_colors <- c(
  "Control" = "darkgray",
  "MSF Group" = "cornflowerblue",
  "NF Group" = "seagreen3",
  "MSF Individual" = "cadetblue2",
  "NF Individual" = "lightgreen"
)

shapes <- c("Control" = 16, 
            "MSF Group" = 17, 
            "NF Group" = 18,
            "MSF Individual" = 2,
            "NF Individual" = 5)  # Example shapes

ggplot(predicted_data, aes(x = Treatment, y = Predicted)) +
  geom_jitter(aes(Treatment, Observed, 
                  colour = Treatment, shape = Treatment), width = 0.1, size = 3) +  # Add observed values offset 
  geom_point(colour = "black", size = 4) +  # Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  # CIs
  labs(x = "Treatment", y = "Number of Individuals Responding") +  # Axis labels
  theme_classic() +  # Customize the theme
  theme(text = element_text(family = "Times New Roman"),  # Change the font to Times New Roman
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.position = "none") +
  scale_color_manual(values = treatment_colors) +
  scale_shape_manual(values = shapes)  # Specify shapes

# Visualize Response Data No. Species----------------------------------------------------------

#Histogram to show distribution of response
hist(Safety23$Total.spp.rec.trial)
#Poisson or negative binomial

#Means with standard error
summary_stats <- Safety23 %>%
  group_by(Treatment) %>%
  summarize(
    Mean = mean(Total.spp.rec.trial),
    SD = sd(Total.spp.rec.trial),
    SE = sd(Total.spp.rec.trial) / sqrt(n())
  )

ggplot(summary_stats, aes(Treatment, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="Treatment", y="Mean No. Spp Responding")


# Multivariate Linear Regression  -----------------------------------------------------------

#In this section, we conducted statistical modeling and analysis to evaluate the relationship between two predictor variables and number of species recruited to five playback treatments. We utliized the glmer(), glm() and glmer.nb() function from the lme4 package in R to fit generalized linear models and generalized linear mixed-effects models, and the glm.nb() function from the MASS package. 
#The models were estimated using Poisson and negative binomial families, as the response variable was a discrete count. 
# GLMER with logit link 'binomial' or 'Poisson'
#
# We assessed the goodness-of-fit of the models by evaluating the AICc values due to small sample size. Lower AICc values indicate better model fit with fewer parameters, suggesting a more parsimonious model. The model summaries provided estimates, standard errors, z-values, and p-values for the fixed effects, allowing us to evaluate the significance of the predictors.
# 
# To further evaluate the models, we generated residual plots to examine the standardized and Pearson residuals using the DHARMa package. These plots provide insights into the distribution of residuals and help identify any patterns or deviations from the model assumptions.
# 
# Finally, we compared the models based on their AICc values to determine the most appropriate model. This evaluation step helps us select the model that best balances goodness-of-fit and model complexity, providing insights into the relationship between the predictor variables and number of individuals recruited.
# 
# Overall, this modeling and analysis approach allows us to investigate the effects of multiple predictor variables on number of individuals recruited, the distribution of the data, assess model fit, and draw meaningful conclusions about the relationship between these variables and the observed number of individuals recruited in our study.

# Model Selection No.Individuals ---------------------------------------------------------

## Poisson Distribution GLM Models -----------------------------------------

# Intercept Only Model 
glm.s.pN1<-glm(Total.spp.rec.trial~1,family="poisson",data=Safety23)

# Single Fixed-effect Model
# These models consider one predictor at a time to gauge their individual impacts on the dependent variable.
glm.s.p1<-glm(Total.spp.rec.trial~Treatment,family="poisson",data=Safety23) 


# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.s.pN1, glm.s.p1, rank = "AICc") 
best.mod
summary(glm.s.p1)


## Poisson Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.s.pN1 <- glmer(Total.spp.rec.trial~1+(1|Point), family = "poisson", data = Safety23)

# Single Fixed-effect Model
glmer.s.p1<-glmer(Total.spp.rec.trial~Treatment + (1|Point), family="poisson", data=Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.s.pN1, glmer.s.p1, rank = "AICc") 
best.mod
summary(glmer.s.p1)

## Negative Binomial Distribution GLMs -----------------------------------------------------------------

# Intercept Only Model 
glm.s.nbN1 <- glm.nb(Total.spp.rec.trial~1, data = Safety23)

# Single Fixed-effect Model
glm.s.nb1<-glm.nb(Total.spp.rec.trial~Treatment, data=Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.s.nbN1, glm.s.nb1, rank = "AICc") 
best.mod
summary(glm.s.nb1)

## Negative Binomial Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.s.nbN1 <- glmer.nb(Total.spp.rec.trial~1+(1|Point), data = Safety23) #Boundary singular

# Single Fixed-effect Model
glmer.s.nb1<-glmer.nb(Total.spp.rec.trial~Treatment + (1|Point), data=Safety23) #Boundary singular

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.s.nbN1, glmer.s.nb1, rank = "AICc") 
best.mod
summary(glmer.nb1)


## Model Selection with all Models -----------------------------------------
# Here we compare models from all distribution types to determine the best model
best.mod <- model.sel(glm.s.pN1, glm.s.p1, glmer.s.pN1, glmer.s.p1, glm.s.nbN1, glm.s.nb1, glmer.s.nbN1, glmer.s.nb1, rank = "AICc") 
best.mod
# Top model is GLM with negative binomial distribution and treatment as covariate, 
# Next is GLMM with negative binomial distribution, treatment and trial location random effect
summary(glm.s.nb1)
#Determine confidence intervals for each treatment
confint(glm.s.nb1)
#Run log likelihood
logLik(glm.s.nb1)
logLik(glmer.s.nb1) #errors

## Goodness of Fit Number of Individuals -----------------------------------

#To test the goodness of fit of the top model candidate, we plotted model residuals against the predicted line, and plotted a histogram of simulated residuals against fitted residuals 
qqnorm(resid(glm.s.nb1))
qqline(resid(glm.s.nb1))

gsimOut <- simulateResiduals(glm.s.nb1)
plot(gsimOut)
testDispersion(glm.s.nb1) #expected dispersion under model
testDispersion(glm.s.p1) #over dispersed 

## Model Selection Table ---------------------------------------------------

# Make model selection table to data frame
#include model terms, random var, intercept, k, df, logLik, delta AICc, weight
best.mod.table <- as.data.frame(best.mod)
#Name models
best.mod.table$Model <- 1:8


#Create column with model terms:
# Column names to check for the plus sign
columns_to_check <- c("Treatment")

# Initialize the new column with empty strings
best.mod.table$Model_Terms <- ""

# Iterate over the rows of the dataframe
for (i in 1:nrow(best.mod.table)) {
  # Initialize an empty vector to hold the names of the terms for this row
  terms <- c()
  
  # Check each column for the plus sign and add its name to the terms vector if present
  for (column_name in columns_to_check) {
    # Check if the value is not NA before comparing to "+"
    if (!is.na(best.mod.table[i, column_name]) && best.mod.table[i, column_name] == "+") {
      terms <- c(terms, column_name)
    }
  }
  
  # Concatenate the terms with plus signs and assign to the new column
  best.mod.table$Model_Terms[i] <- paste(terms, collapse = " + ")
}

#Print updated df
best.mod.table

#Add terms for null model 
best.mod.table["glm.s.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmer.s.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmer.s.pN1", "Model_Terms"] <- "Null Model"
best.mod.table["glm.s.pN1", "Model_Terms"] <- "Null Model"

#Update terms suitable for print
best.mod.table <- best.mod.table %>%
  mutate(class = str_replace(class, "glmerMod", "GLMM")) %>% 
  mutate(class = str_replace(class, "glm", "GLM")) %>% 
  rename("Model_Type"=class) %>% 
  mutate(Family = str_extract(family, "^[A-Za-z]+\\s[A-Za-z]+|^[A-Za-z]+")) %>%
  mutate(Model_Type = str_replace(Model_Type, "negbin", "GLM")) %>%
  mutate(Family = str_replace(Family, "poisson", "Poisson"))

print(best.mod.table)
library(dplyr)
#Remove unnecessary columns
best.mod.table <- best.mod.table %>% 
  select_at(vars(12,13,14,4,8,9,10,11)) %>% #Keep only desired columns -- Model type, logLik, AICc, delta, weight, model, model terms, Family
  mutate(across(where(is.numeric), ~round(., 2))) %>% #Reduce number of decimal points
  mutate(k = c(2,2,1,1,2,2,1,1)) %>%  #Add column for parameters
  select_at(vars(1, 2, 3, 4, 9, everything())) %>% #rearrange to put k after model terms
  rename("∆ AICc"=delta) %>% #Rename delta AICc column
  rename("Model Terms"=Model_Terms, "Model Type"=Model_Type, "Weight"=weight)

####Make table of model selection
###Create table to use in report
#Create flextable
ft <- flextable::qflextable(best.mod.table)
ft <- flextable::colformat_num(ft, columns = "Values", big.mark = "") %>% 
  flextable::width(width =1.1) %>% flextable::align(align = "center", part = "all") 

#Add flextable to word doc
print(ft, preview = "docx")

## Coefficients Table for Top Two Models -----------------------------------

#To show the effects of forest type are negligible even though the second top model includes it, we created a table showing coefficients for both two top models, including model term, estimate, and p values

summary(glm.s.nb1)
confint.s.nb1 <- as.data.frame(confint(glm.s.nb1)) #create data frame of CIs
confint.s.nb1 <- confint.s.nb1 %>% rename(lwr="2.5 %", upr="97.5 %") %>% #rename columns
  mutate(lwr = round(lwr, 3), upr = round(upr, 3)) #round to three decimals
confint.s.nb1$range <- paste0("(", confint.s.nb1$lwr, ",", confint.s.nb1$upr, ")") #create range column for CIs
confint.s.nb1 <- confint.s.nb1 %>% dplyr::select(range) #remove upr and lrw columns

sum.glm.s.nb1 <- as.data.frame(tidy(glm.s.nb1)) #create data frame of model summary
sum.glm.s.nb1 <- cbind(sum.glm.s.nb1, confint.s.nb1) #combine model summary with CI data frame
sum.glm.s.nb1 <- sum.glm.s.nb1 %>% dplyr::select(-std.error, -statistic) %>% #remove columns
  mutate(model_number = rep(1)) %>% #add model number
  mutate(estimate = round(estimate, 3)) %>% #round values to three decimals
  mutate(p.value = round(p.value, 3)) %>% #round values to three decimals
  dplyr::select(model_number, everything()) #move model number to first column

#Repeat for second best model
summary(glmer.nb1)
confint.rnb1 <- as.data.frame(confint(glmer.nb1))
confint.rnb1 <- confint.rnb1 %>% rename(lwr="2.5 %", upr="97.5 %") %>% 
  mutate(lwr = round(lwr, 3), upr = round(upr, 3))
confint.rnb1$range <- paste0("(", confint.rnb1$lwr, ",", confint.rnb1$upr, ")")
confint.rnb1 <- confint.rnb1 %>% dplyr::select(range)

sum.glm.nb3 <- as.data.frame(tidy(glm.nb3))
sum.glm.nb3 <- cbind(sum.glm.nb3, confint.nb3)
sum.glm.nb3 <- sum.glm.nb3 %>% dplyr::select(-std.error, -statistic) %>% 
  mutate(model_number = rep(2)) %>% 
  mutate(estimate = round(estimate, 3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  dplyr::select(model_number, everything())

row.names(sum.glm.s.nb1) <- NULL
#row.names(sum.glm.nb3) <- NULL
#coef.table <- rbind(sum.glm.nb1, sum.glm.nb3)
coef.table <- sum.glm.s.nb1 %>% rename(Estimate=estimate, "p-value"=p.value, "Model"=model_number, "95% CI"=range) %>% mutate(term = str_replace_all(term, fixed("(Intercept)"), "Intercept")) %>% mutate(term = if_else(term == "TreatmentMSF", "Grouped MSF", term))  %>% 
  mutate(term = if_else(term == "TreatmentMSFSOLO", "MSF Solo", term))  %>% 
  mutate(term = if_else(term == "TreatmentNF", "Grouped NF", term)) %>% 
  mutate(term = if_else(term == "TreatmentNFSOLO", "NF Solo", term))  %>% 
  mutate(term = if_else(term == "Forest.typeVA", "Varzea", term)) %>% 
  rename("Model Term"=term) 

####Make table of model selection
###Create table to use in report
#Create flextable
ct <- flextable::qflextable(coef.table)

#Add flextable to word doc
print(ct, preview = "docx")


# Rename treatments -------------------------------------------------------

Safety23  <- Safety23 %>%
  mutate(Treatment_Full = case_when(
    Treatment == "CTRL" ~ "Control",
    Treatment == "MSF" ~ "MSF Group",
    Treatment == "NF" ~ "NF Group",
    Treatment == "MSFSOLO" ~ "MSF Individual",
    Treatment == "NFSOLO" ~ "NF Individual",
    TRUE ~ Treatment  # This keeps any other values unchanged
  ))
head(Safety23)

Safety23  <- Safety23 %>%
  mutate(Tr.Type_Full = case_when(
    Tr.Type == "CTRL" ~ "Control",
    Tr.Type == "MSF" ~ "MSF Group",
    Tr.Type == "NF" ~ "NF Group",
    TRUE ~ Tr.Type  # This keeps any other values unchanged
  ))
head(Safety23)

#reorder the groups for treatment type : 
Safety23$Tr.Type_Full <- factor(Safety23$Tr.Type_Full , levels=c("Control", "MSF Group", "HARU", "MYLO", 
                                                                 "MYSC", "THAR", "TUOC", "NF Group", 
                                                                 "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))
## Plot Model Predictions --------------------------------------------------

### Predict Model Values --------------------------------------------------
# Predict model values for glm.nb1 top model, use type=link as it is on the negative binomial scale
#pred <- predict(model, type="")
predicted_link <- predict(glm.s.nb1, type = "link", se.fit = TRUE)

# Calculate the confidence intervals on the link scale
link_fit <- predicted_link$fit
link_se <- predicted_link$se.fit

z_value <- qnorm(0.975)  # 97.5 percentile point for a normal distribution (for 95% CI)
link_lower <- link_fit - z_value * link_se
link_upper <- link_fit + z_value * link_se

# Transform the confidence intervals back to the response scale
response_lower <- exp(link_lower)
response_upper <- exp(link_upper)

# Predict on the response scale
predicted_vals <- predict(glm.s.nb1, type = "response")

# Create a dataframe with observed values, predicted values, and confidence intervals
predicted_data <- data.frame(
  Trial = Safety23$Trial,
  Observed = Safety23$Total.spp.rec.trial,
  Predicted = predicted_vals,
  Treatment = Safety23$Treatment_Full,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)

# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)
head(Safety23)
# Plot using ggplot predicted values with confidence intervals 
# Define the colors
treatment_colors <- c(
  "Control" = "darkgray",
  "MSF Group" = "cornflowerblue",
  "NF Group" = "seagreen3",
  "MSF Individual" = "cadetblue2",
  "NF Individual" = "lightgreen"
)

shapes <- c("Control" = 16, 
            "MSF Group" = 17, 
            "NF Group" = 18,
            "MSF Individual" = 2,
            "NF Individual" = 5)  # Example shapes

ggplot(predicted_data, aes(x = Treatment, y = Predicted)) +
  geom_jitter(aes(Treatment, Observed, 
                  colour = Treatment, shape = Treatment), width = 0.1, size = 3) +  # Add observed values offset 
  geom_point(colour = "black", size = 4) +  # Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  # CIs
  labs(x = "Treatment", y = "Number of Species Responding") +  # Axis labels
  theme_classic() +  # Customize the theme
  theme(text = element_text(family = "Times New Roman"),  # Change the font to Times New Roman
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.position = "none") +
  scale_color_manual(values = treatment_colors) +
  scale_shape_manual(values = shapes)  # Specify shapes


# Model Treatment Type No.Individuals -------------------------------------
head(Safety23)
#To answer hyp 2, we used the top model candidate from model selection to determine
#which species were significant to the no of individuals recruited
glm.nbtt <- glm.nb(Total.Recs~Tr.Type, data = Safety23)

summary(glm.nbtt)

#Calculate confidence intervals
confint(glm.nbtt)

## Plot Model Predictions --------------------------------------------------

## Predict Model Values --------------------------------------------------

# Predict model values for glm.nb1 top model, use type=link as it is on the negative binomial scale
#pred <- predict(model, type="")
predicted_link <- predict(glm.nbtt, type = "link", se.fit = TRUE)

# Calculate the confidence intervals on the link scale
link_fit <- predicted_link$fit
link_se <- predicted_link$se.fit

z_value <- qnorm(0.975)  # 97.5 percentile point for a normal distribution (for 95% CI)
link_lower <- link_fit - z_value * link_se
link_upper <- link_fit + z_value * link_se

# Transform the confidence intervals back to the response scale
response_lower <- exp(link_lower)
response_upper <- exp(link_upper)

# Predict on the response scale
predicted_vals <- predict(glm.nbtt, type = "response")

# Create a dataframe with observed values, predicted values, and confidence intervals
predicted_data <- data.frame(
  Trial = Safety23$Trial,
  Observed = Safety23$Total.Recs,
  Predicted = predicted_vals,
  Treatment = Safety23$Treatment_Full,
  TrType = Safety23$Tr.Type_Full,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)
head(Safety23)
# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)

# Plot using ggplot predicted values with confidence intervals 

## Define the colors
treatment_colors <- c(
  "Control" = "darkgray",
  "MSF Group" = "cornflowerblue",
  "NF Group" = "seagreen3",
  "MSF Individual" = "cadetblue3",
  "NF Individual" = "springgreen"
)
colours()
shapes <- c("Control" = 16, 
            "MSF Group" = 17, 
            "NF Group" = 18,
            "MSF Individual" = 2,
            "NF Individual" = 5)  # Example shapes

ggplot(predicted_data, aes(x = TrType, y = Predicted)) +
  geom_jitter(aes(TrType, Observed, 
                  colour = Treatment, shape = Treatment), width = 0.1, size = 3) + #Add observed values offset 
  geom_point(colour="black", size=4) +  #Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  #CIs
  labs(x = "Treatment", y = "Number of Individuals Responding") +  #Axis labels
  theme_classic()  +  #Customize the theme
  theme(text = element_text(family = "Times New Roman"),  # Change the font to Times New Roman, 
        axis.title = element_text(size = 19), 
        axis.text.x = element_text(angle = 40, hjust=1, vjust=1,size = 15), 
        axis.text.y = element_text(size = 17), legend.position="none") +
  scale_color_manual(values = treatment_colors) +
  scale_shape_manual(values = shapes)  # Specify shapes



# Model Treatment Type No.Species -------------------------------------
head(Safety23)
#To answer hyp 2, we used the top model candidate from model selection to determine
#which species were significant to the no of individuals recruited
glmS.nbtt <- glm.nb(Total.spp.rec.trial~Tr.Type, data = Safety23)
summary(glmS.nbtt)

#calculate confidence intervals
confint(glmS.nbtt)

# Predict model values for glmS.nb1 top model, use type=link as it is on the negative binomial scale
predicted_link <- predict(glmS.nbtt, type = "link", se.fit = TRUE)

# Calculate the confidence intervals on the link scale
link_fit <- predicted_link$fit
link_se <- predicted_link$se.fit

z_value <- qnorm(0.975)  # 97.5 percentile point for a normal distribution (for 95% CI)
link_lower <- link_fit - z_value * link_se
link_upper <- link_fit + z_value * link_se

# Transform the confidence intervals back to the response scale
response_lower <- exp(link_lower)
response_upper <- exp(link_upper)

# Predict on the response scale
predicted_vals <- predict(glmS.nbtt, type = "response")

# Create a dataframe with observed values, predicted values, and confidence intervals
predicted_data <- data.frame(
  Trial = Safety23$Trial,
  Observed = Safety23$Total.spp.rec.trial,
  Predicted = predicted_vals,
  Treatment = Safety23$Treatment_Full,
  TrType = Safety23$Tr.Type_Full,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)
head(Safety23)
# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)


# Plot using ggplot predicted values with confidence intervals 

## Define the colors
treatment_colors <- c(
  "Control" = "darkgray",
  "MSF Group" = "cornflowerblue",
  "NF Group" = "seagreen3",
  "MSF Individual" = "cadetblue3",
  "NF Individual" = "springgreen"
)
colours()
shapes <- c("Control" = 16, 
            "MSF Group" = 17, 
            "NF Group" = 18,
            "MSF Individual" = 2,
            "NF Individual" = 5)  # Example shapes

ggplot(predicted_data, aes(x = TrType, y = Predicted)) +
  geom_jitter(aes(TrType, Observed, 
                  colour = Treatment, shape = Treatment), width = 0.1, size = 3) + #Add observed values offset 
  geom_point(colour="black", size=4) +  #Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  #CIs
  labs(x = "Treatment", y = "Number of Species Responding") +  #Axis labels
  theme_classic()  +  #Customize the theme
  theme(text = element_text(family = "Times New Roman"),  # Change the font to Times New Roman, 
        axis.title = element_text(size = 19), 
        axis.text.x = element_text(angle = 40, hjust=1, vjust=1,size = 15), 
        axis.text.y = element_text(size = 17), legend.position="none") +
  scale_color_manual(values = treatment_colors) +
  scale_shape_manual(values = shapes)  # Specify shapes


# Determine responder species ---------------------------------------------
### This section is to prepare data for Fisher's exact test to determine whether the response rate of species with specific foraging strategies differed from expected based on the community available to respond that employ same or different foraging strategies as the significant playback species, THAR and MYLO

head(Safety23)
pre.v.postTFLA30 <- read.csv("pre.v.postTFLA30.csv", header=TRUE)
head(pre.v.postTFLA30)
pre.v.postVLA30 <- read.csv("pre.v.postVLA30.csv", header=TRUE)
head(pre.v.postVLA30)
safetyTFLA.R2.comb <- read.csv("safetyTFLA.R2.comb.csv", header=TRUE)
head(safetyTFLA.R2.comb)
safetyVLA.R2.comb <- read.csv("safetyVLA.R2.comb.csv", header=TRUE)
head(safetyVLA.R2.comb)


# Combine all dfs ---------------------------------------------------------

# Add a new column "forest type" with the value "tf" for all rows
pre.v.postTFLA30 <- pre.v.postTFLA30 %>%
  mutate(`Forest.type` = "TF") %>% 
  select_at(vars(-6, -7))

# Add a new column "forest type" with the value "VA" for all rows
pre.v.postVLA30 <- pre.v.postVLA30 %>%
  mutate(`Forest.type` = "VA") %>% 
  select_at(vars(-6, -7))

# Change R2.recruit column to just recruit to match previous dfs
safetyTFLA.R2.comb <- safetyTFLA.R2.comb %>% 
  rename(Recruit = R2.recruit) %>% 
  select_at(vars(-6, -7))

safetyVLA.R2.comb <- safetyVLA.R2.comb %>% 
  rename(Recruit = R2.recruit) %>% 
  select_at(vars(-2, -8, -9, -4))

head(pre.v.postTFLA30)
head(pre.v.postVLA30)
head(safetyTFLA.R2.comb)
head(safetyVLA.R2.comb)
spp.response <- rbind(pre.v.postTFLA30, pre.v.postVLA30, safetyTFLA.R2.comb, safetyVLA.R2.comb)
head(spp.response)

##Add treatment types from response 1 and 2 from both forest types####
####Add column for Tr.type Response 1 TF
head(spp.response)
spp.response <- spp.response %>%
  mutate(Tr.Type = str_replace_all(Exemplar, "[0-9]", "")) %>% 
  mutate(Spp = str_replace_all(Spp, "[0-9]", ""))
unique(spp.response$Tr.Type)


#Issue with exemplars -- one is still called VA3 -- this is a control treatment
# Filter rows where Exemplar is "VA3"
row_with_va3 <- spp.response %>%
  filter(Exemplar == "VA3")

# View the result
print(row_with_va3)

# Replace "VA3" with "Control" in the Exemplar column
spp.response <- spp.response %>%
  mutate(Exemplar = ifelse(Exemplar == "VA3", "CTRL", Exemplar))


###Find responses all signficant species####
###Find responses TFLA all significant spp
#Make new dataframe for responding spp Response 1
TH.MY.response <- spp.response %>%
  filter(Tr.Type %in% c("THAR", "MYLO"), Recruit>=1) %>%
  select_at(vars(1,8,4))

THAR.response <- spp.response %>%
  filter(Tr.Type %in% c("THAR"), Recruit>=1) %>%
  select_at(vars(1,8,4))

#Count number of times each spp was counted as a response
table(THAR.response$Spp)

MYLO.response <- spp.response %>%
  filter(Tr.Type %in% c("MYLO"), Recruit>=1) %>%
  select_at(vars(1,8,4))
#Count number of times each spp was counted as a response
table(MYLO.response$Spp)

THAR.response.df <- table(THAR.response$Spp)
MYLO.response.df <- table(MYLO.response$Spp)
spp.response.df <- table(TH.MY.response$Spp)

###Make new dataframe grouping by spp
spp.counts <- TH.MY.response %>%
  group_by(Tr.Type, Spp) %>% 
  summarise(count = n())

THAR.count <- THAR.response %>%
  group_by(Tr.Type, Spp) %>% 
  summarise(count = n())

MYLO.count <- MYLO.response %>%
  group_by(Tr.Type, Spp) %>% 
  summarise(count = n())

###plot HARU
ggplot(subset(spp.counts, Tr.Type=="MYLO"), aes(Spp, count)) +
  geom_bar(stat = "identity") +theme_minimal() +  
  theme(axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15), legend.position="none") +
  xlab("MYLO")

ggplot(subset(spp.counts, Tr.Type=="THAR"), aes(Spp, count)) +
  geom_bar(stat = "identity") +theme_minimal() +  
  theme(axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15), legend.position="none") +
  xlab("THAR")

TH.MY.response
spp.counts

###Make new dataframe grouping by spp regardless of treatment
all.spp.counts <- TH.MY.response %>%
  group_by(Spp) %>% 
  summarise(count = n())

all.spp.counts

THAR.spp.counts <- THAR.response %>%
  group_by(Spp) %>% 
  summarise(count = n())

MYLO.spp.counts <- MYLO.response %>%
  group_by(Spp) %>% 
  summarise(count = n())

# Add a new column "foraging_ecology" with the value "NA" for all rows
all.spp.thmy <- all.spp.counts %>%
  select_at(vars(-2)) %>% 
  mutate(`foraging_ecology` = "")

all.spp.THAR <- THAR.spp.counts %>%
  select_at(vars(-2)) %>% 
  mutate(`Maneuver` = "")

all.spp.MYLO <- MYLO.spp.counts %>%
  select_at(vars(-2)) %>% 
  mutate(`Maneuver` = "")

#write csv for foraging ecology
write.csv(all.spp.thmy, "Responders_Foraging_Ecology.csv", row.names = FALSE)

write.csv(all.spp.THAR, "THAR_Responders_Foraging_Ecology.csv", row.names = FALSE)
write.csv(all.spp.MYLO, "MYLO_Responders_Foraging_Ecology.csv", row.names = FALSE)


# THAR all species detected in trials--------------------------------------------------------
head(spp.response)
THAR.spp <- spp.response %>%
  filter(Tr.Type %in% c("THAR")) %>%
  select_at(vars(1,8,4,6))
head(THAR.spp)
nrow(THAR.spp)

# Summarize the data
THAR.spp_summary <- THAR.spp %>%
  filter(!is.na(Recruit)) %>% 
  group_by(Spp) %>%
  summarise(
    total_recruit = sum(Recruit), # Sum of Recruit column (0 or 1)
    count = n()                   # Count of rows for each species
  ) %>% 
  mutate(`Maneuver` = "")

write.csv(THAR.spp_summary, "THAR_Spp_Detected_Foraging_Ecology.csv", row.names = FALSE)

# MYLO all species detected in trials -------------------------------------
head(spp.response)
MYLO.spp <- spp.response %>%
  filter(Tr.Type %in% c("MYLO")) %>%
  select_at(vars(1,8,4,6))
head(MYLO.spp)
nrow(MYLO.spp)

# Summarize the data
MYLO.spp_summary <- MYLO.spp %>%
  filter(!is.na(Recruit)) %>%
  group_by(Spp) %>%
  summarise(
    total_recruit = sum(Recruit), # Sum of Recruit column (0 or 1)
    count = n()                   # Count of rows for each species
  ) %>% 
  mutate(`Maneuver` = "")

write.csv(MYLO.spp_summary, "MYLO_Spp_Detected_Foraging_Ecology.csv", row.names = FALSE)
