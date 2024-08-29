##################Code created by Chelsey May 16 2024, updated July 15 2024

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
glm.p2<-glm(Total.Recs~Forest.type, family="poisson", data=Safety23)


# Two covariates Models
glm.p3<-glm(Total.Recs~Treatment+Forest.type, family="poisson", data=Safety23)

# Two covariates with interaction term
glm.p4 <- glm(Total.Recs~Treatment+Forest.type+Treatment*Forest.type, family = "poisson", data = Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.pN1, glm.p1, glm.p2, glm.p3, glm.p4, rank = "AICc") 
best.mod
summary(glm.p1)


## Poisson Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.pN1 <- glmer(Total.Recs~1+(1|Point), family = "poisson", data = Safety23)

# Single Fixed-effect Model
glmer.p1<-glmer(Total.Recs~Treatment + (1|Point), family="poisson", data=Safety23)
glmer.p2 <- glmer(Total.Recs~Forest.type+(1|Point), family = "poisson", data = Safety23)

# Two covariates Models
glmer.p3<-glmer(Total.Recs~Treatment+Forest.type+ (1|Point), family="poisson", data=Safety23)

# Two covariates with interaction term
glmer.p4<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point), family="poisson", data=Safety23) #Warning convergence

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.pN1, glmer.p1, glmer.p2, glmer.p3, glmer.p4, rank = "AICc") 
best.mod
summary(glmer.p1)

## Negative Binomial Distribution GLMs -----------------------------------------------------------------

# Intercept Only Model 
glm.nbN1 <- glm.nb(Total.Recs~1, data = Safety23)

# Single Fixed-effect Model
glm.nb1<-glm.nb(Total.Recs~Treatment, data=Safety23)
glm.nb2 <- glm.nb(Total.Recs~Forest.type, data = Safety23)

# Two covariates Models
glm.nb3<-glm.nb(Total.Recs~Treatment+Forest.type, data=Safety23) 

# Two covariates with interaction term
glm.nb4<-glm.nb(Total.Recs~Treatment+Forest.type + Treatment*Forest.type, data=Safety23) 

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glm.nbN1, glm.nb1, glm.nb2, glm.nb3, glm.nb4, rank = "AICc") 
best.mod
summary(glm.nb1)

## Negative Binomial Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmer.nbN1 <- glmer.nb(Total.Recs~1+(1|Point), data = Safety23) #Boundary singular

# Single Fixed-effect Model
glmer.nb1<-glmer.nb(Total.Recs~Treatment + (1|Point), data=Safety23) #Boundary singular
glmer.nb2 <- glmer.nb(Total.Recs~Forest.type+(1|Point), data = Safety23) #Boundary singular

# Two covariates Models
glmer.nb3<-glmer.nb(Total.Recs~Treatment+Forest.type+ (1|Point), data=Safety23) #Warning convergence

# Two covariates with interaction term
glmer.nb4<-glmer.nb(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point), data=Safety23) #Warning convergence

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmer.nbN1, glmer.nb1, glmer.nb2, glmer.nb3, glmer.nb4, rank = "AICc") 
best.mod
summary(glmer.nb1)


## Model Selection with all Models -----------------------------------------
# Here I compare models from all distribution types and with all combinations of covariates to determine the best model
best.mod <- model.sel(glm.pN1, glm.p1, glm.p2, glm.p3, glm.p4, glmer.pN1, glmer.p1, glmer.p2, glmer.p3, glmer.p4, glm.nbN1, glm.nb1, glm.nb2, glm.nb3, glm.nb4, glmer.nbN1, glmer.nb1, glmer.nb2, glmer.nb3, glmer.nb4, rank = "AICc") 
best.mod
# Top model is GLM with negative binomial distribution and treatment as covariate, 
# Next is GLM with negative binomial distribution and both treatment and forest type covariates
summary(glm.nb1)
#Determine confidence intervals for each treatment
confint(glm.nb1)
#Run log likelihood
logLik(glm.nb1)

## Goodness of Fit Number of Individuals -----------------------------------

#To test the goodness of fit of the top model candidate, I plotted model residuals against the predicted line, and plotted a histogram of simulated residuals against fitted residuals 
qqnorm(resid(glm.nb1))
qqline(resid(glm.nb1))
       
gsimOut <- simulateResiduals(glm.nb1)
plot(gsimOut)
testDispersion(glm.nb1)

## Model Selection Table ---------------------------------------------------

# Make model selection table to data frame
#include model terms, random var, intercept, k, df, logLik, delta AICc, weight
best.mod.table <- as.data.frame(best.mod)
#Name models
best.mod.table$Model <- 1:20


#Create column with model terms:
# Column names to check for the plus sign
columns_to_check <- c("Forest.type", "Treatment", "Forest.type:Treatment")

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
  mutate(Model_Terms = str_replace(Model_Terms, "Forest.type", "Forest Type")) %>% 
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glm.nb4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glm.p4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmer.nb4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmer.p4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
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
  select_at(vars(6, 10, 11, 12, 13, 14, 15, 16)) %>% #Keep only desired columns -- Model type, logLik, AICc, delta, weight, model, model terms, Family
  select_at(vars(6, 7, 8, everything())) %>% #Rearrange columns so last two are first two
  mutate(across(where(is.numeric), ~round(., 2))) %>% #Reduce number of decimal points
  mutate(k = c(2, 3, 2, 3, 1, 4, 2, 1,4,2,2,3,4,1,2,2,3,4,1,2)) %>%  #Add column for parameters
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
summary(glm.nb3)
confint.nb3 <- as.data.frame(confint(glm.nb3))
confint.nb3 <- confint.nb3 %>% rename(lwr="2.5 %", upr="97.5 %") %>% 
  mutate(lwr = round(lwr, 3), upr = round(upr, 3))
confint.nb3$range <- paste0("(", confint.nb3$lwr, ",", confint.nb3$upr, ")")
confint.nb3 <- confint.nb3 %>% dplyr::select(range)

sum.glm.nb3 <- as.data.frame(tidy(glm.nb3))
sum.glm.nb3 <- cbind(sum.glm.nb3, confint.nb3)
sum.glm.nb3 <- sum.glm.nb3 %>% dplyr::select(-std.error, -statistic) %>% 
  mutate(model_number = rep(2)) %>% 
  mutate(estimate = round(estimate, 3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  dplyr::select(model_number, everything())

row.names(sum.glm.nb1) <- NULL
row.names(sum.glm.nb3) <- NULL
coef.table <- rbind(sum.glm.nb1, sum.glm.nb3)
coef.table <- coef.table %>% rename(Estimate=estimate, "p-value"=p.value, "Model Number"=model_number, "95% CI"=range) %>% mutate(term = str_replace_all(term, fixed("(Intercept)"), "Intercept")) %>% mutate(term = if_else(term == "TreatmentMSF", "Grouped MSF", term))  %>% 
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
  labs(x = "Treatment", y = "Number of Individuals Recruited") +  # Axis labels
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
predicted_vals <- predict(glm.nb1, type = "response")

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
  labs(x = "Treatment", y = "Number of Individuals Recruited") +  #Axis labels
  theme_classic()  +  #Customize the theme
  theme(text = element_text(family = "Times New Roman"),  # Change the font to Times New Roman, 
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 40, hjust=1, vjust=1,size = 15), 
        axis.text.y = element_text(size = 17), legend.position="none") +
  scale_color_manual(values = treatment_colors) +
  scale_shape_manual(values = shapes)  # Specify shapes


# Model Selection No. Species ---------------------------------------------------------

## Visualize Response Data No. Species----------------------------------------------------------

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
  geom_point() + theme_bw() + labs(x="Treatment", y="Mean No. Recruits")


## Poisson Distribution GLM Models -----------------------------------------

# Intercept Only Model 
glmS.pN1<-glm(Total.spp.rec.trial~1,family="poisson",data=Safety23)

# Single Fixed-effect Model
# These models consider one predictor at a time to gauge their individual impacts on the dependent variable.
glmS.p1<-glm(Total.spp.rec.trial~Treatment,family="poisson",data=Safety23) 
glmS.p2<-glm(Total.spp.rec.trial~Forest.type, family="poisson", data=Safety23)


# Two covariates Models
glmS.p3<-glm(Total.spp.rec.trial~Treatment+Forest.type, family="poisson", data=Safety23)

# Two covariates with interaction term
glmS.p4 <- glm(Total.spp.rec.trial~Treatment+Forest.type+Treatment*Forest.type, family = "poisson", data = Safety23)

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmS.pN1, glmS.p1, glmS.p2, glmS.p3, glmS.p4, rank = "AICc") 
best.mod
summary(glmS.p1)


## Poisson Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmerS.pN1 <- glmer(Total.spp.rec.trial~1+(1|Point), family = "poisson", data = Safety23)

# Single Fixed-effect Model
glmerS.p1<-glmer(Total.spp.rec.trial~Treatment + (1|Point), family="poisson", data=Safety23)
glmerS.p2 <- glmer(Total.spp.rec.trial~Forest.type+(1|Point), family = "poisson", data = Safety23)

# Two covariates Models
glmerS.p3<-glmer(Total.spp.rec.trial~Treatment+Forest.type+ (1|Point), family="poisson", data=Safety23)

# Two covariates with interaction term
glmerS.p4<-glmer(Total.spp.rec.trial~Treatment+Forest.type + Treatment*Forest.type + (1|Point), family="poisson", data=Safety23) #Warning convergence

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmerS.pN1, glmerS.p1, glmerS.p2, glmerS.p3, glmerS.p4, rank = "AICc") 
best.mod
summary(glmer.p1)

## Negative Binomial Distribution GLMs -----------------------------------------------------------------

# Intercept Only Model 
glmS.nbN1 <- glm.nb(Total.spp.rec.trial~1, data = Safety23)

# Single Fixed-effect Model
glmS.nb1<-glm.nb(Total.spp.rec.trial~Treatment, data=Safety23)
glmS.nb2 <- glm.nb(Total.spp.rec.trial~Forest.type, data = Safety23)

# Two covariates Models
glmS.nb3<-glm.nb(Total.spp.rec.trial~Treatment+Forest.type, data=Safety23) 

# Two covariates with interaction term
glmS.nb4<-glm.nb(Total.spp.rec.trial~Treatment+Forest.type + Treatment*Forest.type, data=Safety23) 

# Model selection glm Poisson number of species recruited
best.mod <- model.sel(glmS.nbN1, glmS.nb1, glmS.nb2, glmS.nb3, glmS.nb4, rank = "AICc") 
best.mod
summary(glmS.nb1)

## Negative Binomial Distribution GLMMs -----------------------------------------------------------------

# Adding in random effect to explore effects

# Intercept Only Model 
glmerS.nbN1 <- glmer.nb(Total.spp.rec.trial~1+(1|Point), data = Safety23) #Boundary singular

# Single Fixed-effect Model
glmerS.nb1<-glmer.nb(Total.spp.rec.trial~Treatment + (1|Point), data=Safety23) #Boundary singular
glmerS.nb2 <- glmer.nb(Total.spp.rec.trial~Forest.type+(1|Point), data = Safety23) #Boundary singular

# Two covariates Models
glmerS.nb3<-glmer.nb(Total.spp.rec.trial~Treatment+Forest.type+ (1|Point), data=Safety23) #Warning convergence

# Two covariates with interaction term
glmerS.nb4<-glmer.nb(Total.spp.rec.trial~Treatment+Forest.type + Treatment*Forest.type + (1|Point), data=Safety23) #Warning convergence/Boundary singular

# Model selection glm Poisson number of recruits
best.mod <- model.sel(glmerS.nbN1, glmerS.nb1, glmerS.nb2, glmerS.nb3, glmerS.nb4, rank = "AICc") 
best.mod
summary(glmerS.nb1)


## Model Selection with all Models -----------------------------------------

best.mod <- model.sel(glmS.pN1, glmS.p1, glmS.p2, glmS.p3, glmS.p4, glmerS.pN1, glmerS.p1, glmerS.p2, glmerS.p3, glmerS.p4, glmS.nbN1, glmS.nb1, glmS.nb2, glmS.nb3, glmS.nb4, glmerS.nbN1, glmerS.nb1, glmerS.nb2, glmerS.nb3, glmerS.nb4, rank = "AICc") 
best.mod
summary(glmS.nb1)
confint(glmS.nb1)
summary(glmerS.nb3)

## Coefficients Table for Top Two Models -----------------------------------

#To show the effects of forest type are negligible even though the second top model includes it, I created a table showing coefficients for both two top models, including model term, estimate, and p values

summary(glmS.nb1)
confint.Snb1 <- as.data.frame(confint(glmS.nb1))
confint.Snb1 <- confint.Snb1 %>% rename(lwr="2.5 %", upr="97.5 %") %>% 
  mutate(lwr = round(lwr, 3), upr = round(upr, 3))
confint.Snb1$range <- paste0("(", confint.Snb1$lwr, ",", confint.Snb1$upr, ")")
confint.Snb1 <- confint.Snb1 %>% dplyr::select(range)

sum.glm.Snb1 <- as.data.frame(tidy(glmS.nb1))
sum.glm.Snb1 <- cbind(sum.glm.Snb1, confint.Snb1)
sum.glm.Snb1 <- sum.glm.Snb1 %>% dplyr::select(-std.error, -statistic) %>% 
  mutate(model_number = rep(1)) %>%
  mutate(estimate = round(estimate, 3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  dplyr::select(model_number, everything())


summary(glmS.nb3)
confint.Snb3 <- as.data.frame(confint(glmS.nb3))
confint.Snb3 <- confint.Snb3 %>% rename(lwr="2.5 %", upr="97.5 %") %>% 
  mutate(lwr = round(lwr, 3), upr = round(upr, 3))
confint.Snb3$range <- paste0("(", confint.Snb3$lwr, ",", confint.Snb3$upr, ")")
confint.Snb3 <- confint.Snb3 %>% dplyr::select(range)

sum.glm.Snb3 <- as.data.frame(tidy(glmS.nb3))
sum.glm.Snb3 <- cbind(sum.glm.Snb3, confint.Snb3)
sum.glm.Snb3 <- sum.glm.Snb3 %>% dplyr::select(-std.error, -statistic) %>% 
  mutate(model_number = rep(2)) %>%
  mutate(estimate = round(estimate, 3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  dplyr::select(model_number, everything())

row.names(sum.glm.Snb1) <- NULL
row.names(sum.glm.Snb3) <- NULL
coef.table <- rbind(sum.glm.Snb1, sum.glm.Snb3)
coef.table <- coef.table %>% rename(Estimate=estimate, "p-value"=p.value, "Model Number"=model_number, "95% CI"=range) %>% 
  mutate(term = str_replace_all(term, fixed("(Intercept)"), "Intercept")) %>%
  mutate(term = if_else(term == "TreatmentMSF", "Grouped MSF", term))  %>% 
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


## Goodness of Fit Number of Individuals -----------------------------------

#To test the goodness of fit of the top model candidate, I plotted model residuals against the predicted line, and plotted a histogram of simulated residuals against fitted residuals 
qqnorm(resid(glmS.nb1))
qqline(resid(glmS.nb1))

gsimOut <- simulateResiduals(glmS.nb1)
plot(gsimOut)
testDispersion(glmS.nb1)


# Make Model Selection Table to Data Frame --------------------------------

#include model terms, random var, intercept, k, df, logLik, delta BIC, weight
best.mod.table <- as.data.frame(best.mod)
#Name models
best.mod.table$Model <- 1:20


#Create column with model terms:
# Column names to check for the plus sign
columns_to_check <- c("Forest.type", "Treatment", "Forest.type:Treatment")

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

#Add terms for null model and random effect for M7
best.mod.table["glmS.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmerS.nbN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmerS.pN1", "Model_Terms"] <- "Null Model"
best.mod.table["glmS.pN1", "Model_Terms"] <- "Null Model"
best.mod.table <- best.mod.table %>%
  mutate(Model_Terms = str_replace(Model_Terms, "Forest.type", "Forest Type")) %>% 
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmS.nb4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmS.p4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmerS.nb4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(Model_Terms = replace(Model_Terms, row_number() == which(rownames(best.mod.table) == "glmerS.p4"), 
                               "Forest Type + Treatment + Forest Type*Treatment")) %>%
  mutate(class = str_replace(class, "glmerMod", "GLMM")) %>% 
  mutate(class = str_replace(class, "glm", "GLM")) %>% 
  rename("Model_Type"=class) %>% 
  mutate(Family = str_extract(family, "^[A-Za-z]+\\s[A-Za-z]+|^[A-Za-z]+")) %>%
  mutate(Model_Type = str_replace(Model_Type, "negbin", "GLM")) %>%
  mutate(Family = str_replace(Family, "poisson", "Poisson"))


print(best.mod.table)

#Remove unnecessary columns
best.mod.table <- best.mod.table %>% 
  select_at(vars(6, 10, 11, 12, 13, 14, 15, 16)) %>% #Keep only desired columns -- Model type, logLik, AICc, delta, weight, model, model terms, Family
  select_at(vars(6, 7, 8, everything())) %>% #Rearrange columns so last two are first two
  mutate(across(where(is.numeric), ~round(., 2))) %>% #Reduce number of decimal points
  mutate(k = c(2, 3, 2, 3, 1, 4, 2, 1,4,2,2,3,4,1,2,2,3,4,1,2)) %>%  #Add column for parameters
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


## Plot Model Predictions No.Species --------------------------------------------------

# Predict model values for glmS.nb1 top model, use type=link as it is on the negative binomial scale
predicted_link <- predict(glmS.nb1, type = "link", se.fit = TRUE)

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
predicted_vals <- predict(glmS.nb1, type = "response")

# Create a dataframe with observed values, predicted values, and confidence intervals
predicted_data <- data.frame(
  Trial = Safety23$Trial,
  Observed = Safety23$Total.Recs,
  Predicted = predicted_vals,
  Treatment = Safety23$Treatment,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)
head(Safety23)
# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)

# Plot using ggplot predicted values with confidence intervals 
ggplot(predicted_data, aes(x = Treatment, y = Predicted)) +
  geom_jitter(aes(Treatment, Observed, 
                  colour=Treatment), width = 0.1) + #Add observed values offset 
  geom_point(colour="black", size=4) +  #Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  #CIs
  labs(x = "Treatment", y = "Number of Species Recruited") +  #Axis labels
  theme_classic()  +  #Customize the theme
  theme(text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 00, size = 12), 
        axis.text.y = element_text(size = 15), legend.position="none") 

# Model Treatment Type No.Species -------------------------------------
head(Safety23)
#To answer hyp 2, we used the top model candidate from model selection to determine
#which species were significant to the no of individuals recruited
glmS.nbtt <- glm.nb(Total.spp.rec.trial~Tr.Type, data = Safety23)
summary(glmS.nbtt)

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
  Observed = Safety23$Total.Recs,
  Predicted = predicted_vals,
  Treatment = Safety23$Tr.Type,
  CI_Lower = response_lower,
  CI_Upper = response_upper
)
head(predicted_data)
head(Safety23)
# Combine predicted values and prediction intervals
predicted_data <- cbind(predicted_data, predicted_vals)
head(predicted_data)

# Plot using ggplot predicted values with confidence intervals 
ggplot(predicted_data, aes(x = Treatment, y = Predicted)) +
  geom_jitter(aes(Treatment, Observed, 
                  colour=Treatment), width = 0.1) + #Add observed values offset 
  geom_point(colour="black", size=4) +  #Predicted mean
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +  #CIs
  labs(x = "Treatment", y = "Number of Species Recruited") +  #Axis labels
  theme_classic()  +  #Customize the theme
  theme(text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(angle = 90, size = 12), 
        axis.text.y = element_text(size = 15), legend.position="none") 

