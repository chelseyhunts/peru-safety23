###combination of tf and v dfs & Model selection and 

###Required packages####
#library MuMIn --install for model selection
#install.packages("MuMIn")
library(MuMIn)
#matrix dependent package
#install.packages("Matrix")
library(Matrix)
#glmm package
library(lme4)
#betareg package
library(glmmTMB)
#plotting model predictions
library(ggplot2) #loaded in tidyverse
#install.packages("ggeffects")
library(ggeffects)
#annotating plots
library(ggpubr)
library(tidyverse) #for use of dplyr and ggplot
#install.packages("optimx")
library(optimx)
#install.packages("minqa")
library(minqa)
#install.packages("dfoptim")
library(dfoptim)

###Combine plots for side by side of no and prop recruits for both forest types
#First need to remove axis titles

P1 <- prop.plotTFLAcomb + theme(axis.title.x = element_blank())
P2 <- ggplot(subset(VLA.Rcomb,Treatment%in% c("MSF","CTRL","NF")),
             aes(x=Treatment, y=Total.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="") + theme_bw() +
  theme(axis.title.x = element_blank(), axis.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Paired")+
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") 
N1 <- ggplot(subset(TFLA.Rcomb, Treatment %in% c("MSF", "CTRL", "NF")), 
             aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Tierra Firme", y="No. Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  scale_fill_brewer(palette = "Paired") +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw()+
  theme(axis.title = element_text(size = 20))+
  ylim(NA, 12.5)
N2 <- ggplot(subset(VLA.Rcomb,Treatment%in% c("MSF","CTRL","NF")), aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Varzea",y="") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  scale_fill_brewer(palette = "Paired")+
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size=20)) +
  ylim(NA,12.5)

pcombRcomb <- ggarrange(P1, P2, N1, N2,
                         labels = c("A","B","C", "D"),
                         ncol=2, nrow=2, 
                         common.legend = TRUE, legend = "right")
pcombRcomb

###Plot TFLA response 1&2 by treatment type####
N3 <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Tierra Firme", y="No. Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 16)

###Plot TFLA proportion response 1&2 by treatment type####
P3 <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x= "", y="Proportion Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") +theme_bw() +
  theme(axis.title = element_text(size = 20), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.7)

pcombTFR.type <- ggarrange(P3, N3,
                           labels = c("A", "B"),
                           ncol=1, nrow=2, 
                           legend = "none")
pcombTFR.type

###Plot VLA response 1&2 by treatment type####
N4 <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Varzea", y="") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 16)

###Plot VLA proportion response 1&2 by treatment type####
P4 <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x= "", y="") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.7)

pcombVR.type <- ggarrange(prop.plotV.type, num.plotV.type,
                          labels = c("C", "D"),
                          ncol=1, nrow=2, 
                          legend = "none")
pcombVR.type

pcombRtypecomb <- ggarrange(P3, P4, N3, N4,
                        labels = c("A","B","C", "D"),
                        ncol=2, nrow=2, 
                        common.legend = TRUE, legend = "right")
pcombRtypecomb



###Response 1 & 2 Combine####
head(TFLA.Rcomb)
TFLA.Rcomb <- read.csv("TFLA.Rcomb.csv")
head(VLA.Rcomb)
VLA.Rcomb <- read.csv("VLA.Rcomb.csv")
Safety23 <- rbind(TFLA.Rcomb, VLA.Rcomb)
head(Safety23)
unique(Safety23$Forest.type)
write.csv(Safety23, "Safety23.csv", row.names = FALSE)
Safety23 <- read.csv("Safety23.csv", header = TRUE, stringsAsFactors = TRUE)
glimpse(Safety23)

#reorder the groups order : I change the order of the factor data$names to order plot
Safety23$Tr.Type <- factor(Safety23$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

####Plot all number combined response 1&2 by treatment####
num.plot.all <- ggplot(Safety23, aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15))

####Plot all proportion response 1&2 by treatment type####
prop.plot.all <- ggplot(Safety23, aes(x=Treatment, y=Total.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) 

p.ALL <- ggarrange(prop.plot.all, num.plot.all,
                          labels = c("A", "B"),
                          ncol=1, nrow=2, 
                          legend = "none")
p.ALL

###Add title to figure
annotate_figure(p.ALL, top = text_grob("Response 1 & 2 in Both Forest Types", 
                                              color = "black", face = "bold", size = 20))

####Plot all number combined response 1&2 by treatment type####
num.plot.all.type <- ggplot(Safety23, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15)) 

####Plot all proportion combined response 1&2 by treatment type####
prop.plot.all.type <- ggplot(Safety23, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) 

p.ALL.type <- ggarrange(prop.plot.all.type, num.plot.all.type,
                   labels = c("C", "D"),
                   ncol=1, nrow=2, 
                   legend = "none")
p.ALL.type

###Add title to figure
annotate_figure(p.ALL.type, top = text_grob("Response 1 & 2 in Both Forest Types", 
                                       color = "black", face = "bold", size = 20))


###Net dist combine####
net.dist.VLA <- read.csv("net.dist.VLA23.csv")
head(net.dist.VLA)
net.dist.TFLA <- read.csv("net.dist.TFLA23.csv")
head(net.dist.TFLA)
net.dist23 <- rbind(net.dist.TFLA, net.dist.VLA)
head(net.dist23)
unique(net.dist23$Forest.type)
unique(net.dist23$Tr.Type)
unique(net.dist.VLA$Tr.Type)
unique(net.dist.VLA$Exemplar)
write.csv(net.dist23, "net.dist23.csv", row.names = FALSE)
net.dist23 <- read.csv("net.dist23.csv")

#reorder the groups order : I change the order of the factor data$names to order plot
net.dist23$Tr.Type <- factor(net.dist23$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))


####plot net dist each treatment combined forest type####
##Combined forest types
P.net.dist.all <- ggplot(net.dist23, aes(Treatment, Net.Dist)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size = 15)) +
  labs(y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add title to figure
annotate_figure(P.net.dist.all, top = text_grob("Net Distance Change in Both Forest Types", 
                                            color = "black", face = "bold", size = 20))

#Try violin plot
V.net.dist.all <- ggplot(net.dist23, aes(Treatment, Net.Dist)) + geom_violin(aes(fill=Treatment), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size = 15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3)

###Add title to figure
annotate_figure(V.net.dist.all, top = text_grob("Net Distance Change in Both Forest Types", 
                                                color = "black", face = "bold", size = 20))

####plot net dist each treatment type####
##Combined forest types
P.net.dist.all.type <- ggplot(net.dist23, aes(Tr.Type, Net.Dist)) + geom_boxplot(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size = 15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

unique(net.dist23$Tr.Type)

###Add title to figure
annotate_figure(P.net.dist.all.type, top = text_grob("", 
                                                color = "black", face = "bold", size = 20))

#Try violin plot
V.net.dist.all <- ggplot(net.dist23, aes(Tr.Type, Net.Dist)) + geom_violin(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size = 15))+labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3)

###Add title to figure
annotate_figure(V.net.dist.all, top = text_grob("", 
                                                color = "black", face = "bold", size = 20))


###Model GLMM: number of recruits####
####Hypotheses, Predictions####
#H1: MSF spp provide better safety information to the bird community relative to other forest birds
  #P1: There will be a higher response to MSF vocalizations than to non-flock vocalizations
#H2: Bird species vocalizations function as safety cues for forest birds regardless of sociality status
  #P2: There will be a similar response to MSF and non-flock vocalizations
#H3: Habitat type will influence the importance of safety cues to the larger bird community
  #P3: There will be a higher response to MSF safety cues in tierra firme compared to varzea

####read in safety23 here####
Safety23 <- read.csv("Safety23.csv")
head(Safety23) 
unique(Safety23$Treatment)
#Fixed vars: forest type, treatment/treatment type
#Random vars: exemplar, point

####Model number of recruits with all treatments####
#Run glmm for total recruits
#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var) + (1|random.exp.var)), family = "", data = dataframe
##Can set optimizer more specifically with lower formula
  #control_params <- glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000, nAGQ = 10, calc.derivs=TRUE))
M1<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23,
          control = glmerControl(optimizer = "bobyqa"))
M2<-glmer(Total.Recs~Treatment+Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M3<-glmer(Total.Recs~Treatment + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M4<-glmer(Total.Recs~Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
MN1<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=Safety23)
summary(M3)

#Determine best model to explain data with model.sel(M1, M2, etc...)
selection<-model.sel(M1, M2, M3, M4, MN1)
selection #M3 is best model but only 0.66 difference from null model
anova(M1)
plot(M1)

##Should we remove solo treatments and just compare MSF, NF, CTRL?
#Do we need to run optimizer on all models or just the ones that require it?

####Model number of recruits with group treatments####
#Want to model group effects, so just solo treatments
group.trmts <- Safety23 %>% filter(!Treatment=="MSFSOLO" & !Treatment=="NFSOLO")

#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var)), family = "", data = dataframe
M5<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=group.trmts)
M6<-glmer(Total.Recs~Treatment+Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
M7<-glmer(Total.Recs~Treatment + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
M8<-glmer(Total.Recs~Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
MN2<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=group.trmts)
summary(M5)
selection<-model.sel(M5, M6, M7, M8, MN2)
selection #M7 is best model but 0.99 difference from null model

####Model number of recruits solo treatments with control####
#Want to also model individual effects, so just solo treatments
solo.trmts <- Safety23 %>% filter(!Tr.Type=="MSF" & !Tr.Type=="NF")

####Run glmm for total recruits for only solo treatments##
#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var)), family = "", data = dataframe
M9<-glmer(Total.Recs~Tr.Type+Forest.type + Tr.Type*Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=solo.trmts)
M10<-glmer(Total.Recs~Tr.Type+Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=solo.trmts)
M11<-glmer(Total.Recs~Tr.Type+(1|Point) + (1|Exemplar),family="poisson",data=solo.trmts) #failed to converge
MN3<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=solo.trmts)

#####Running into convergence problems, need to add optimizer####

#using allfit() with glmer()
#install.packages("optimx")
#library(optimx)
#install.packages("minqa")
#library(minqa)
#install.packages("dfoptim")
#library(dfoptim)
#This will tell you which optimizer to use...

# Iterate through a set of optimizers, report convergence results
#check first model of solo only with convergence problem
diff_optims <- allFit(M1, maxfun = 1e5)

#Set up to print which optimizers will work -- if null printed, good to use
diff_optims_OK <- diff_optims[sapply(diff_optims, is, "merMod")]
lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)

#Not sure what this part does...need to check website
convergence_results <- lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)
if(sum(working_indices) == 0){
  print("No algorithms from allFit converged. You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- diff_optims[working_indices][[1]]
}
first_fit

#####Run glmm for total recruits for solo trtmts with optimizer####
#glmer(y ~ arm + (1 + arm | clustid),
    # data = dat, control = lmerControl(optimizer = "bobyqa"))
M9<-glmer(Total.Recs~Tr.Type+Forest.type + Tr.Type*Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
M10<-glmer(Total.Recs~Tr.Type+Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
M11<-glmer(Total.Recs~Tr.Type+(1|Point) + (1|Exemplar),
          family="poisson",data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
MN3<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=solo.trmts)

summary(M9)
selection<-model.sel(M9, M10, M11, MN3)
selection #Null model best here by 5.39 followed by M11

####Check for overdispersion in glmm####
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#install packages
install.packages(("glmmTMB"))
install.packages("TMB")
library(glmmTMB)
library(TMB)

##Use previous function to check for overdispersion in model
#output: chisq--tests whether variance of model resids is greater than expected
#based on assumed dist of response var
#ratio-- chisq/resid degrees of freedom, want to be close to 1
#rdf--resid degrees freedom
#p-val--H0: no overdispersion, HA: overdispersion
overdisp_fun(M11) 
#Overdispersed: M1,2,3,5,6,7,8
#Should we add optimizers to all to help with overdispersion?

####use DHARMa package to cheeck residuals after running model#####
#install.packages("DHARMa")
library(DHARMa)

#Create DHARMa residuals using simulateResiduals(model)
residuals <- simulateResiduals(M2)

plot(residuals)


####Predict values and plot for number of recruits####
##Run model
M1<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M9<-glmer(Total.Recs~Tr.Type+Forest.type + Tr.Type*Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))


##Predict model values
#pred <- predict(model, type="")
predicted_vals <- predict(M9, type="response")

##Create data frame for plotting by combining observed and predicted values
#because above predict function provides values in the same order as original dataframe
#can just add predictions by either adding predictions to original df or by creating
#new df with just the variables I want for plotting
predicted_data <- data.frame(Trial= solo.trmts$Trial, Observed=solo.trmts$Total.Recs, Predicted=predicted_vals, 
                             Treatment=solo.trmts$Tr.Type, Forest.type=solo.trmts$Forest.type)

#####Create boxplot with predicted values and points of observed values####
ggplot(predicted_data, aes(Treatment, Predicted)) + #adds treatment to x and predicted values for y
  geom_boxplot() + #creates boxplot for predicted values
  theme_minimal() +
  facet_wrap(~Forest.type, scales = "free_y") + #adds two panes to show forest types separately
  geom_jitter(aes(solo.trmts$Tr.Type, solo.trmts$Total.Recs, colour=solo.trmts$Tr.Type), 
              width = 0.2) + #adds points for observed values and shrinks width of points
  labs(x="Treatment", y="Recruits", color="Treatment") #labels for x, y, legend

#####Create boxplot of predicted values and layer boxplot of observed values####
pred.plot <- ggplot(predicted_data, aes(Treatment, Predicted)) +
  geom_boxplot()+ #geom_jitter()+
  labs(x="Treatment", y="Values") +theme_bw() #Need to add label in here somehow to show pred vs obs

pred.plot #predicted plot from model
#Add observed boxplot layer
pred.plot + geom_boxplot(data = predicted_data, 
                         aes(Treatment, Observed, fill=Treatment), width = 0.2, alpha = 0.5) +
  facet_wrap(~Forest.type, scales = "free_y") #+ 
 # geom_jitter(data=Safety23, aes(Treatment, Total.Recs, colour=Treatment))

#####Another way to plot this using points and lines####
ggplot(predicted_data, aes(x = Treatment)) +
  geom_line(aes(y = Predicted), color = "black", linetype = "solid", linewidth = 2) +
  geom_point(aes(y = Observed), color = "magenta", size = 1) +
  labs(title = "Predicted vs. Actual Values",
       x = "Treatment",
       y = "Number of Recruits") +
  theme_minimal()

#####Plot model using ggeffects####
library(ggeffects)
ggpred <- ggpredict(M9, terms = c("Tr.Type", "Forest.type"))
plot(ggpred)

#Can turn ggpredict object into dataframe and use for creating a plot with 
#observed and predicted? Needs work here...
ggpred <- as.data.frame(ggpred)
ggplot(ggpred, aes(x, predicted)) +
  geom_point(aes(color=group))


###Beta regression for proportion of recruits#### 
#beta regression (with mixed effects)
#Need package to run mixed effects beta reg
#library("glmmTMB") 
#Use same dataframes created in previous section, solo.trmts, group.trmts, and all safety23
head(solo.trmts)

####First test assumptions####
#Can use DHARMa package to cheeck residuals after running model
#library(DHARMa)

#1: response follows beta distribution, not really a way to do for the categorical predictors
# Create a density plot for each category --doesn't show distribution but to explore
#distribution of response variable within different categories
ggplot(solo.trmts, aes(x = Total.prop.rec, fill = Tr.Type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Response by Category")

hist(Safety23$Total.prop.rec)

#2: check heteroskedasticity -- unequal variability of the response var across diff
#levels of predictors

#3: assess for multicollinearity among predictor vars, high correlations no good

####Model specifications####
#Fixed effects: treatment/tr.type, forest.type
#Random effects: exemplar, point
Safety23 <- read.csv("Safety23.csv", header = TRUE, stringsAsFactors = TRUE)

####Fit model beta reg for prop recruits####
#model <- glmmTMB(response_variable ~ predictor1 + predictor2 + (1 | random_effect1) + (1 | random_effect2), 
#family = beta_family(link = "logit"), data = your_data)
#needed to remove and update package remove.packages("glmmTMB")
#install.packages("glmmTMB")
#tried using just this package, didn't help the problem bc matrix was not compatible install.packages("TMB")
library(glmmTMB)
library(TMB)
# needed to update matrix package to be able to run glmmtmb install.packages("Matrix")
library(Matrix)
# needed to install in terminal -- install.packages("gfortran")
sapply(Safety23, class)
any(is.na(Safety23$Total.prop.rec))

##Issue of having 0s and 1s in response variable, need to transform
#Will try to add 0.000000000001 to zeros, and there are no 1s
#Make new column to retain original props
unique(Safety23$Total.prop.rec)
Safety23$Tr.prop <- Safety23$Total.prop.rec
#Add small amount to zeros
Safety23$Tr.prop[which(Safety23$Total.prop.rec==0)] <- 1e-14
#Want to model group effects, so just solo treatments
group.trmts <- Safety23 %>% filter(!Treatment=="MSFSOLO" & !Treatment=="NFSOLO")
#Want to also model individual effects, so just solo treatments
solo.trmts <- Safety23 %>% filter(!Tr.Type=="MSF" & !Tr.Type=="NF")


##Run models

####Run betareg for prop of all treatments####
B1 <- glmmTMB(Tr.prop~Treatment+Forest.type+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=Safety23)
B2 <- glmmTMB(Tr.prop~Treatment+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=Safety23)
B3 <- glmmTMB(Tr.prop~Forest.type+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=Safety23)
BN1<-glmmTMB(Tr.prop~1, family=beta_family(link = "logit"), data=Safety23)
selection<-model.sel(B1, B2, B3, BN1)
selection

####Run betareg for prop group treatments only####
B4 <- glmmTMB(Tr.prop~Treatment+Forest.type+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=group.trmts)
B5 <- glmmTMB(Tr.prop~Treatment+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=group.trmts)
B6 <- glmmTMB(Tr.prop~Forest.type+(1|Exemplar) + (1|Point),
              family=beta_family(link = "logit"), data=group.trmts)
BN2<-glmmTMB(Tr.prop~1, family=beta_family(link = "logit"), data=group.trmts)
selection<-model.sel(B4, B5, B6, BN2)
selection

####Run betareg for prop solo treatments only####
B7 <- glmmTMB(Tr.prop~Tr.Type+Forest.type+(1|Exemplar) + (1|Point),
            family=beta_family(link = "logit"), data=solo.trmts)
B8 <- glmmTMB(Tr.prop~Tr.Type+(1|Exemplar) + (1|Point),
            family=beta_family(link = "logit"), data=solo.trmts)
B9 <- glmmTMB(Tr.prop~Tr.Type+(1|Exemplar) + (1|Point),
            family=beta_family(link = "logit"), data=solo.trmts)
BN3 <- glmmTMB(Tr.prop~1, family=beta_family(link = "logit"), data=solo.trmts)
selection<-model.sel(B7, B8, B9, BN3)
selection
summary(B9)


####beta reg Model assessment####
#assess model fit by examining resids and goodness of fit measures
####Interpretation####
#interpret fixed effects to understand relationship between predictors and response vars
#interpret random effects to understand group-specific variations
####Post-estimation tests####
#after fitting model, can perform hypothesis tests on coefficients
#check significance, and assess overall model fit
####Diagnostic plots####
#examine diagnostic plots sa resid plots, to check model assumptions
#identify any patterns or issues in the data that may require further investigation

####Predict model values and plot####
#pred <- predict(model, type="")
predicted_vals <- predict(B1, type="response")

##Create data frame for plotting by combining observed and predicted values
#because above predict function provides values in the same order as original dataframe
#can just add predictions by either adding predictions to original df or by creating
#new df with just the variables I want for plotting
predicted_data <- data.frame(Trial= Safety23$Trial, Observed=Safety23$Tr.prop, Predicted=predicted_vals, 
                             Treatment=Safety23$Treatment, Forest.type=Safety23$Forest.type)

#####Create boxplot with predicted values and points of observed values####
ggplot(predicted_data, aes(Treatment, Predicted)) + #adds treatment to x and predicted values for y
  geom_boxplot() + #creates boxplot for predicted values
  theme_minimal() +
  facet_wrap(~Forest.type, scales = "free_y") + #adds two panes to show forest types separately
  geom_jitter(aes(Safety23$Treatment, Safety23$Tr.prop, colour=Safety23$Treatment), 
              width = 0.2) + #adds points for observed values and shrinks width of points
  labs(x="Treatment", y="Recruits", color="Treatment") #labels for x, y, legend




###LMEM: net distance####
##lmem or lm
net.dist23 <- read.csv("net.dist23.csv", stringsAsFactors = TRUE)
head(net.dist23)

##split net.dist into group trmts and solo.trmts
net.solo.trmts <- net.dist23 %>% filter(!Tr.Type=="MSF" & !Tr.Type=="NF")
net.group.trmts <- net.dist23 %>% filter(!Treatment=="MSFSOLO" & !Treatment=="NFSOLO")

####Plot net.dist####
#####plot nest dist by treatment group####
ggplot(net.dist23, aes(Treatment, Net.Dist)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +labs(x="Treatment", y="Net Dist Moved (m)") + theme_bw() +
  theme(axis.title = element_text(size = 15))+ stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

#####plots net dist by treatment type####

ggplot(net.dist23, aes(Tr.Type, Net.Dist)) + geom_boxplot(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) + 
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

#m.dist1 <- lmer(Net.Dist~Treatment+Forest.type+Forest.Type*Treatment+(1|Exemplar),data = net.dist23)
L1 <- lmer(Net.Dist~Treatment+Forest.type+Forest.type*Treatment+(1|Point)+(1|Exemplar), data = net.dist23)
#error of boundary (singular) fit, check for multicollinearity, however issue is likely with random effects ***Problem goes away if you remove exemplar random effect??
chi_squared <- chisq.test(net.dist23$Treatment, net.dist23$Forest.type)
print(chi_squared) #p=0.84, this suggests no strong relationship between the two effects
#unlikely multicollinearity is an issue
#Check for collinearity using vif(model)
library(car)
vif(L1)
#interaction term high meaning possible collinearity between treatment and forest type vars and their interaction
#Remove interaction term??

L2 <- lmer(Net.Dist~Treatment+Forest.type+(1|Point)+(1|Exemplar),data = net.dist23) #check assumptions, log transform?
m.distnull <- lmer(Net.Dist~1+(1|Exemplar), data = net.dist23)
help("isSingular") #not sure about that
summary(m.distnull)

anova(m.dist1)


###Check assumptions net distance model####
#Check assumptions using resid()
#Will return residuals around the regression line/linear model
#Plot histogram
#hist(resid(m1), probability = TRUE)
hist(resid(L1), probability = TRUE)
resids <- resid(L1)
mean(resids)

curve(dnorm(x, mean=mean(resids), sd=sd(resids) ),from=-50,to=50,add=TRUE)
#Normalish but have a long tail to the left

qqnorm(resids)
qqline(resids)
#not really normal here

#check goodness of fit test to a normal distribution 
#H0: normal
#HA: not normal
shapiro.test(resids)
#p-val=2.2e-16, not normal***

#Are the variances equal enough at all levels of X? 
#Assess via scatterplot, using temp as X, and resids as Y
#categories must be factors
plot(net.dist23$Treatment, resids, xlab = "Treatment")
plot(net.dist23$Forest.type, resids, xlab = "Forest Type")
#variance seems equal enough

library(car)
#Variance among groups using Formal test 
#H0: variances equal, HA: variances not equal
sapply(net.dist23, class)
leveneTest(resids, net.dist23$Forest.type)  #p=0.23
leveneTest(resids, net.dist23$Treatment) #p=0.28
#Variance is equal for both fixed variables

###use DHARMa package to cheeck residuals after running model#####
#Create DHARMa residuals using simulateResiduals(model)
residuals <- simulateResiduals(L2)

plot(residuals)

####Transform data####
#Make new transformed variable and add to dataframe #***This produces NaNs and 
# -inf, how to deal with this??
net.dist23$l.dist <- log(net.dist23$Net.Dist)
net.dist23$l.dist[is.na(net.dist23$l.dist)] <- 0
#Try cube root transformation
net.dist23$cube_nd <- net.dist23$Net.Dist^(1/3)
#Still gives NaNs so try...make function to deal with negatives
CubeRoot<-function(x){
  sign(x)*abs(x)^(1/3)
}
net.dist23$cube_nd <- CubeRoot(net.dist23$Net.Dist)

#Reassess assumptions for transformed values, 

#m2 <- lmer(logResponse~Fixed+(1|Random), data = dataframe)
m.dist2 <- lmer(cube_nd~Treatment+Forest.type+(1|Point),data = net.dist23)

#View summary of model
summary(m.dist2)

#Check assumptions using resid()
#Will return residuals around the regression line/linear model
#Plot histogram
#hist(resid(m1), probability = TRUE)
hist(resid(m.dist2), probability = TRUE)
resids2 <- resid(m.dist2)
mean(resids2)

curve(dnorm(x, mean=mean(resids2), sd=sd(resids2) ),from=-5,to=5,add=TRUE)
#Pretty almost dang normal!

qqnorm(resids2)
qqline(resids2)
#Super normalish

#check goodness of fit test to a normal distribution 
shapiro.test(resids2)
#p-val=2.2e-16 still not normal

#Are the variances equal enough at all levels of X? 
#Assess via scatterplot, using temp as X, and resids as Y
plot(net.dist23$Treatment, resids2, xlab = "Treatment")
#from scatter plot -- we can see variances are fairly equal

#Variance among groups using Formal test 
leveneTest(resids2, net.dist23$Treatment) #p-value=0.054
#After running a formal test, we see the variances are equal, 

#Interpret fixed effects of the model
summary(m.dist2)

###Do I need to do a different model or different transformation??












###Plot model predictions####
####Needs work! Practice practice####
?ggpredict
prac.plot <- ggplot(Safety23, aes(x=Treatment, y=Total.Recs)) + 
geom_boxplot(show.legend = FALSE, alpha=0.2, color="grey30") + geom_jitter(aes(x=Treatment, y=Total.Recs, colour=Treatment))+
 labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15))+ scale_color_manual(values = plasma(5)[1:5])
prac.plot

M1<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
mypred <- ggpredict(model, terms = c(""))

##MAke dataframe with every possible unique combo of each factor effect, and mean value of any cont effect
predicted <- expand.grid(Point = factor(unique(M1$data$Point)),
                         Treatment = factor(unique(M1$data$Treatment)),
                         Forest.type = factor(unique(M1$data$Forest.type)))
M1

mydf <- ggpredict(M1,terms = c("Forest.type"))

ggplot(mydf, aes(x, predicted)) +
  geom_boxplot(data=Safety23, aes(Forest.type, Total.Recs, color = Treatment), alpha = 0.5) +
  labs(x = "Forest Type", y = "Predicted Response")

ggplot(mydf, aes(x, y = predicted)) +
  geom_boxplot(data=Safety23, aes(fill = factor(Exemplar)), alpha = .2)

###Try this way

predicted <- predict(model.indglobal1, newdata = df, type = "log", interval = "confidence", se = TRUE)
df <- cbind(df, data.frame(predicted))

# create and summarise model
M1 <- glmer(Total.Recs~Treatment+Forest.type+(1|Exemplar),family="poisson", data=Safety23)
summary(M1) 

# add 'fit', 'lwr', and 'upr' columns to dataframe (generated by predict)
M1.predict <- cbind(Safety23, predict(M1, interval = 'confidence'))
head(M1.predict)
# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(M1.predict, aes(Treatment,Total.Recs))+ geom_boxplot()
p <- p + geom_line(aes(speed, fit))
p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3)
p

