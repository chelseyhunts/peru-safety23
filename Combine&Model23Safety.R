###combination of tf and v dfs & Model selection and 

###Required packages####
#library MuMIn --install for model selection
#install.packages("MuMIn")
library(MuMIn)
#glmm package
library(lme4)
#plotting model predictions
library(ggplot2)
#install.packages("ggeffects")
library(ggeffects)
library(tidyverse)
#install.packages("optimx")
library(optimx)
#install.packages("minqa")
library(minqa)
#install.packages("dfoptim")
library(dfoptim)

###Response 1 & 2 Combine####
head(TFLA.Rcomb)
TFLA.Rcomb <- read.csv("TFLA.Rcomb.csv")
head(VLA.Rcomb)
VLA.Rcomb <- read.csv("VLA.Rcomb.csv")
Safety23 <- rbind(TFLA.Rcomb, VLA.Rcomb)
head(Safety23)
unique(Safety23$Forest.type)
write.csv(Safety23, "Safety23.csv", row.names = FALSE)
Safety23 <- read.csv("Safety23.csv")


#reorder the groups order : I change the order of the factor data$names to order plot
Safety23$Tr.Type <- factor(Safety23$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

###Plot combined response 1&2 by treatment####
library(viridis)
num.plot.all <- ggplot(Safety23, aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15))

###Plot VLA proportion response 1&2 by treatment type####
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

###Plot combined response 1&2 by treatment type####
num.plot.all.type <- ggplot(Safety23, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15)) 

###Plot VLA proportion response 1&2 by treatment type####
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

p.ALL.type <- ggarrange(prop.plot.all.type, num.plot.all.type,
                        labels = c("C", "D"),
                        ncol=1, nrow=2, 
                        legend = "none")


###Net dist combine
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


###plot net dist each treatment combined forest type####
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

###plot net dist each treatment type####
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


###Model Selection number/prop recruits####
##Hypotheses, Predictions####
#H1: MSF spp provide better safety information to the bird community relative to other forest birds
  #P1: There will be a higher response to MSF vocalizations than to non-flock vocalizations
#H2: Bird species vocalizations function as safety cues for forest birds regardless of sociality status
  #P2: There will be a similar response to MSF and non-flock vocalizations
#H3: Habitat type will influence the importance of safety cues to the larger bird community
  #P3: There will be a higher response to MSF safety cues in tierra firme compared to varzea

###read in safety23 here####
Safety23 <- read.csv("Safety23.csv")
head(Safety23) 
unique(Safety23$Treatment)
#Fixed vars: forest type, treatment/treatment type
#Random vars: exemplar, point

###Model number of recruits with all treatments####
#Run glmm for total recruits
#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var)), family = "", data = dataframe
M1<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M2<-glmer(Total.Recs~Treatment+Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M3<-glmer(Total.Recs~Treatment + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
M4<-glmer(Total.Recs~Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=Safety23)
MN1<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=Safety23)
summary(M3)
selection<-model.sel(M1, M2, M3, M4, MN1)
selection
anova(M1)
plot(M1)

##Should we remove solo treatments and just compare MSF, NF, CTRL?

###Model number of recruits with group treatments####
#Want to model group effects, so just solo treatments
group.trmts <- Safety23 %>% filter(!Treatment=="MSFSOLO" & !Treatment=="NFSOLO")

#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var)), family = "", data = dataframe
M5<-glmer(Total.Recs~Treatment+Forest.type + Treatment*Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=group.trmts)
M6<-glmer(Total.Recs~Treatment+Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
M7<-glmer(Total.Recs~Treatment + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
M8<-glmer(Total.Recs~Forest.type + (1|Point) + (1|Exemplar), family="poisson", data=group.trmts)
MN2<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=group.trmts)
summary(M3)
selection<-model.sel(M5, M6, M7, M8, MN2)
selection

###Model solo treatments with control####
#Want to also model individual effects, so just solo treatments
solo.trmts <- Safety23 %>% filter(!Tr.Type=="MSF" & !Tr.Type=="NF")

####Run glmm for total recruits for only solo treatments####
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
diff_optims <- allFit(M9, maxfun = 1e5)

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

####Run glmm for total recruits for solo trtmts with optimizer####
#glmer(y ~ arm + (1 + arm | clustid),
    # data = dat, control = lmerControl(optimizer = "bobyqa"))
M9<-glmer(Total.Recs~Tr.Type+Forest.type + Tr.Type*Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
M10<-glmer(Total.Recs~Tr.Type+Forest.type + (1|Point) + (1|Exemplar), 
          family="poisson", data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
M11<-glmer(Total.Recs~Tr.Type+(1|Point) + (1|Exemplar),
          family="poisson",data=solo.trmts, control = glmerControl(optimizer = "bobyqa"))
MN3<-glmer(Total.Recs~1+(1|Point) + (1|Exemplar),family="poisson",data=solo.trmts)

summary(M11)
selection<-model.sel(M9, M10, M11, MN3)
selection

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
overdisp_fun(M9) 

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
  labs(x="Treatment", y="Values", color="Treatment") #labels for x, y, legend

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
  geom_line(aes(y = Predicted), color = "black", linetype = "solid", size = 2) +
  geom_point(aes(y = Observed), color = "magenta", size = 1) +
  labs(title = "Predicted vs. Actual Values",
       x = "Treatment",
       y = "Number of Recruits") +
  theme_minimal()

#####Plot model using ggeffects####
library(ggeffects)
ggpred <- ggpredict(M9, terms = c("Tr.Type", "Forest.type"))
plot(ggpred)


###Run glmm for proportion of recruits#### 
#beta regression (with mixed effects)
#glmer(response~fixed.exp.var+fixedexp.var+(1|random.exp.var)), family = "", data = dataframe
M1<-glmer(Total.prop.rec~Treatment+Forest.type+(1|Exemplar),family="binomial", data=Safety23)
M2<-glmer(Total.prop.rec~Treatment+(1|Exemplar),family="binomial",data=Safety23)
#model.indglobal<-glmer(Total.Recs~Time+(1|treatments),family="poisson",data=rec.trialall)
M.null<-glmer(Total.prop.~1+(1|Exemplar),family="poisson",data=Safety23)
selection<-model.sel(M1, M2, M.null)

###Model selection net distance####
##lmem or lm
net.dist23 <- read.csv("net.dist23.csv")
head(net.dist23)

#m.dist1 <- lmer(Net.Dist~Treatment+Forest.type+Forest.Type*Treatment+(1|Exemplar),data = net.dist23)
m.dist1 <- lmer(Net.Dist~Treatment+Forest.type+(1|Point),data = net.dist23)
m.dist1 <- lm(Net.Dist~Treatment+Forest.type+Forest.type*Treatment,data = net.dist23) #check assumptions, log transform?
m.distnull <- lmer(Net.Dist~1+(1|Exemplar), data = net.dist23)
help("isSingular") #not sure about that
summary(m.distnull)
summary(m.dist1)
anova(m.dist1)


###Check assumptions net distance model####
#Check assumptions using resid()
#Will return residuals around the regression line/linear model
#Plot histogram
#hist(resid(m1), probability = TRUE)
hist(resid(m.dist1), probability = TRUE)
resids <- resid(m.dist1)
mean(resids)

curve(dnorm(x, mean=mean(resids), sd=sd(resids) ),from=-50,to=50,add=TRUE)
#Normalish

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
plot(net.dist23$Treatment, resids, xlab = "Treatment")
#not sure how to run this one...changing treatment/forest type to factor seemed to fix this
plot(net.dist23$Forest.type, resids, xlab = "Forest Type")

library(car)
#Variance among groups using Formal test 
#not sure how to run this one...
sapply(net.dist23, class)
net.dist23$Forest.type <- as.factor(net.dist23$Forest.type)
leveneTest(resids, net.dist23$Forest.type) 
net.dist23$Treatment <- as.factor(net.dist23$Treatment)
leveneTest(resids, net.dist23$Treatment) 
#Variance is equal for both fixed variables

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

#Reassess assumptions for transformed MO2 values, 

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

