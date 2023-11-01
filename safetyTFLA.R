### Clear R's brain
rm(list = ls()) 

###Set working directory
setwd("~/Documents/CSULB Documents/Thesis Work/Data/")

###read in TFLA csv file and create object
safetyTFLA<-read.csv("22safetycueTFLA.csv", header=TRUE)

###Quick look over
head(safetyTFLA)

####Subset dataframe for variables of interest####
safetyTFLA <- subset(safetyTFLA, select = c("trial","treatment","stage","spp", "distance", "Time.Slot"))
sapply(safetyTFLA, class)

###Need to change distance to numeric value
safetyTFLA$distance <- as.numeric(as.character(safetyTFLA$distance))

###Response 1####
####Remove anything greater than 30m distance Response 1#### 
safetyTFLA30 <- safetyTFLA[!safetyTFLA$distance>30,]
head(safetyTFLA30)
length(safetyTFLA30$stage[which(safetyTFLA30$distance>30)])
safetyTFLA30[safetyTFLA30$distance>30,]

####Split data frame into two sets, based on time.slot Response 1####
safetyTFLA30.1 <- safetyTFLA30[safetyTFLA30$Time.Slot==1,]
head(safetyTFLA30.1)
safetyTFLA30.2 <- safetyTFLA30[safetyTFLA30$Time.Slot==2,]
head(safetyTFLA30.2)

###See summary of current data frame Response 1###
summary(safetyTFLA30.1)
summary(safetyTFLA30.2)

###subset data to include only trial, treatment, stage, spp, distance variables
#Rewrite over original safety data frame
safetyTFLA30.1 <- safetyTFLA30.1[,-6]
safetyTFLA30.2 <- safetyTFLA30.2[,-6]

#Check to see if it is how we want
sapply(safetyTFLA30.1, class)

###################TFLA Time Slot 1 Response 1#####################################################
###Prepare for for loops (Time slot 1)
#Create spp list of all spp observed throughout experiment to add to pre and post
spp.list<-unique(safetyTFLA30.1$spp)
#Create treatment list to add to each line
treatment.list<-sort(unique(safetyTFLA30.1$treatment))

#Create null vectors for each variable of interest
treatment<-NULL
counts<- NULL
species<-NULL
trials<-NULL

for(i in treatment.list){
  s1<-subset(safetyTFLA30.1, safetyTFLA30.1$treatment == i) #i=treatment
  trial.list<-sort(unique(s1$trial))
  for(j in trial.list){
    s2<-subset(s1, s1$trial == j) #j= add trial number to each row
    #counts<-NULL
    for(k in spp.list){
      s3<-subset(s2, s2$spp == k) #k=species
      if(dim(s3)[1] == 0){cts<-c(0,0)} else{cts<-c(length(which(s3$stage=="PRE")), #if row 1 of s3=0, then make vector of pre/post=0,0, if s3 is not 0, then cts vector = length of pre, length of post
                                                   length(which(s3$stage=="POST")))}
      #this is where you can add anything to summarize distances moved, etc, total before.after, diversity,
      #copy if statement and change inner bits, dist traveled change c(0, NA)
      counts=rbind(counts, cts) #combine rows for counts of pre and post
      treatment<-c(treatment, i) #combines treatment
      trials<-c(trials, j) #adds trial
    }
    species<-c(species, spp.list) #
  }
}

###Combine all vectors to make data frame of each variable of interest
pre.v.postTFLA30.1<-as.data.frame(cbind(trials, treatment, species, counts))

###Rename columns
names(pre.v.postTFLA30.1)<-c("trial", "treatments", "species", "pre.count", "post.count")
head(pre.v.postTFLA30.1)

###Create vectors of pre and post counts to the find response totals
prevec <- as.numeric(pre.v.postTFLA30.1$pre.count)
postvec <- as.numeric(pre.v.postTFLA30.1$post.count)
pre.v.postTFLA30.1$recruit <- ifelse(pre.v.postTFLA30.1$pre.count >= pre.v.postTFLA30.1$post.count, 0, postvec-prevec)
head(pre.v.postTFLA30.1)
sapply(pre.v.postTFLA30.1, class)
pre.v.postTFLA30.1$trial <- as.character(as.integer(pre.v.postTFLA30.1$trial))

###Create new csv file
write.csv(pre.v.postTFLA30.1, "pre.v.postTFLA30.1.csv", row.names = FALSE)

####read in pre.v.postTFLA30.1####
pre.v.postTFLA30.1 <- read.csv("pre.v.postTFLA30.1.csv", header=T)
head(pre.v.postTFLA30.1)

###Add treatment group to all trials####
pre.v.postTFLA30.1$trtmt.grp <- substr(pre.v.postTFLA30.1$treatments, 1, 1)
tail(pre.v.postTFLA30.1)

# create vector
trmt.grp <- pre.v.postTFLA30.1$trtmt.grp

# replacement vector
new <- NULL

# start for loop, using 1:length(test) lets us index the values for replacement; 
# take the 1st spot and put in "fff", second spot = "fff" and so on. 
for (i in 1:length(trmt.grp)) {
  
  # if the first value is "FFF" then take the ith value and pop in "fff"
  # OR IF FALSE (else) MOVE ON,
  if (trmt.grp[i] == "F") {new[i] <- "Flock"} else 
    
    # if the ith value is "CL2" then take ith of "new" object and pop in "cl2"
    # OR, IF STILL FALSE, MOVE ON (else)
    if (trmt.grp[i] == "C") {new[i] <- "Control"} else 
      
      # finally, if ith value of test is "GG1", then take ith value of new and 
      # put in "gg1". 
      if (trmt.grp[i] == "S") {new[i] <- "NonFlock"} 
  
  # you don't need a final "else" here because all options are consumed by the above if/else conditions. if there are options leftover, let's say "test" ended with "CCC" you'd get an error with this code, but that's okay because you'd want to be alerted to the fact that there's a value you haven't told R what to do with (in this case "CCC").
  
}

# output should be all lowercase now,
# then you can just "$" it into the database! 

pre.v.postTFLA30.1$trtmt.grp <- new
head(pre.v.postTFLA30.1)

###Remove conspecifics from Solo spp
unique(pre.v.postTFLA30.1$trtmt.grp)
head(pre.v.postTFLA30.1)

install.packages("dplyr")
library(dplyr)

#NF <- pre.v.postTFLA30.1 %>% 
  #filter(trtmt.grp=="NonFlock" )
minuscon <- pre.v.postTFLA30.1 %>% 
  filter(trtmt.grp=="NonFlock" ) %>% filter(!species=="HEGR" & !species== "LECO"& !species== "MOMO" & !species== "PLCO" & !species== "LAHY")

head(minuscon)
#check spp were actually removed
sum(minuscon$species=="MOMO")
#check total obsservations in nonflock vs how many in minuscon 
sum(pre.v.postTFLA30.1$trtmt.grp=="NonFlock")
#-- looks like 40 were removed

#Split data frame into all but nonflock
minusNF <- subset(pre.v.postTFLA30.1, !trtmt.grp=="NonFlock") #1820 obs

#Check this removed nonflock treatment
sum(minusNF$trtmt.grp=="NonFlock")

#ReCombine the nonflock minus conspeficis and all trtmts but nonflock
pre.v.postTFLA30.1 <- rbind(minuscon, minusNF)
head(pre.v.postTFLA30.1)

####Create new csv file over original pre.v.post.TFLA30.1####
write.csv(pre.v.postTFLA30.1, "pre.v.postTFLA30.1.csv", row.names = FALSE)
pre.v.postTFLA30.1 <- read.csv("pre.v.postTFLA30.1.csv")
head(pre.v.postTFLA30.1)
unique(pre.v.postTFLA30.1$trtmt.grp)


####Sum by trial using aggregate function####
rec.trialTFLA30.1 <- aggregate(recruit~ trial+treatments+trtmt.grp, pre.v.postTFLA30.1, sum)
head(rec.trialTFLA30.1)

###order new data set to add 
rec.trialTFLA30.1 <- rec.trialTFLA30.1[order(rec.trialTFLA30.1$trial),]

####Set proportions Response 1 Time 1####
###Set up for loop to count number of species observed per trial using code above
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(pre.v.postTFLA30.1$trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(pre.v.postTFLA30.1, trial==i) 
  species.no<-length(unique(pre.v.postTFLA30.1$species[which(pre.v.postTFLA30.1$trial==i & pre.v.postTFLA30.1$pre.count>=1 | pre.v.postTFLA30.1$post.count>=1 & pre.v.postTFLA30.1$trial==i)]))
  spp.per.trial<-c(spp.per.trial, species.no) 
}
#Create date frame of spp per trial #
spp.per.trial<-data.frame(unique.trials, spp.per.trial)
#Change names
names(spp.per.trial) <- c("trial", "spp.per.trial")

###Count number of spp responding in pre.v.post df using for loop
head(pre.v.postTFLA30.1)
spp.rec.per.trial <- NULL

unique.trials<-unique(pre.v.postTFLA30.1$trial)

for(i in unique.trials){
  trial<-subset(pre.v.postTFLA30.1, trial==i) #
  species.no<-length(which(trial$recruit>=1))
  spp.rec.per.trial<-c(spp.rec.per.trial, species.no)
}
#Create data frame of spp recruited per trial
spp.rec.per.trial<-data.frame(unique.trials, spp.rec.per.trial)
#Change names
names(spp.rec.per.trial) <- c("trial", "spp.rec.per.trial")

#Create data frame of just trial and treatment
head(rec.trialTFLA30.1)
trial.trtmt <- subset(rec.trialTFLA30.1, select = c("trial","treatments", "trtmt.grp"))
head(trial.trtmt)
###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial
install.packages("tidyverse")
library(tidyverse)
str(spp.rec.per.trial)

####Merge data to get prop df####
prop.trialTFLA30.1 <- merge(trial.trtmt, c(spp.per.trial, spp.rec.per.trial), by="trial")
###Combine recruits per trial and number of species per trial to get proportion 
prop.trialTFLA30.1$prop.rec <- prop.trialTFLA30.1$spp.rec.per.trial/prop.trialTFLA30.1$spp.per.trial
###Check it out...
head(prop.trialTFLA30.1)
###Replace NaNs with 0s
prop.trialTFLA30.1$prop.rec[is.nan(prop.trialTFLA30.1$prop.rec)]<-0

####Merge prop rec and rec Response 1 Time 1####
###Now need to add number of recruits per trial to this
#Using merge
TFLA30.1 <- merge(rec.trialTFLA30.1, prop.trialTFLA30.1, by="trial")
head(TFLA30.1)

#Subset variables of interest aka remove duplicate columns
TFLA30.1 <- subset(TFLA30.1, select = c("trial", "treatments.x", "trtmt.grp.x","recruit", "spp.per.trial", "spp.rec.per.trial", "prop.rec"))
#Rename columns
names(TFLA30.1)[1:3] <- c("trial", "treatments", "trtmt.grp")
head(TFLA30.1)
###Add time to each trial
TFLA30.1$Time <- rep("Time 6:30-8:30")

####Write final Response 1 time 1 new csv/Read in####
write.csv(TFLA30.1, "TFLA30.1.csv", row.names = FALSE)
TFLA30.1 <- read.csv("TFLA30.1.csv")
head(TFLA30.1)

###Plot using ggplot

library(ggplot2)
library(wesanderson)
library(viridis)

prop.plotTFLA30.1 <- ggplot(TFLA30.1, aes(trtmt.grp, prop.rec)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme 630-830", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") +scale_fill_manual(values = wes_palette("Moonrise3", n = 3))
colors()

num.plotTFLA30.1 <- ggplot(TFLA30.1, aes(trtmt.grp, recruit)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme 630-830", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))


######################TFLA R1 Time 2###########################################################

###Prepare for for loops (Time slot 2)
#Create spp list of all spp observed throughout experiment to add to pre and post
spp.list<-unique(safetyTFLA30.2$spp)
#Create treatment list to add to each line
treatment.list<-sort(unique(safetyTFLA30.2$treatment))

#Create null vectors for each variable of interest
treatment<-NULL
counts<- NULL
species<-NULL
trials<-NULL

for(i in treatment.list){
  s1<-subset(safetyTFLA30.2, safetyTFLA30.2$treatment == i) #i=treatment
  trial.list<-sort(unique(s1$trial))
  for(j in trial.list){
    s2<-subset(s1, s1$trial == j) #j= add trial number to each row
    #counts<-NULL
    for(k in spp.list){
      s3<-subset(s2, s2$spp == k) #k=species
      if(dim(s3)[1] == 0){cts<-c(0,0)} else{cts<-c(length(which(s3$stage=="PRE")), #if row 1 of s3=0, then make vector of pre/post=0,0, if s3 is not 0, then cts vector = length of pre, length of post
                                                   length(which(s3$stage=="POST")))}
      #this is where you can add anything to summarize distances moved, etc, total before.after, diversity,
      #copy if statement and change inner bits, dist traveled change c(0, NA)
      counts=rbind(counts, cts) #combine rows for counts of pre and post
      treatment<-c(treatment, i) #combines treatment
      trials<-c(trials, j) #adds trial
    }
    species<-c(species, spp.list) #
  }
}

###Combine all vectors to make data frame of each variable of interest
pre.v.postTFLA30.2<-as.data.frame(cbind(trials, treatment, species, counts))

###Rename columns
names(pre.v.postTFLA30.2)<-c("trial", "treatments", "species", "pre.count", "post.count")
head(pre.v.postTFLA30.2)

###Add treatment group to all trials
pre.v.postTFLA30.2$trtmt.grp <- substr(pre.v.postTFLA30.2$treatments, 1, 1)
tail(pre.v.postTFLA30.2)

# create vector
trmt.grp <- pre.v.postTFLA30.2$trtmt.grp

# replacement vector
new <- NULL

# start for loop, using 1:length(test) lets us index the values for replacement; 
# take the 1st spot and put in "fff", second spot = "fff" and so on. 
for (i in 1:length(trmt.grp)) {
  
  # if the first value is "FFF" then take the ith value and pop in "fff"
  # OR IF FALSE (else) MOVE ON,
  if (trmt.grp[i] == "F") {new[i] <- "Flock"} else 
    
    # if the ith value is "CL2" then take ith of "new" object and pop in "cl2"
    # OR, IF STILL FALSE, MOVE ON (else)
    if (trmt.grp[i] == "C") {new[i] <- "Control"} else 
      
      # finally, if ith value of test is "GG1", then take ith value of new and 
      # put in "gg1". 
      if (trmt.grp[i] == "S") {new[i] <- "NonFlock"} 
  
  # you don't need a final "else" here because all options are consumed by the above if/else conditions. if there are options leftover, let's say "test" ended with "CCC" you'd get an error with this code, but that's okay because you'd want to be alerted to the fact that there's a value you haven't told R what to do with (in this case "CCC").
  
}

# output should be all lowercase now,
# then you can just "$" it into the database! 

pre.v.postTFLA30.2$trtmt.grp <- new
head(pre.v.postTFLA30.2)

###Remove conspecifics from Solo spp
unique(pre.v.postTFLA30.2$trtmt.grp)
head(pre.v.postTFLA30.2)

library(dplyr)

minuscon <- pre.v.postTFLA30.2 %>% 
  filter(trtmt.grp=="NonFlock" ) %>% filter(!species=="HEGR" & !species== "LECO"& !species== "MOMO" & !species== "PLCO" & !species== "LAHY")

head(minuscon)
sum(minuscon$species=="MOMO")

sum(pre.v.postTFLA30.1$trtmt.grp=="NonFlock")

minusNF <- subset(pre.v.postTFLA30.2, !trtmt.grp=="NonFlock")

sum(minusNF$trtmt.grp=="NonFlock")

pre.v.postTFLA30.2 <- rbind(minuscon, minusNF)
head(pre.v.postTFLA30.2)

####Create vectors of pre and post counts to the find response totals####
prevec <- as.numeric(pre.v.postTFLA30.2$pre.count)
postvec <- as.numeric(pre.v.postTFLA30.2$post.count)
pre.v.postTFLA30.2$recruit <- ifelse(pre.v.postTFLA30.2$pre.count >= pre.v.postTFLA30.2$post.count, 0, postvec-prevec)
head(pre.v.postTFLA30.2)
sapply(pre.v.postTFLA30.2, class)
pre.v.postTFLA30.2$trial <- as.integer(pre.v.postTFLA30.2$trial)
sapply(pre.v.postTFLA30.2, class)

####Create new csv R1 T2 pre.v.post.TFLA####
write.csv(pre.v.postTFLA30.2, "pre.v.postTFLA30.2.csv", row.names = FALSE)
pre.v.postTFLA30.2 <- read.csv("pre.v.postTFLA30.2.csv")
head(pre.v.postTFLA30.2)

###Sum by trial using aggregate function, then run through Alex code to add trtmt group
rec.trialTFLA30.2 <- aggregate(recruit~ trial+treatments+trtmt.grp, pre.v.postTFLA30.2, sum)
head(rec.trialTFLA30.2)

###order new dataset to add 
rec.trialTFLA30.2 <- rec.trialTFLA30.2[order(rec.trialTFLA30.2$trial),]

####Set up for proportions R1 T2####
###Set up for loop to count number of species observed per trial using code above
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(pre.v.postTFLA30.2$trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(pre.v.postTFLA30.2, trial==i) 
  species.no<-length(unique(pre.v.postTFLA30.2$species[which(pre.v.postTFLA30.2$trial==i & pre.v.postTFLA30.2$pre.count>=1 | pre.v.postTFLA30.2$post.count>=1 & pre.v.postTFLA30.2$trial==i)]))
  spp.per.trial<-c(spp.per.trial, species.no)
}

#Create date frame of spp per trial #
spp.per.trial<-data.frame(unique.trials, spp.per.trial)
#Change names
names(spp.per.trial) <- c("trial", "spp.per.trial")

###Count number of spp responding in pre.v.post df using for loop
head(pre.v.postTFLA30.2)
spp.rec.per.trial <- NULL

unique.trials<-unique(pre.v.postTFLA30.2$trial)

for(i in unique.trials){
  trial<-subset(pre.v.postTFLA30.2, trial==i) #
  species.no<-length(which(trial$recruit>=1))
  spp.rec.per.trial<-c(spp.rec.per.trial, species.no)
}

#Create data frame of spp recruited per trial
spp.rec.per.trial<-data.frame(unique.trials, spp.rec.per.trial)
#Change names
names(spp.rec.per.trial) <- c("trial", "spp.rec.per.trial")

#Create data frame of just trial and treatment
head(rec.trialTFLA30.2)
trial.trtmt <- subset(rec.trialTFLA30.2, select = c("trial","treatments", "trtmt.grp"))

###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial
prop.trialTFLA30.2 <- merge(trial.trtmt, c(spp.per.trial, spp.rec.per.trial), by="trial")

###Combine recruits per trial and number of species per trial to get proportion 
prop.trialTFLA30.2$prop.rec <- prop.trialTFLA30.2$spp.rec.per.trial/prop.trialTFLA30.2$spp.per.trial
###Check it out...
head(prop.trialTFLA30.2)
#So need to replace with 0s using following code
prop.trialTFLA30.2$prop.rec[is.nan(prop.trialTFLA30.2$prop.rec)]<-0

###Now need to add number of recruits per trial to this or keep as two different dfs
#Using aggregate?? or merge
TFLA30.2 <- merge(rec.trialTFLA30.2, prop.trialTFLA30.2, by="trial")
head(TFLA30.2)

#Subset variables of interest aka remove duplicate columns
TFLA30.2<- subset(TFLA30.2, select = c("trial", "treatments.x", "trtmt.grp.x","recruit", "spp.per.trial", "spp.rec.per.trial", "prop.rec"))
#Rename columns
names(TFLA30.2)[2:3] <- c("treatments", "trtmt.grp")
head(TFLA30.2)
###Add time to each trial
TFLA30.2$Time <- rep("Time 8:30+")

####Write final Response 1 time 2 new csv####
write.csv(TFLA30.2, "TFLA30.2.csv", row.names = FALSE)
TFLA30.2 <- read.csv("TFLA30.2.csv")
head(TFLA30.2)

###Write new csv
write.csv(rec.trialTFLA30.2, "rec.trialTFLA30.2.csv", row.names = FALSE)
rec.trialTFLA30.2 <- read.csv("rec.trialTFLA30.2.csv")
head(rec.trialTFLA30.2)
head(rec.trialTFLA30.1)

###Plot using ggplot
library(ggplot2)
prop.plotTFLA2 <- ggplot(TFLA30.2, aes(trtmt.grp, prop.rec)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme 830+", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group"))+ stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4")  + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))

num.plotTFLA2 <- ggplot(TFLA30.2, aes(trtmt.grp, recruit)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme 830+", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group"))+ stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4")  + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))


###Combine dfs TFLA30.1 and 2:
TFLA.R1 <- rbind(TFLA30.1, TFLA30.2)
head(TFLA.R1)

###Add Forest type column
TFLA.R1$forest.type <- rep("TF")
head(TFLA.R1)

###Rename responses as response 1 to prepare for combination with Response 2
names(TFLA.R1)[4:7] <- c("R1.recruit", "R1.spp.per.trial", "R1.spp.rec.per.trial", "R1.prop.rec")
head(TFLA.R1)

####Write csv for TFLA R1 both times####
write.csv(TFLA.R1, "TFLA.R1.csv", row.names = FALSE)
TFLA.R1 <- read.csv("TFLA.R1.csv")
head(TFLA.R1)

####Plot comb TFLA.R1####
p1TF <- ggplot(TFLA.R1, aes(x=trtmt.grp, y=R1.prop.rec, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Proportion of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

p2TF <- ggplot(TFLA.R1, aes(x=trtmt.grp, y=R1.recruit, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")  


library(ggpubr)
pcombTF <- ggarrange(p1TF, p2TF, 
          labels = c("A", "B"),
          ncol=1, nrow=2, 
          common.legend = TRUE, legend = "right")

###Add title to figure
annotate_figure(pcombTF, top = text_grob("Response 1 in Tierra Firme", 
                                        color = "black", face = "bold", size = 20))

###Plot both times together...
p3TF <- ggplot(rec.trialTFLAall, aes(x=trtmt.grp, y=recruit, fill=trtmt.grp)) + 
  geom_boxplot() +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun.y=mean, geom="point", shape=15, size=4, color="black", fill="black")  

###Plot both times together...
p4TF <- ggplot(rec.trialTFLAall, aes(x=trtmt.grp, y=prop.obs, fill=trtmt.grp)) + 
  geom_boxplot() +
  labs(x="Treatment", y="Proportion of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun.y=mean, geom="point", shape=15, size=4, color="black", fill="black")  

###################################Response 2#####
####Select those that moved in closer from pre to post Response 2######
####Subset data frame for variables of interest####

safetyTFLA <- read.csv("22safetycueTFLA.csv")
safetyTFLA <- subset(safetyTFLA, select = c("trial","treatment","stage","spp", "distance", "Time.Slot"))
head(safetyTFLA)

####Subset dataframe for variables of interest Resp2####
#Want to say if pre distance is less than post distance...but think I need to aggregate
#so that spp are side by side here. might need to first put through master for loop 
#if that is the case then I need to somehow keep distance for each spp in master loop 
#Or do i....Can maybe just aggregate/merge from start...lets see

safetyTFLA.R2 <- safetyTFLA
head(safetyTFLA.R2)

####Split dataframe into pre and post####
safetyTFLA.R2.Pre <- safetyTFLA[which(safetyTFLA$stage=="Pre"),] #335 obs
head(safetyTFLA.R2.Pre)
safetyTFLA.R2.Post <- safetyTFLA[which(safetyTFLA$stage=="Post"),] #390 obs
head(safetyTFLA.R2.Post)

###Try adding unique identifiers to each spp in both pre and post then combining this way
library(dplyr)

safetyTFLA.R2.Pre <- safetyTFLA.R2.Pre %>% 
  group_by(trial, spp) %>% 
  mutate(Spp_id = paste0(spp, "_", row_number()))

head(safetyTFLA.R2.Pre)

safetyTFLA.R2.Post <- safetyTFLA.R2.Post %>% 
  group_by(trial, spp) %>% 
  mutate(Spp_id = paste0(spp, "_", row_number()))

head(safetyTFLA.R2.Post)
###Perfect! This way no one is being counted or merged more than once

#Now each observation has a unique identifier for both stages
#Now can combine 
safetyTFLA.R2.comb <- merge(safetyTFLA.R2.Pre, safetyTFLA.R2.Post, by=c("Spp_id", "trial"), all.x = TRUE)
head(safetyTFLA.R2.comb) #335 obs, by adding in all.x, we don't lose trials with no recruits
###OMG i think it worked! 
#Can double check with anti join but columns need to be the same, look later
anti_join()

#Remove duplicate columns
safetyTFLA.R2.comb <- subset(safetyTFLA.R2.comb, select = c("trial", "treatment.x", "spp.x", "distance.x", "distance.y", "Time.Slot.x"))
###I think this may have worked! Just need to remove any rows with NAs
#rename columns 
names(safetyTFLA.R2.comb) <- c("Trial", "Treatment", "Spp", "Pre.Dist", "Post.Dist", "Time.Slot")

###order new data set to add 
safetyTFLA.R2.comb <- safetyTFLA.R2.comb[order(safetyTFLA.R2.comb$Trial),]
head(safetyTFLA.R2.comb)
#Add column to show if this counts as response 2 or not
safetyTFLA.R2.comb$R2.recruit <- ifelse(safetyTFLA.R2.comb$Pre.Dist > safetyTFLA.R2.comb$Post.Dist, 1,0)
head(safetyTFLA.R2.comb)
head(TFLA.R1)

####Add treatment group to all trials####
safetyTFLA.R2.comb$trtmt.grp <- substr(safetyTFLA.R2.comb$Treatment, 1, 1)
tail(safetyTFLA.R2.comb)

# create vector
trmt.grp <- safetyTFLA.R2.comb$trtmt.grp

# replacement vector
new <- NULL

# start for loop, using 1:length(test) lets us index the values for replacement; 
# take the 1st spot and put in "fff", second spot = "fff" and so on. 
for (i in 1:length(trmt.grp)) {
  
  # if the first value is "FFF" then take the ith value and pop in "fff"
  # OR IF FALSE (else) MOVE ON,
  if (trmt.grp[i] == "F") {new[i] <- "Flock"} else 
    
    # if the ith value is "CL2" then take ith of "new" object and pop in "cl2"
    # OR, IF STILL FALSE, MOVE ON (else)
    if (trmt.grp[i] == "C") {new[i] <- "Control"} else 
      
      # finally, if ith value of test is "GG1", then take ith value of new and 
      # put in "gg1". 
      if (trmt.grp[i] == "S") {new[i] <- "NonFlock"} 
  
  # you don't need a final "else" here because all options are consumed by the above if/else conditions. if there are options leftover, let's say "test" ended with "CCC" you'd get an error with this code, but that's okay because you'd want to be alerted to the fact that there's a value you haven't told R what to do with (in this case "CCC").
  
}

# output should be all lowercase now,
# then you can just "$" it into the database! 

safetyTFLA.R2.comb$trtmt.grp <- new
head(safetyTFLA.R2.comb)


####Remove conspecifics from Solo spp####
unique(safetyTFLA.R2.comb$trtmt.grp)
head(safetyTFLA.R2.comb) #335 obs

install.packages("dplyr")
library(dplyr)

#NF <- pre.v.postVALA30.1 %>% 
#filter(trtmt.grp=="NonFlock" ) #Don't need this part i dont think
minuscon <- safetyTFLA.R2.comb %>% 
  filter(trtmt.grp=="NonFlock" ) %>% filter(!Spp=="HEGR" & !Spp== "LECO"& !Spp== "MOMO" & !Spp== "PLCO" & !Spp== "LAHY")

head(minuscon)
sum(safetyTFLA.R2.comb$Spp=="HEGR")
sum(minuscon$Spp=="MOMO")
#check total obsservations in nonflock
sum(safetyTFLA.R2.comb$trtmt.grp=="NonFlock")
length(minuscon$Spp)

#Split df to contain all treatments but nonflock
minusNF <- subset(safetyTFLA.R2.comb, !trtmt.grp=="NonFlock")
#Check this removed nonflock treatment
sum(minusNF$trtmt.grp=="NonFlock")

#ReCombine the nonflock minus conspeficis and all trtms but nonflock
safetyTFLA.R2.comb <- rbind(minuscon, minusNF)
head(safetyVALA.R2.comb)
#Order this shit
safetyTFLA.R2.comb <- safetyTFLA.R2.comb %>% arrange(safetyTFLA.R2.comb$Trial)
head(safetyTFLA.R2.comb)

####Get R2 data set ready to combine with R1####
#Need to add up R2 recruits with aggregate function by trial
#Need to get proportion of responses by bringing in spp.per.trial from R1
#Columns in R1: Trial, treatments, trtmt.grp, R1.recruit, R1.spp.per.trial, R1.spp.rec.per.trial,
#R1.prop.rec, #Time, Forest.type

safetyTFLA.R2.comb$forest.type <- rep("TF")
safetyTFLA.R2.comb$Time.Slot <- ifelse(safetyTFLA.R2.comb$Time.Slot==1, "Time 6:30-8:30","Time 8:30+")
names(safetyTFLA.R2.comb)[6] <- ("Time")
names(safetyTFLA.R2.comb)[2] <- ("treatments")

####Get spp.per.trial and spp.rec.per.trial for prop rec
head(safetyTFLA)
safetyTFLA.R2.Pre <- as.data.frame(safetyTFLA.R2.Pre)
safetyTFLA.R2.Post <- as.data.frame(safetyTFLA.R2.Post)
safetyTFLA.R2 <- rbind(safetyTFLA.R2.Pre, safetyTFLA.R2.Post)
head(safetyTFLA.R2)
safetyTFLA.R2 <- safetyTFLA.R2[order(safetyTFLA.R2$trial),]
head(safetyTFLA.R2)

###Set up for loop to count number of species observed per trial
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(safetyTFLA.R2$trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(safetyTFLA.R2, trial==i) 
  species.no<-length(unique(safetyTFLA.R2$spp[which(safetyTFLA.R2$trial==i)]))
  spp.per.trial<-c(spp.per.trial, species.no)
}

#Create date frame of spp per trial #
spp.per.trial<-data.frame(unique.trials, spp.per.trial)
#Change names
names(spp.per.trial) <- c("Trial", "R2.spp.per.trial")
head(spp.per.trial)

###Now count spp.rec.trial
head(safetyTFLA.R2.comb)
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.rec.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(safetyTFLA.R2.comb$Trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(safetyTFLA.R2.comb, Trial==i) 
  species.no<-length(unique(safetyTFLA.R2.comb$Spp[which(safetyTFLA.R2.comb$Trial==i & safetyTFLA.R2.comb$R2.recruit>=1)]))
  spp.rec.per.trial<-c(spp.rec.per.trial, species.no)
}

#Create date frame of spp per trial #
spp.rec.per.trial<-data.frame(unique.trials, spp.rec.per.trial)
#Change names
names(spp.rec.per.trial) <- c("Trial", "R2.spp.rec.per.trial")
head(spp.rec.per.trial)
nrow(spp.rec.per.trial)
nrow(spp.per.trial)
#have the same amount of rows in all three to combine!

##Create data frame of just trial and treatment
head(safetyTFLA.R2)
trial.trtmt.time <- subset(safetyTFLA.R2, select = c("trial","treatment", "Time.Slot"))
trial.trtmt.time <- unique(trial.trtmt.time)

###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial
install.packages("tidyverse")
library(tidyverse)

##Need to rename column time in trial.trtmt.time df
names(trial.trtmt.time)[1] <- "Trial"
safetyTFLA.prop.R2 <- merge(trial.trtmt.time, c(spp.per.trial, spp.rec.per.trial), by="Trial")
head(safetyTFLA.prop.R2)

#Remove duplicate columns
safetyTFLA.prop.R2 <- safetyTFLA.prop.R2[,-5]

###Combine recruits per trial and number of species per trial to get proportion 
safetyTFLA.prop.R2$R2.prop.rec <- safetyTFLA.prop.R2$spp.rec.per.trial/safetyTFLA.prop.R2$spp.per.trial
###Check it out...
head(safetyTFLA.prop.R2)
#Good!

#Rename columns
names(safetyTFLA.prop.R2)[4:5] <- c("R2.spp.per.trial", "R2.spp.rec.per.trial")

####Read in safetyTFLA.R2.comb here####
head(safetyTFLA.R2.comb)
write.csv(safetyTFLA.R2.comb, "safetyTFLA.R2.comb.csv", row.names = FALSE)

safetyTFLA.R2.comb <- read.csv("safetyTFLA.R2.comb.csv")

####Read in safetyTFLA.prop.R2 here####
head(safetyTFLA.prop.R2)
write.csv(safetyTFLA.prop.R2, "safetyTFLA.prop.R2.csv", row.names = FALSE)
safetyTFLA.prop.R2 <- read.csv("safetyTFLA.prop.R2.csv")
unique(safetyVALA.prop.R2$Trial)
nrow(safetyTFLA.prop.R2)
####Add number of recruits to this safety TFLA.prop.R2####
#aggregate here
head(safetyTFLA.R2.comb)

##Replace NAs in R2 recruits with 0s
safetyTFLA.R2.comb$R2.recruit[is.na(safetyTFLA.R2.comb$R2.recruit)]<-0

###Sum by trial using aggregate function, then run through Alex code to add trtmt group
safetyTFLA.rec.R2 <- aggregate(R2.recruit~ Trial+treatments+Time+trtmt.grp, safetyTFLA.R2.comb, sum)
head(safetyTFLA.rec.R2)

###order new data set to add 
safetyTFLA.rec.R2 <- safetyTFLA.rec.R2[order(safetyTFLA.rec.R2$Trial),]
head(safetyTFLA.rec.R2)
nrow(safetyTFLA.rec.R2)

##Rename so trials are the same
head(safetyTFLA.prop.R2)
names(safetyTFLA.rec.R2)[2] <- "Treatment"
names(safetyTFLA.prop.R2)[2] <- "Treatment"
##Fix times in prop R2
safetyTFLA.prop.R2$Time <- ifelse(safetyTFLA.prop.R2$Time.Slot==1, "Time 6:30-8:30","Time 8:30+")
#Remove time.slot from dataframe
safetyTFLA.prop.R2$Time.Slot <- NULL

###Now combine data frames for prop and recs
#Using merge
TFLA.R2 <- merge(safetyTFLA.prop.R2, safetyTFLA.rec.R2, by=c("Trial", "Treatment", "Time"))
head(TFLA.R2)
head(TFLA.R1)
names(TFLA.R1)[1:2] <- c("Trial","Treatment")

###Read in TFLA.R2 here####
write.csv(TFLA.R2, "TFLA.R2.csv", row.names = FALSE)
TFLA.R2 <- read.csv("TFLA.R2.csv")
head(TFLA.R2)


####Plot R2####
prop.plotTFLA.R2.t1 <- ggplot(TFLA.R2[TFLA.R2$Time=="Time 6:30-8:30",], aes(trtmt.grp, R2.prop.rec)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme R2", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") +scale_fill_manual(values = wes_palette("Moonrise3", n = 3))

prop.plotTFLA.R2.t2 <- ggplot(TFLA.R2[TFLA.R2$Time=="Time 8:30+",], aes(trtmt.grp, R2.prop.rec)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme R2", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") +scale_fill_manual(values = wes_palette("Moonrise3", n = 3))

num.plotTFLA.R2.t1 <- ggplot(TFLA.R2[TFLA.R2$Time=="Time 6:30-8:30",], aes(trtmt.grp, R2.recruit)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme R2", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))

num.plotTFLA.R2.t2 <- ggplot(TFLA.R2[TFLA.R2$Time=="Time 8:30+",], aes(trtmt.grp, R2.recruit)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme R2", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))

prop.plotTFLA.R2 <- ggplot(VALA.R2, aes(x=trtmt.grp, y=R2.prop.rec, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Proportion of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme(text = element_text(size = 16))

num.plotTFLA.R2 <- ggplot(TFLA.R2, aes(x=trtmt.grp, y=R2.recruit, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme(text = element_text(size = 16))

install.packages("ggpubr")
library(ggpubr)
pcombTF.R2 <- ggarrange(prop.plotTFLA.R2, num.plotTFLA.R2,
                       labels = c("A", "B"),
                       ncol=1, nrow=2, 
                       common.legend = TRUE, legend = "right")
pcombTF.R2

###Add title to figure
annotate_figure(pcombTF.R2, top = text_grob("Response 2 in Tierra Firme", 
                                           color = "black", face = "bold", size = 20))

pcombTFR1R2 <- ggarrange(prop.plotTFLA.R2, num.plotTFLA.R2, p1V, p2V,
                        labels = c("A", "B", "C", "D"),
                        ncol=2, nrow=4, 
                        common.legend = TRUE, legend = "right")

pcombTFR1R2


###Combine data frames for both responses
head(TFLA.R1)
head(TFLA.R2)

####Merge R1 and R2 responses into one data frame####
TFLA.Rcomb <- merge(TFLA.R1, TFLA.R2, by= c("Trial", "Treatment", "trtmt.grp", "Time"))
head(TFLA.Rcomb)

###Order data frame
TFLA.Rcomb <- TFLA.Rcomb[, c(1,2,3,4,9,6,10,7,11,8,12,5,13)]
TFLA.Rcomb <- TFLA.Rcomb[order(TFLA.Rcomb$Trial),]

####Now combine R1 and R2 recruits####
TFLA.Rcomb$Total.Recs <- TFLA.Rcomb$R2.recruit+TFLA.Rcomb$R1.recruit

num.plotTFLAcomb <- ggplot(TFLA.Rcomb, aes(x=trtmt.grp, y=Total.Recs, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme(text = element_text(size = 16)) 

####Now combine R1 and R2 spp per trial, spp rec per trial, prop recs####
##Is my spp per trial wrong....?
head(TFLA.Rcomb)
TFLA.Rcomb$Total.spp.trial <- TFLA.Rcomb$R2.spp.per.trial+TFLA.Rcomb$R1.spp.per.trial
TFLA.Rcomb$Total.spp.rec.trial <- TFLA.Rcomb$R2.spp.rec.per.trial+TFLA.Rcomb$R1.spp.rec.per.trial
TFLA.Rcomb$Total.prop.rec <- TFLA.Rcomb$Total.spp.rec.trial/TFLA.Rcomb$Total.spp.trial

###Order data frame
TFLA.Rcomb <- TFLA.Rcomb[, c(1,2,3,4,5,6,7,15,8,9,16,17,12,13,14)]

###Plot TFLA comb prop
prop.plotTFLAcomb <- ggplot(TFLA.Rcomb, aes(x=trtmt.grp, y=Total.prop.rec, fill=trtmt.grp)) + 
  geom_boxplot() +
  facet_wrap(~Time) +
  labs(x="Treatment", y="Proportion of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme(text = element_text(size = 16)) 

prop.plotTFLAcomb

pcombTFRcomb <- ggarrange(prop.plotTFLAcomb, num.plotTFLAcomb,
                         labels = c("A", "B"),
                         ncol=1, nrow=2, 
                         common.legend = TRUE, legend = "right")
pcombTFRcomb

###Add title to figure
annotate_figure(pcombTFRcomb, top = text_grob("Response 1 & 2 in Tierra Firme", 
                                             color = "black", face = "bold", size = 20))


####Read TFLA.Rcomb in here####
write.csv(TFLA.Rcomb, "TFLA.Rcomb.csv", row.names = FALSE)
TFLA.Rcomb <- read.csv("TFLA.Rcomb.csv")


###Net Distance####
safetyTFLA.R2.comb <- read.csv("safetyTFLA.R2.comb.csv")
head(safetyTFLA.R2.comb)

#check class of variables
sapply(safetyVALA.R2.comb,class)

#change pre.dist to numeric variable
safetyTFLA.R2.comb$Pre.Dist <- as.numeric(safetyTFLA.R2.comb$Pre.Dist)
safetyTFLA.R2.comb$Post.Dist <- as.numeric(safetyTFLA.R2.comb$Post.Dist)

#Remove spp not seen in both pre and post
net.dist.TFLA <- na.omit(safetyTFLA.R2.comb)
head(net.dist.TFLA)

#Post.Dist - Pre.Dist to get net dist change from pre to post for all individuals seen in both 
#negative number is away, positive number is toward
net.dist.TFLA$Net.Dist <- net.dist.TFLA$Pre.Dist - net.dist.TFLA$Post.Dist

#plot
ggplot(net.dist.TFLA, aes(trtmt.grp, Net.Dist)) + geom_boxplot(aes(fill=trtmt.grp)) + theme_bw() +labs(title="Tierra Firme Individual Net Distance", x="Treatment", y="Net Dist Moved (m)") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") + scale_fill_manual(values = wes_palette("Moonrise3", n = 3))
