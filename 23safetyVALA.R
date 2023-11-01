##Analysis for safety cue thesis work in varzea forest 


### Clear R's brain
rm(list = ls()) 

###Set working directory

###Read in libraries
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(wesanderson)
library(viridis)

###read in TFLA csv file and create object
safetyVLA<-read.csv("23safetycueVLA.csv", header=TRUE)
VAmaster <- read.csv("23.VA.MASTER.csv")

###Quick look over
head(safetyVLA)
head(VAmaster)

####Subset dataframe for variables of interest####
safetyVLA <- subset(safetyVLA, select = c("Trial","Treatment", "Exemplar", "Stage","Spp", "ID", "Distance"))
sapply(safetyVLA, class)
safetyVLA$Trial <- as.numeric(safetyVLA$Trial)

#Keep only Trial # and location in master 
####Subset dataframe for variables of interest####
VAmaster <- subset(VAmaster, select = c("Prueba","Point"))
names(VAmaster)[1] <- "Trial"

############Response 1################################################################
#Absent in pre-trial within 30m, present within 30m in post-trial
####Remove anything greater than 30m distance Response 1#### Do we need to do this still??
safetyVLA30 <- safetyVLA[!safetyVLA$Distance>30,]
head(safetyVLA30)
#Check if all distances above 30m were removed
length(safetyVLA30$Stage[which(safetyVLA30$Distance>30)])
safetyVLA30[safetyVLA30$Distance>30,]
unique(safetyVLA30$Trial) #140 trials included

###################TFLA Response 1#####################################################
###Prepare for for loops 
#Create spp list of all spp with ID observed throughout experiment to add to pre and post
ID.list<-unique(safetyVLA30$ID)
#Create treatment list to add to each line
Treatment.list<-sort(unique(safetyVLA30$Treatment))
#with exemplar 
Exemplar.list <- sort(unique(safetyVLA30$Exemplar))

#Create null vectors for each variable of interest
Treatment<-NULL
Exemplar <- NULL
Count<- NULL
ID<-NULL
Trial<-NULL

for(i in Exemplar.list){
  s1<-subset(safetyVLA30, safetyVLA30$Exemplar == i) #i=treatment
  trial.list<-sort(unique(s1$Trial))
  for(j in trial.list){
    s2<-subset(s1, s1$Trial == j) #j= add trial number to each row
    Treatment.list <- sort(unique(s2$Treatment))
    for(k in Treatment.list){
      s3 <- subset(s2, s2$Treatment == k) #add treatment to each row
      #counts<-NULL
      for(l in ID.list){
        s4<-subset(s3, s3$ID == l) #k=species
        if(dim(s4)[1] == 0){cts<-c(0,0)} else{cts<-c(length(which(s4$Stage=="PRE")), #if row 1 of s3=0, then make vector of pre/post=0,0, if s3 is not 0, then cts vector = length of pre, length of post
                                                     length(which(s4$Stage=="POST")))}
        #this is where you can add anything to summarize distances moved, etc, total before.after, diversity,
        #copy if statement and change inner bits, dist traveled change c(0, NA)
        Count=rbind(Count, cts) #combine rows for counts of pre and post
        Exemplar<-c(Exemplar, i) #combines exemplars
        Trial<-c(Trial, j) #adds trial
        Treatment <- c(Treatment, k) #combines treatments
      }
      ID<-c(ID, ID.list) #creates ID column
    }
  }
}

###Combine all vectors to make data frame of each variable of interest
pre.v.postVLA30<-as.data.frame(cbind(Trial, Treatment, Exemplar, ID, Count))

###Rename columns
names(pre.v.postVLA30)<-c("Trial", "Treatment", "Exemplar", "ID", "Pre.count", "Post.count")
head(pre.v.postVLA30)

###Make column for Spp
pre.v.postVLA30$Spp <- gsub('[0-9.]', '', pre.v.postVLA30$ID)
head(pre.v.postVLA30)

###Reorder df
###Order data frame
pre.v.postVLA30 <- pre.v.postVLA30[, c(1,2,3,7,4,5,6)]
###order new data set to add 
pre.v.postVLA30$Trial <- as.numeric(pre.v.postVLA30$Trial)
pre.v.postVLA30 <- pre.v.postVLA30[order(pre.v.postVLA30$Trial),]
#pre.v.postTFLA30 %>% arrange(Trial) can also do this way
head(pre.v.postVLA30)

###Create vectors of pre and post counts to the find response totals
Prevec <- as.numeric(pre.v.postVLA30$Pre.count)
Postvec <- as.numeric(pre.v.postVLA30$Post.count)
pre.v.postVLA30$Recruit <- ifelse(pre.v.postVLA30$Pre.count >= pre.v.postVLA30$Post.count, 0, Postvec-Prevec)
head(pre.v.postVLA30)

###Create new csv file
write.csv(pre.v.postVLA30, "23.pre.v.postVLA30.csv", row.names = FALSE)

####read in 23.pre.v.postTFLA30####
pre.v.postVLA30 <- read.csv("23.pre.v.postVLA30.csv", header=T)
head(pre.v.postVLA30)

###Remove conspecifics from NF spp
unique(pre.v.postVLA30$Treatment)
head(pre.v.postVLA30)

#NF <- pre.v.postTFLA30.1 %>% 
#filter(trtmt.grp=="NonFlock" )
minuscon.R1 <- pre.v.postVLA30 %>% 
  filter(Treatment=="NF" ) %>% filter(!Spp=="HEGRI" & !Spp== "LECOR" & !Spp== "MOMOM" & !Spp== "PLCOR" & !Spp== "LAHYP")

head(minuscon.R1)
#check spp were actually removed
sum(minuscon.R1$Spp=="MOMOM")
#check total observations in nonflock vs how many in minuscon 
sum(pre.v.postVLA30$Treatment=="NF")
sum(minuscon.R1$Treatment=="NF")
#-- looks like 160 were removed

#Split data frame into all but nonflock
minusNF.R1 <- subset(pre.v.postVLA30, !Treatment=="NF") #18088 obs

#Check this removed nonflock treatment
sum(minusNF.R1$Treatment=="NF") #good

#ReCombine the nonflock minus conspeficis and all trtmts but nonflock
pre.v.postVLA30 <- rbind(minuscon.R1, minusNF.R1)
head(pre.v.postVLA30)
###order new data set to add 
pre.v.postVLA30 <- pre.v.postVLA30[order(pre.v.postVLA30$Trial),]

####Create new csv file over original pre.v.post.TFLA30.1####
write.csv(pre.v.postVLA30, "pre.v.postVLA30.csv", row.names = FALSE)
pre.v.postVLA30 <- read.csv("pre.v.postVLA30.csv")
head(pre.v.postVLA30)

####Sum number of recruits by trial using aggregate function####
rec.trialVLA30 <- aggregate(Recruit~ Trial+Treatment+Exemplar, pre.v.postVLA30, sum)
head(rec.trialVLA30)

###order new data set to add 
rec.trialVLA30 <- rec.trialVLA30[order(rec.trialVLA30$Trial),]

####Create new csv file for number of recruits for Response 1####
write.csv(rec.trialVLA30, "rec.trialVLA30.csv", row.names = FALSE)
rec.trialVLA30 <- read.csv("rec.trialVLA30.csv")

####Set proportions Response 1 Time 1####
###Set up for loop to count number of species observed per trial using code above
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
head(pre.v.postVLA30)
Spp.per.trial <- NULL
#Create vector of trials to run through
Unique.trials<-unique(pre.v.postVLA30$Trial)
#Start for loop
for(i in Unique.trials){
  Trial<-subset(pre.v.postVLA30, pre.v.postVLA30$Trial==i) 
  Species.no<-length(unique(pre.v.postVLA30$Spp[which(pre.v.postVLA30$Trial==i & pre.v.postVLA30$Pre.count>=1 | pre.v.postVLA30$Post.count>=1 & pre.v.postVLA30$Trial==i)]))
  Spp.per.trial<-c(Spp.per.trial, Species.no) 
}
#Create date frame of spp per trial #
Spp.per.trial<-data.frame(Unique.trials, Spp.per.trial)
#Change names
names(Spp.per.trial) <- c("Trial", "Spp.per.trial")
head(Spp.per.trial)

###Count number of spp responding in pre.v.post df using for loop
head(pre.v.postVLA30)
Spp.rec.per.trial <- NULL

Unique.trials<-unique(pre.v.postVLA30$Trial)

for(i in Unique.trials){
  Trial<-subset(pre.v.postVLA30, Trial==i) #
  Species.no<-length(which(Trial$Recruit>=1))
  Spp.rec.per.trial<-c(Spp.rec.per.trial, Species.no)
}
#Create data frame of spp recruited per trial
Spp.rec.per.trial<-data.frame(Unique.trials, Spp.rec.per.trial)
#Change names
names(Spp.rec.per.trial) <- c("Trial", "Spp.rec.per.trial")

###Create data frame of just trial and treatment
head(rec.trialVLA30)
Trial.trtmt <- subset(rec.trialVLA30, select = c("Trial", "Treatment","Exemplar"))
head(Trial.trtmt)
###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial
str(Spp.rec.per.trial)

####Merge data to get prop df####
Prop.trialVLA30 <- merge(Trial.trtmt, c(Spp.per.trial, Spp.rec.per.trial), by="Trial", all = TRUE)
###Combine recruits per trial and number of species per trial to get proportion 
Prop.trialVLA30$Prop.rec <- Prop.trialVLA30$Spp.rec.per.trial/Prop.trialVLA30$Spp.per.trial
###Check it out...
head(Prop.trialVLA30)
###Replace NaNs with 0s
Prop.trialVLA30$Prop.rec[is.nan(Prop.trialVLA30$Prop.rec)]<-0 #There are no NAs

####Merge prop rec and rec Response 1 Time 1####
###Now need to add number of recruits per trial to this
#Using merge
VLA.R1 <- merge(rec.trialVLA30, Prop.trialVLA30)
head(VLA.R1)

#Subset variables of interest aka remove duplicate columns
VLA.R1 <- VLA.R1[-6]

###order new data set to add 
VLA.R1 <- VLA.R1[order(VLA.R1$Trial),]

###Add trial location column
head(VLA.R1)
VLA.R1 <- merge(VAmaster, VLA.R1, by="Trial")
###Order data frame
VLA.R1 <- VLA.R1[, c(1,2,3,4,9,5,6,7,8)]

####Write final Response 1 time 1 new csv/Read in####
write.csv(VLA.R1, "VLA.R1.csv", row.names = FALSE)
VLA.R1 <- read.csv("VLA.R1.csv")
head(VLA.R1)

###Plot using ggplot

prop.plotVLA.R1 <- ggplot(VLA.R1, aes(Treatment, Prop.rec)) + geom_boxplot(aes(fill=Treatment)) + theme_bw() +labs(title="Varzea Response 1", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

num.plotVLA.R1 <- ggplot(VLA.R1, aes(Treatment, Recruit)) + geom_boxplot(aes(fill=Treatment)) + theme_bw() +labs(title="Varzea Response 1", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add Forest type column
VLA.R1$Forest.type <- rep("VA")
head(VLA.R1)

###Rename responses as response 1 to prepare for combination with Response 2
names(VLA.R1)[4:7] <- c("R1.recruit", "R1.spp.per.trial", "R1.spp.rec.per.trial", "R1.prop.rec")
head(VLA.R1)

####Write csv for VLA R1 both times####
write.csv(VLA.R1, "VLA.R1.csv", row.names = FALSE)
VLA.R1 <- read.csv("VLA.R1.csv")
head(VLA.R1)

####Plot comb VLA.R1####
p1V <- ggplot(VLA.R1, aes(x=Treatment, y=R1.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + theme_bw() +
  theme(axis.title.x = element_blank(), axis.title = element_text(size=15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

p2V <- ggplot(VLA.R1, aes(x=Treatment, y=R1.recruit, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + theme_bw() +
  theme(axis.title = element_text(size=15))+
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")  


pcombV <- ggarrange(p1V, p2V, 
                     labels = c("A", "B"),
                     ncol=1, nrow=2, 
                     common.legend = TRUE, legend = "right")

###Add title to figure
annotate_figure(pcombV, top = text_grob("Response 1 in Varzea", 
                                         color = "black", face = "bold", size = 20))


###################################Response 2#####
####Select those that moved in closer from pre to post Response 2######
####Subset data frame for variables of interest####

safetyVLA <- read.csv("23safetycueVLA.csv")
safetyVLA <- subset(safetyVLA, select = c("Trial","Treatment", "Exemplar","Stage","Spp", "ID", "Distance"))
head(safetyVLA)

####Subset dataframe for variables of interest Resp2####
#Want to say if pre distance is less than post distance...but think I need to aggregate
#so that spp are side by side here. might need to first put through master for loop 
#if that is the case then I need to somehow keep distance for each spp in master loop 
#Or do i....Can maybe just aggregate/merge from start...lets see

safetyVLA.R2 <- safetyVLA
head(safetyVLA.R2)
unique(safetyVLA.R2$Stage)
####Split dataframe into pre and post####
safetyVLA.R2.Pre <- safetyVLA[which(safetyVLA$Stage=="PRE"),] #1166 obs 
#filter(safetyTFLA, Stage=="PRE") Can also do this way
head(safetyVLA.R2.Pre)
safetyVLA.R2.Post <- safetyVLA[which(safetyVLA$Stage=="POST"),] #1166 obs
head(safetyVLA.R2.Post)

# each observation has a unique identifier for both stages
# can combine 
safetyVLA.R2.comb <- merge(safetyVLA.R2.Pre, safetyVLA.R2.Post, by=c("Trial", "Treatment", "Exemplar", "Spp", "ID"), all.x = TRUE)
head(safetyVLA.R2.comb) #1159 obs, by adding in all.x, we don't lose trials with no recruits

#Can double check with anti join but columns need to be the same, look later
#anti_join()

#Remove duplicate columns
safetyVLA.R2.comb <- subset(safetyVLA.R2.comb, select = c("Trial", "Treatment", "Exemplar", "Spp", "ID", "Distance.x", "Distance.y"))

###Just need to remove any rows with NAs
#rename columns 
names(safetyVLA.R2.comb) <- c("Trial", "Treatment", "Exemplar", "Spp", "ID", "Pre.Dist", "Post.Dist")
head(safetyVLA.R2.comb)

###order new data set to add 
sapply(safetyVLA.R2.comb, class)
safetyVLA.R2.comb$Trial <- as.numeric(safetyVLA.R2.comb$Trial)
safetyVLA.R2.comb <- safetyVLA.R2.comb[order(safetyVLA.R2.comb$Trial),]
head(safetyVLA.R2.comb)

#Add column to show if this counts as response 2 or not
safetyVLA.R2.comb$R2.recruit <- ifelse(safetyVLA.R2.comb$Pre.Dist > safetyVLA.R2.comb$Post.Dist, 1,0)
head(safetyVLA.R2.comb)
head(VLA.R1)

####Remove conspecifics from Solo spp####
unique(safetyVLA.R2.comb$Treatment)
head(safetyVLA.R2.comb) #335 obs

#NF <- pre.v.postVALA30.1 %>% 
#filter(trtmt.grp=="NonFlock" ) #Don't need this part i dont think
minuscon.R2 <- safetyVLA.R2.comb %>% 
  filter(Treatment=="NF" ) %>% filter(!Spp=="HEGRI" & !Spp== "LECOR"& !Spp== "MOMOM" & !Spp== "PLCOR" & !Spp== "LAHYP")

head(minuscon.R2)
sum(safetyVLA.R2.comb$Spp=="HEGRI")
sum(minuscon.R2$Spp=="HEGRI")
#check total obsservations in nonflock
sum(safetyVLA.R2.comb$Treatment=="NF")
length(minuscon.R2$Spp)

#Split df to contain all treatments but nonflock
minusNF.R2 <- subset(safetyVLA.R2.comb, !Treatment=="NF")
#Check this removed nonflock treatment
sum(minusNF.R2$Treatment=="NF")

#ReCombine the nonflock minus conspeficis and all trtms but nonflock
safetyVLA.R2.comb <- rbind(minuscon.R2, minusNF.R2)
head(safetyVLA.R2.comb)
#Order this shit
safetyVLA.R2.comb <- safetyVLA.R2.comb %>% arrange(safetyVLA.R2.comb$Trial)
head(safetyVLA.R2.comb)

####Get R2 data set ready to combine with R1####
#Need to add up R2 recruits with aggregate function by trial
#Need to get proportion of responses by bringing in spp.per.trial from R1
#Columns in R1: Trial, treatments, trtmt.grp, R1.recruit, R1.spp.per.trial, R1.spp.rec.per.trial,
#R1.prop.rec, #Time, Forest.type

safetyVLA.R2.comb$Forest.type <- rep("VA")
head(safetyVLA.R2.comb)

####Get spp.per.trial and spp.rec.per.trial for prop rec
head(safetyVLA)
safetyVLA.R2.Pre <- as.data.frame(safetyVLA.R2.Pre)
safetyVLA.R2.Post <- as.data.frame(safetyVLA.R2.Post)
safetyVLA.R2 <- rbind(safetyVLA.R2.Pre, safetyVLA.R2.Post)
head(safetyVLA.R2)
safetyVLA.R2 <- safetyVLA.R2[order(safetyVLA.R2$Trial),]
head(safetyVLA.R2)

###Set up for loop to count number of species observed per trial
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(safetyVLA.R2$Trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(safetyVLA.R2, Trial==i) 
  species.no<-length(unique(safetyVLA.R2$Spp[which(safetyVLA.R2$Trial==i)]))
  spp.per.trial<-c(spp.per.trial, species.no)
}

#Create date frame of spp per trial #
spp.per.trial<-data.frame(unique.trials, spp.per.trial)
#Change names
names(spp.per.trial) <- c("Trial", "R2.spp.per.trial")
head(spp.per.trial)

###Now count spp.rec.trial
head(safetyVLA.R2.comb)
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.rec.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(safetyVLA.R2.comb$Trial)

#Start for loop
for(i in unique.trials){
  trial<-subset(safetyVLA.R2.comb, Trial==i) 
  species.no<-length(unique(safetyVLA.R2.comb$Spp[which(safetyVLA.R2.comb$Trial==i & safetyVLA.R2.comb$R2.recruit>=1)]))
  spp.rec.per.trial<-c(spp.rec.per.trial, species.no)
}

#Create date frame of spp per trial #
spp.rec.per.trial<-data.frame(unique.trials, spp.rec.per.trial)
#Change names
names(spp.rec.per.trial) <- c("Trial", "R2.spp.rec.per.trial")
head(spp.rec.per.trial)
nrow(spp.rec.per.trial)
nrow(spp.per.trial)
#have the same amount of rows in all three to combine! Trial 36 in spp.rec.per.trial is disappearing, why?? is gone in safetyvla.r2.comb also, fixed **due to pre having a space after it in csv

##Create data frame of just trial and treatment
head(safetyVLA.R2)
trial.trtmt <- subset(safetyVLA.R2, select = c("Trial","Treatment"))
trial.trtmt <- unique(trial.trtmt)

###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial

##Need to rename column time in trial.trtmt.time df
safetyVLA.prop.R2 <- merge(trial.trtmt, c(spp.per.trial, spp.rec.per.trial), by="Trial")
head(safetyVLA.prop.R2)

#Remove duplicate columns
safetyVLA.prop.R2 <- safetyVLA.prop.R2[,-4]

###Combine recruits per trial and number of species per trial to get proportion 
safetyVLA.prop.R2$R2.prop.rec <- safetyVLA.prop.R2$R2.spp.rec.per.trial/safetyVLA.prop.R2$R2.spp.per.trial
###Check it out...
head(safetyVLA.prop.R2)
#Good!

####Read in safetyVLA.R2.comb here####
head(safetyVLA.R2.comb)
write.csv(safetyVLA.R2.comb, "safetyVLA.R2.comb.csv", row.names = FALSE)

safetyVLA.R2.comb <- read.csv("safetyVLA.R2.comb.csv")

####Read in safetyVLA.prop.R2 here####
head(safetyVLA.prop.R2)
write.csv(safetyVLA.prop.R2, "safetyVLA.prop.R2.csv", row.names = FALSE)
safetyVLA.prop.R2 <- read.csv("safetyVLA.prop.R2.csv")
unique(safetyVLA.prop.R2$Trial)
nrow(safetyVLA.prop.R2)
####Add number of recruits to this safety TFLA.prop.R2####
#aggregate here
head(safetyVLA.R2.comb)

##Replace NAs in R2 recruits with 0s
safetyVLA.R2.comb$R2.recruit[is.na(safetyVLA.R2.comb$R2.recruit)]<-0

###Sum by trial using aggregate function, then run through Alex code to add trtmt group
safetyVLA.rec.R2 <- aggregate(R2.recruit~ Trial+Exemplar+Treatment, safetyVLA.R2.comb, sum)
head(safetyVLA.rec.R2)

###order new data set to add 
safetyVLA.rec.R2$Trial <- as.numeric(safetyVLA.rec.R2$Trial)
safetyVLA.rec.R2 <- safetyVLA.rec.R2[order(safetyVLA.rec.R2$Trial),]
head(safetyVLA.rec.R2)
nrow(safetyVLA.rec.R2)

###Now combine data frames for prop and recs
#Using merge
VLA.R2 <- merge(safetyVLA.prop.R2, safetyVLA.rec.R2, by=c("Trial", "Treatment"))
VLA.R2$Trial <- as.numeric(VLA.R2$Trial)
VLA.R2 <- VLA.R2[order(VLA.R2$Trial),]

###Add Forest type column
VLA.R2$Forest.type <- rep("VA")
head(VLA.R2)
head(VLA.R1)

###Add trial location column
head(VLA.R2)
VLA.R2 <- merge(VAmaster, VLA.R2, by="Trial")
###Order data frame
VLA.R2 <- VLA.R2[, c(1,2,3,7,9,8,4,5,6)]

###Read in VLA.R2 here####
write.csv(VLA.R2, "VLA.R2.csv", row.names = FALSE)
VLA.R2 <- read.csv("VLA.R2.csv")
head(VLA.R2)


####Plot R2####
prop.plotVLA.R2 <- ggplot(VLA.R2, aes(Treatment, R2.prop.rec)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +
  labs(y="Proportion of Recruits") + theme_bw() +
  theme(axis.title.x = element_blank(), axis.title = element_text(size=15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

num.plotVLA.R2 <- ggplot(VLA.R2, aes(Treatment, R2.recruit)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + theme_bw()+
  theme(axis.title = element_text(size=15)) +
  guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") 

#library(ggpubr)
pcombV.R2 <- ggarrange(prop.plotVLA.R2, num.plotVLA.R2,
                        labels = c("C", "D"),
                        ncol=1, nrow=2, 
                        common.legend = TRUE, legend = "right")
pcombV.R2

###Add title to figure
annotate_figure(pcombV.R2, top = text_grob("Response 2 in Varzea", 
                                            color = "black", face = "bold", size = 20))

#Combine responses on grid
pcombVR1R2 <- ggarrange(prop.plotVLA.R2, p1V, num.plotVLA.R2, p2V,
                         labels = c("A", "B", "C", "D"),
                         ncol=2, nrow=4, 
                         common.legend = TRUE, legend = "right")

pcombVR1R2
##***problem here with plot labels....R1 is being listed as R2

###Combine data frames for both responses####
head(VLA.R1)
head(VLA.R2)

####Merge R1 and R2 responses into one data frame####
VLA.Rcomb <- merge(VLA.R1, VLA.R2, by= c("Trial", "Exemplar", "Treatment", "Forest.type"))
head(VLA.Rcomb)

###Order data frame
VLA.Rcomb <- VLA.Rcomb[, c(1,3,2,4,5,6,7,8,9,10,11,12)]
VLA.Rcomb$Trial <- as.numeric(VLA.Rcomb$Trial)
VLA.Rcomb <- arrange(VLA.Rcomb, Trial)
#TFLA.Rcomb <- TFLA.Rcomb[order(TFLA.Rcomb$Trial),]

####Now combine R1 and R2 recruits####
VLA.Rcomb$Total.Recs <- VLA.Rcomb$R2.recruit+VLA.Rcomb$R1.recruit

num.plotVLAcomb <- ggplot(VLA.Rcomb, aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size=15)) 

####Now combine R1 and R2 spp per trial, spp rec per trial, prop recs####
head(VLA.Rcomb)
VLA.Rcomb$Total.spp.trial <- VLA.Rcomb$R2.spp.per.trial+VLA.Rcomb$R1.spp.per.trial
VLA.Rcomb$Total.spp.rec.trial <- VLA.Rcomb$R2.spp.rec.per.trial+VLA.Rcomb$R1.spp.rec.per.trial
VLA.Rcomb$Total.prop.rec <- VLA.Rcomb$Total.spp.rec.trial/VLA.Rcomb$Total.spp.trial

###Update all treatments as there are some controls labeled VA
VLA.Rcomb$Exemplar[VLA.Rcomb$Exemplar == "VA5"] <- "CTRL5"
VLA.Rcomb$Exemplar[VLA.Rcomb$Exemplar == "VA4"] <- "CTRL4"
VLA.Rcomb$Exemplar[VLA.Rcomb$Exemplar == "VA2"] <- "CTRL2"

###Add column for solo types
VLA.Rcomb$Tr.Type <- gsub('[0-9.]', '', VLA.Rcomb$Exemplar)

###Order data frame
#TFLA.Rcomb <- TFLA.Rcomb[, c(1,2,3,4,5,6,7,15,8,9,16,17,12,13,14)]

###Add trial location column
head(VLA.Rcomb)
###Order data frame
VLA.Rcomb <- VLA.Rcomb[, c(1,2,17,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
VLA.Rcomb <- merge(VAmaster, VLA.Rcomb, by="Trial")

####Read VLA.Rcomb in here####
write.csv(VLA.Rcomb, "VLA.Rcomb.csv", row.names = FALSE)
VLA.Rcomb <- read.csv("VLA.Rcomb.csv")
#reorder the groups order : I change the order of the factor data$names to order plot
VLA.Rcomb$Tr.Type <- factor(VLA.Rcomb$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

###Plot VLA comb prop
prop.plotVLAcomb <- ggplot(VLA.Rcomb, aes(x=Treatment, y=Total.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + theme_bw() +
  theme(axis.title.x = element_blank(), axis.title = element_text(size = 15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

prop.plotVLAcomb

pcombVRcomb <- ggarrange(prop.plotVLAcomb, num.plotVLAcomb,
                          labels = c("C", "D"),
                          ncol=1, nrow=2, 
                          common.legend = TRUE, legend = "right")
pcombVRcomb

###Add title to figure
annotate_figure(pcombVRcomb, top = text_grob("Response 1 & 2 in Varzea", 
                                              color = "black", face = "bold", size = 20))

###Plot response 1&2 by treatment type####
num.plotV.type <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15)) 

###Plot VLA proportion response 1&2 by treatment type####
prop.plotV.type <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) 

pcombVR.type <- ggarrange(prop.plotV.type, num.plotV.type,
                         labels = c("C", "D"),
                         ncol=1, nrow=2, 
                         legend = "none")
pcombVR.type

###Add title to figure
annotate_figure(pcombVR.type, top = text_grob("Response 1 & 2 in Varzea", 
                                             color = "black", face = "bold", size = 20))


###Net Distance####
safetyVLA.R2.comb <- read.csv("safetyVLA.R2.comb.csv")
head(safetyVLA.R2.comb)

#check class of variables
sapply(safetyVLA.R2.comb,class)

#change pre.dist to numeric variable
safetyVLA.R2.comb$Trial <- as.numeric(safetyVLA.R2.comb$Trial)
safetyVLA.R2.comb$Pre.Dist <- as.numeric(safetyVLA.R2.comb$Pre.Dist)
safetyVLA.R2.comb$Post.Dist <- as.numeric(safetyVLA.R2.comb$Post.Dist)

###Update all treatments as there are some controls labeled VA
unique(safetyVLA.R2.comb$Exemplar)
safetyVLA.R2.comb$Exemplar[safetyVLA.R2.comb$Exemplar == "VA5"] <- "CTRL5"
safetyVLA.R2.comb$Exemplar[safetyVLA.R2.comb$Exemplar == "VA4"] <- "CTRL4"
safetyVLA.R2.comb$Exemplar[safetyVLA.R2.comb$Exemplar == "VA3"] <- "CTRL3"
safetyVLA.R2.comb$Exemplar[safetyVLA.R2.comb$Exemplar == "VA2"] <- "CTRL2"

#Remove spp not seen in both pre and post
net.dist.VLA <- na.omit(safetyVLA.R2.comb)
head(net.dist.VLA)

#Post.Dist - Pre.Dist to get net dist change from pre to post for all individuals seen in both 
#negative number is away, positive number is toward
net.dist.VLA$Net.Dist <- net.dist.VLA$Pre.Dist - net.dist.VLA$Post.Dist

###Add column for solo types
net.dist.VLA$Tr.Type <- gsub('[0-9.]', '', net.dist.VLA$Exemplar)

###Add column for trial location
net.dist.VLA <- merge(VAmaster, net.dist.VLA, by="Trial")
head(net.dist.VLA)

###Order data frame
net.dist.VLA <- net.dist.VLA[, c(1,2,3,12,4,10,5,6,7,8,9,11)]

###Read net.dist.VLA csv in here####
write.csv(net.dist.VLA, "net.dist.VLA23.csv", row.names = FALSE)
net.dist.VLA <- read.csv("net.dist.VLA23.csv")

###plot nest dist by treatment group####
P.net.dist.V <- ggplot(net.dist.VLA, aes(Treatment, Net.Dist)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +labs(x="Treatment", y="Net Dist Moved (m)") + theme_bw() +
  theme(axis.title = element_text(size = 15))+ stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add title to figure
annotate_figure(P.net.dist.V, top = text_grob("Net Distance Change in Varzea", 
                                             color = "black", face = "bold", size = 20))

#Try violin plot
PV.net.dist.VA <- ggplot(net.dist.VLA, aes(Treatment, Net.Dist)) + geom_violin(aes(fill=Treatment), show.legend="none") + theme_bw() + theme(axis.title = element_text(size = 15))+
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3) 

###Add title to figure
annotate_figure(PV.net.dist.VA, top = text_grob("Net Distance Change in Varzea", 
                                              color = "black", face = "bold", size = 20))

###Varzea plots net dist by treatment type####

P.net.dist.V.type <- ggplot(net.dist.VLA, aes(Tr.Type, Net.Dist)) + geom_boxplot(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) + 
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add title to figure
annotate_figure(P.net.dist.V.type, top = text_grob("Net Distance Change in Varzea", 
                                                color = "black", face = "bold", size = 20))

#Try violin plot
PV.net.dist.V.type <- ggplot(net.dist.VLA, aes(Tr.Type, Net.Dist)) + geom_violin(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size = 15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3)

###Add title to figure
annotate_figure(PV.net.dist.V.type, top = text_grob("Net Distance Change in Varzea", 
                                                   color = "black", face = "bold", size = 20)) 

####Proportion UK
UKV <- read.csv("VLAUks.csv")
head(UKV)
prop.uk <- length(which(UKV$X== 1))/2338
