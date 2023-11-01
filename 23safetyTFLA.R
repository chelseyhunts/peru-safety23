##Analysis for safety cue thesis work in tierra firme forest 

###Set working directory

###Read in libraries
library(tidyverse)
library(ggpubr)

###read in TFLA csv file and create object
safetyTFLA<-read.csv("23safetycueTFLA.csv", header=TRUE)
TFLAmaster <- read.csv("23TFLA.MASTER.csv")

###Quick look over
head(safetyTFLA)
head(TFLAmaster)

####Subset dataframe for variables of interest####
safetyTFLA <- subset(safetyTFLA, select = c("Trial","Treatment", "Exemplar", "Stage","Spp", "ID", "Distance"))
sapply(safetyTFLA, class)

#Keep only Trial # and location in master 
####Subset dataframe for variables of interest####
TFLAmaster <- subset(TFLAmaster, select = c("Prueba","Point"))
names(TFLAmaster)[1] <- "Trial"

###Add trial location column
head(safetyTFLA)
safetyTFLA <- merge(TFLAmaster, safetyTFLA, by="Trial")


############Response 1################################################################
#Absent in pre-trial within 30m, present within 30m in post-trial
####Remove anything greater than 30m distance Response 1#### Do we need to do this still??
safetyTFLA30 <- safetyTFLA[!safetyTFLA$Distance>30,]
head(safetyTFLA30)
#Check if all distances above 30m were removed
length(safetyTFLA30$Stage[which(safetyTFLA30$Distance>30)])
safetyTFLA30[safetyTFLA30$Distance>30,]
unique(safetyTFLA30$Trial) #137 trials included

###################TFLA Response 1#####################################################
###Prepare for for loops 
#Create spp list of all spp with ID observed throughout experiment to add to pre and post
ID.list<-unique(safetyTFLA30$ID)
#Create treatment list to add to each line
Treatment.list<-sort(unique(safetyTFLA30$Treatment))
#with exemplar 
Exemplar.list <- sort(unique(safetyTFLA30$Exemplar))

#Create null vectors for each variable of interest
Treatment<-NULL
Exemplar <- NULL
Count<- NULL
ID<-NULL
Trial<-NULL

for(i in Exemplar.list){
  s1<-subset(safetyTFLA30, safetyTFLA30$Exemplar == i) #i=treatment
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
pre.v.postTFLA30<-as.data.frame(cbind(Trial, Treatment, Exemplar, ID, Count))

###Rename columns
names(pre.v.postTFLA30)<-c("Trial", "Treatment", "Exemplar", "ID", "Pre.count", "Post.count")
head(pre.v.postTFLA30)

###Make column for Spp
pre.v.postTFLA30$Spp <- gsub('[0-9.]', '', pre.v.postTFLA30$ID)
head(pre.v.postTFLA30)

###Reorder df
###Order data frame
pre.v.postTFLA30 <- pre.v.postTFLA30[, c(1,2,3,7,4,5,6)]
###order new data set to add 
pre.v.postTFLA30$Trial <- as.numeric(pre.v.postTFLA30$Trial)
pre.v.postTFLA30 <- pre.v.postTFLA30[order(pre.v.postTFLA30$Trial),]
#pre.v.postTFLA30 %>% arrange(Trial) can also do this way
head(pre.v.postTFLA30)

###Create vectors of pre and post counts to the find response totals
Prevec <- as.numeric(pre.v.postTFLA30$Pre.count)
Postvec <- as.numeric(pre.v.postTFLA30$Post.count)
pre.v.postTFLA30$Recruit <- ifelse(pre.v.postTFLA30$Pre.count >= pre.v.postTFLA30$Post.count, 0, Postvec-Prevec)
head(pre.v.postTFLA30)

###Create new csv file
write.csv(pre.v.postTFLA30, "23.pre.v.postTFLA30.csv", row.names = FALSE)

####read in 23.pre.v.postTFLA30####
pre.v.postTFLA30 <- read.csv("23.pre.v.postTFLA30.csv", header=T)
head(pre.v.postTFLA30)

###Remove conspecifics from NF spp
unique(pre.v.postTFLA30$Treatment)
head(pre.v.postTFLA30)

install.packages("dplyr")
library(dplyr)

#NF <- pre.v.postTFLA30.1 %>% 
  #filter(trtmt.grp=="NonFlock" )
minuscon <- pre.v.postTFLA30 %>% 
  filter(Treatment=="NF" ) %>% filter(!Spp=="HEGRI" & !Spp== "LECOR" & !Spp== "MOMOM" & !Spp== "PLCOR" & !Spp== "LAHYP")

head(minuscon)
#check spp were actually removed
sum(minuscon$Spp=="MOMOM")
#check total observations in nonflock vs how many in minuscon 
sum(pre.v.postTFLA30$Treatment=="NF")
sum(minuscon$Treatment=="NF")
#-- looks like 200 were removed

#Split data frame into all but nonflock
minusNF <- subset(pre.v.postTFLA30, !Treatment=="NF") #18954 obs

#Check this removed nonflock treatment
sum(minusNF$Treatment=="NF") #good

#ReCombine the nonflock minus conspeficis and all trtmts but nonflock
pre.v.postTFLA30 <- rbind(minuscon, minusNF)
head(pre.v.postTFLA30)
###order new data set to add 
pre.v.postTFLA30 <- pre.v.postTFLA30[order(pre.v.postTFLA30$Trial),]

####Create new csv file over original pre.v.post.TFLA30.1####
write.csv(pre.v.postTFLA30, "pre.v.postTFLA30.csv", row.names = FALSE)
pre.v.postTFLA30 <- read.csv("pre.v.postTFLA30.csv")
head(pre.v.postTFLA30)
unique(pre.v.postTFLA30$Treatment)


####Sum number of recruits by trial using aggregate function####
rec.trialTFLA30 <- aggregate(Recruit~ Trial+Treatment+Exemplar, pre.v.postTFLA30, sum)
head(rec.trialTFLA30)

###order new data set to add 
rec.trialTFLA30 <- rec.trialTFLA30[order(rec.trialTFLA30$Trial),]

####Create new csv file for number of recruits for Response 1####
write.csv(rec.trialTFLA30, "rec.trialTFLA30.csv", row.names = FALSE)
rec.trialTFLA30 <- read.csv("rec.trialTFLA30.csv")

####Set proportions Response 1 Time 1####
###Set up for loop to count number of species observed per trial using code above
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
head(pre.v.postTFLA30)
Spp.per.trial <- NULL
#Create vector of trials to run through
Unique.trials<-unique(pre.v.postTFLA30$Trial)
#Start for loop
for(i in Unique.trials){
  Trial<-subset(pre.v.postTFLA30, pre.v.postTFLA30$Trial==i) 
  Species.no<-length(unique(pre.v.postTFLA30$Spp[which(pre.v.postTFLA30$Trial==i & pre.v.postTFLA30$Pre.count>=1 | pre.v.postTFLA30$Post.count>=1 & pre.v.postTFLA30$Trial==i)]))
  Spp.per.trial<-c(Spp.per.trial, Species.no) 
}
#Create date frame of spp per trial #
Spp.per.trial<-data.frame(Unique.trials, Spp.per.trial)
#Change names
names(Spp.per.trial) <- c("Trial", "Spp.per.trial")
head(Spp.per.trial)

###Count number of spp responding in pre.v.post df using for loop
head(pre.v.postTFLA30)
Spp.rec.per.trial <- NULL

Unique.trials<-unique(pre.v.postTFLA30$Trial)

for(i in Unique.trials){
  Trial<-subset(pre.v.postTFLA30, Trial==i) #
  Species.no<-length(which(Trial$Recruit>=1))
  Spp.rec.per.trial<-c(Spp.rec.per.trial, Species.no)
}
#Create data frame of spp recruited per trial
Spp.rec.per.trial<-data.frame(Unique.trials, Spp.rec.per.trial)
#Change names
names(Spp.rec.per.trial) <- c("Trial", "Spp.rec.per.trial")

###Create data frame of just trial and treatment
head(rec.trialTFLA30)
Trial.trtmt <- subset(rec.trialTFLA30, select = c("Trial", "Treatment","Exemplar"))
head(Trial.trtmt)
###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial
install.packages("tidyverse")
library(tidyverse)
str(Spp.rec.per.trial)

####Merge data to get prop df####
Prop.trialTFLA30 <- merge(Trial.trtmt, c(Spp.per.trial, Spp.rec.per.trial), by="Trial", all = TRUE)
###Combine recruits per trial and number of species per trial to get proportion 
Prop.trialTFLA30$Prop.rec <- Prop.trialTFLA30$Spp.rec.per.trial/Prop.trialTFLA30$Spp.per.trial
###Check it out...
head(Prop.trialTFLA30)
###Replace NaNs with 0s
Prop.trialTFLA30$Prop.rec[is.nan(Prop.trialTFLA30$Prop.rec)]<-0 #There are no NAs

####Merge prop rec and rec Response 1 Time 1####
###Now need to add number of recruits per trial to this
#Using merge
TFLA.R1 <- merge(rec.trialTFLA30, Prop.trialTFLA30)
head(TFLA.R1)

#Subset variables of interest aka remove duplicate columns
TFLA.R1 <- TFLA.R1[-6]

###order new data set to add 
TFLA.R1 <- TFLA.R1[order(TFLA.R1$Trial),]

###Add trial location column
head(TFLA.R1)
TFLA.R1 <- merge(TFLAmaster, TFLA.R1, by="Trial")
###Order data frame
TFLA.R1 <- TFLA.R1[, c(1,2,3,4,9,5,6,7,8)]

####Write final Response 1 time 1 new csv/Read in####
write.csv(TFLA.R1, "TFLA.R1.csv", row.names = FALSE)
TFLA.R1 <- read.csv("TFLA.R1.csv")
head(TFLA.R1)

###Plot using ggplot

library(ggplot2)
library(wesanderson)
library(viridis)

prop.plotTFLA.R1 <- ggplot(TFLA.R1, aes(Treatment, Prop.rec)) + geom_boxplot(aes(fill=Treatment)) + theme_bw() +labs(title="Tierra Firme Response 1", x="Treatment", y="Proportion Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") +scale_fill_manual(values = wes_palette("Moonrise3", n = 5))
colors()

num.plotTFLA.R1 <- ggplot(TFLA.R1, aes(Treatment, Recruit)) + geom_boxplot(aes(fill=Treatment)) + theme_bw() +labs(title="Tierra Firme Response 1", x="Treatment", y="Number of Recruits") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="gold4", fill="gold4") + scale_fill_manual(values = wes_palette("Moonrise3", n = 5))


###Add Forest type column
TFLA.R1$Forest.type <- rep("TF")
head(TFLA.R1)

###Rename responses as response 1 to prepare for combination with Response 2
names(TFLA.R1)[4:7] <- c("R1.recruit", "R1.spp.per.trial", "R1.spp.rec.per.trial", "R1.prop.rec")
head(TFLA.R1)

####Write csv for TFLA R1####
write.csv(TFLA.R1, "TFLA.R1.csv", row.names = FALSE)
TFLA.R1 <- read.csv("TFLA.R1.csv")
head(TFLA.R1)

####Plot comb TFLA.R1####
p1TF <- ggplot(TFLA.R1, aes(x=Treatment, y=R1.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatmnt", y="Proportion of Recruits") + theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title = element_text(size=15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

p2TF <- ggplot(TFLA.R1, aes(x=Treatment, y=R1.recruit, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Number of Recruits") + theme_bw() +
  theme(axis.title = element_text(size = 15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")  


#library(ggpubr)
pcombTF <- ggarrange(p1TF, p2TF, 
          labels = c("A", "B"),
          ncol=1, nrow=2, 
          common.legend = TRUE, legend = "right")

###Add title to figure
annotate_figure(pcombTF, top = text_grob("Response 1 in Tierra Firme", 
                                        color = "black", face = "bold", size = 20))


###################################Response 2#####
####Select those that moved in closer from pre to post Response 2######
####Subset data frame for variables of interest####

safetyTFLA <- read.csv("23safetycueTFLA.csv")
safetyTFLA <- subset(safetyTFLA, select = c("Trial","Treatment", "Exemplar","Stage","Spp", "ID", "Distance"))
head(safetyTFLA)

####Subset dataframe for variables of interest Resp2####
#Want to say if pre distance is less than post distance...but think I need to aggregate
#so that spp are side by side here. might need to first put through master for loop 
#if that is the case then I need to somehow keep distance for each spp in master loop 
#Or do i....Can maybe just aggregate/merge from start...lets see

safetyTFLA.R2 <- safetyTFLA
head(safetyTFLA.R2)

####Split dataframe into pre and post####
safetyTFLA.R2.Pre <- safetyTFLA[which(safetyTFLA$Stage=="PRE"),] #1143 obs
#filter(safetyTFLA, Stage=="PRE") Can also do this way
head(safetyTFLA.R2.Pre)
safetyTFLA.R2.Post <- safetyTFLA[which(safetyTFLA$Stage=="POST"),] #1165 obs
head(safetyTFLA.R2.Post)

# each observation has a unique identifier for both stages
# can combine 
safetyTFLA.R2.comb <- merge(safetyTFLA.R2.Pre, safetyTFLA.R2.Post, by=c("ID", "Trial"), all.x = TRUE)
head(safetyTFLA.R2.comb) #1159 obs, by adding in all.x, we don't lose trials with no recruits

#Can double check with anti join but columns need to be the same, look later
anti_join()

#Remove duplicate columns
safetyTFLA.R2.comb <- subset(safetyTFLA.R2.comb, select = c("Trial", "Treatment.x", "Exemplar.x", "Spp.x", "ID", "Distance.x", "Distance.y"))

###Just need to remove any rows with NAs
#rename columns 
names(safetyTFLA.R2.comb) <- c("Trial", "Treatment", "Exemplar", "Spp", "ID", "Pre.Dist", "Post.Dist")
head(safetyTFLA.R2.comb)

###order new data set to add 
sapply(safetyTFLA.R2.comb, class)
safetyTFLA.R2.comb$Trial <- as.numeric(safetyTFLA.R2.comb$Trial)
safetyTFLA.R2.comb <- safetyTFLA.R2.comb[order(safetyTFLA.R2.comb$Trial),]
head(safetyTFLA.R2.comb)

#Add column to show if this counts as response 2 or not
safetyTFLA.R2.comb$R2.recruit <- ifelse(safetyTFLA.R2.comb$Pre.Dist > safetyTFLA.R2.comb$Post.Dist, 1,0)
head(safetyTFLA.R2.comb)
head(TFLA.R1)

####Remove conspecifics from Solo spp####
unique(safetyTFLA.R2.comb$Treatment)
head(safetyTFLA.R2.comb) #335 obs

#NF <- pre.v.postVALA30.1 %>% 
#filter(trtmt.grp=="NonFlock" ) #Don't need this part i dont think
minuscon.R2 <- safetyTFLA.R2.comb %>% 
  filter(Treatment=="NF" ) %>% filter(!Spp=="HEGRI" & !Spp== "LECOR"& !Spp== "MOMOM" & !Spp== "PLCOR" & !Spp== "LAHYP")

head(minuscon.R2)
sum(safetyTFLA.R2.comb$Spp=="HEGRI")
sum(minuscon.R2$Spp=="HEGRI")
#check total obsservations in nonflock
sum(safetyTFLA.R2.comb$Treatment=="NF")
length(minuscon.R2$Spp)

#Split df to contain all treatments but nonflock
minusNF.R2 <- subset(safetyTFLA.R2.comb, !Treatment=="NF")
#Check this removed nonflock treatment
sum(minusNF.R2$Treatment=="NF")

#ReCombine the nonflock minus conspeficis and all trtms but nonflock
safetyTFLA.R2.comb <- rbind(minuscon.R2, minusNF.R2)
head(safetyTFLA.R2.comb)
#Order this shit
safetyTFLA.R2.comb <- safetyTFLA.R2.comb %>% arrange(safetyTFLA.R2.comb$Trial)
head(safetyTFLA.R2.comb)

####Get R2 data set ready to combine with R1####
#Need to add up R2 recruits with aggregate function by trial
#Need to get proportion of responses by bringing in spp.per.trial from R1
#Columns in R1: Trial, treatments, trtmt.grp, R1.recruit, R1.spp.per.trial, R1.spp.rec.per.trial,
#R1.prop.rec, #Time, Forest.type

safetyTFLA.R2.comb$Forest.type <- rep("TF")
head(safetyTFLA.R2.comb)

####Get spp.per.trial and spp.rec.per.trial for prop rec
head(safetyTFLA)
safetyTFLA.R2.Pre <- as.data.frame(safetyTFLA.R2.Pre)
safetyTFLA.R2.Post <- as.data.frame(safetyTFLA.R2.Post)
safetyTFLA.R2 <- rbind(safetyTFLA.R2.Pre, safetyTFLA.R2.Post)
head(safetyTFLA.R2)
safetyTFLA.R2 <- safetyTFLA.R2[order(safetyTFLA.R2$Trial),]
head(safetyTFLA.R2)

###Set up for loop to count number of species observed per trial
#Need to create for loop to include trial and treatment for each line. 
#First create empty vector for total spp per trial and spp recruited per trial
spp.per.trial <- NULL
#Create vector of trials to run through
unique.trials<-unique(safetyTFLA.R2$Trial)
#Start for loop
for(i in unique.trials){
  trial<-subset(safetyTFLA.R2, Trial==i) 
  species.no<-length(unique(safetyTFLA.R2$Spp[which(safetyTFLA.R2$Trial==i)]))
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
trial.trtmt <- subset(safetyTFLA.R2, select = c("Trial","Treatment"))
trial.trtmt <- unique(trial.trtmt)

###Now need to combine trial.trtmt, spp.rec.per.trial, spp.per.trial

##Need to rename column time in trial.trtmt.time df
safetyTFLA.prop.R2 <- merge(trial.trtmt, c(spp.per.trial, spp.rec.per.trial), by="Trial")
head(safetyTFLA.prop.R2)

#Remove duplicate columns
safetyTFLA.prop.R2 <- safetyTFLA.prop.R2[,-4]

###Combine recruits per trial and number of species per trial to get proportion 
safetyTFLA.prop.R2$R2.prop.rec <- safetyTFLA.prop.R2$R2.spp.rec.per.trial/safetyTFLA.prop.R2$R2.spp.per.trial
###Check it out...
head(safetyTFLA.prop.R2)
#Good!

####Read in safetyTFLA.R2.comb here####
head(safetyTFLA.R2.comb)
write.csv(safetyTFLA.R2.comb, "safetyTFLA.R2.comb.csv", row.names = FALSE)

safetyTFLA.R2.comb <- read.csv("safetyTFLA.R2.comb.csv")

####Read in safetyTFLA.prop.R2 here####
head(safetyTFLA.prop.R2)
write.csv(safetyTFLA.prop.R2, "safetyTFLA.prop.R2.csv", row.names = FALSE)
safetyTFLA.prop.R2 <- read.csv("safetyTFLA.prop.R2.csv")
unique(safetyTFLA.prop.R2$Trial)
nrow(safetyTFLA.prop.R2)
####Add number of recruits to this safety TFLA.prop.R2####
#aggregate here
head(safetyTFLA.R2.comb)

##Replace NAs in R2 recruits with 0s
safetyTFLA.R2.comb$R2.recruit[is.na(safetyTFLA.R2.comb$R2.recruit)]<-0

###Sum by trial using aggregate function, then run through Alex code to add trtmt group
safetyTFLA.rec.R2 <- aggregate(R2.recruit~ Trial+Exemplar+Treatment, safetyTFLA.R2.comb, sum)
head(safetyTFLA.rec.R2)

###order new data set to add 
safetyTFLA.rec.R2$Trial <- as.numeric(safetyTFLA.rec.R2$Trial)
safetyTFLA.rec.R2 <- safetyTFLA.rec.R2[order(safetyTFLA.rec.R2$Trial),]
head(safetyTFLA.rec.R2)
nrow(safetyTFLA.rec.R2)

###Now combine data frames for prop and recs
#Using merge
TFLA.R2 <- merge(safetyTFLA.prop.R2, safetyTFLA.rec.R2, by=c("Trial", "Treatment"))
TFLA.R2$Trial <- as.numeric(TFLA.R2$Trial)
TFLA.R2 <- TFLA.R2[order(TFLA.R2$Trial),]

###Add Forest type column
TFLA.R2$Forest.type <- rep("TF")
head(TFLA.R2)
head(TFLA.R1)

###Add trial location column
head(TFLA.R2)
TFLA.R2 <- merge(TFLAmaster, TFLA.R2, by="Trial")
###Order data frame
TFLA.R2 <- TFLA.R2[, c(1,2,3,4,5,9,6,7,8)]

###Read in TFLA.R2 here####
write.csv(TFLA.R2, "TFLA.R2.csv", row.names = FALSE)
TFLA.R2 <- read.csv("TFLA.R2.csv")
head(TFLA.R2)


####Plot R2####
prop.plotTFLA.R2 <- ggplot(TFLA.R2, aes(Treatment, R2.prop.rec)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) + labs(x="Treatment", y="Proportion of Recruits") + theme_bw()+ 
  theme(axis.title.x = element_blank(), axis.title = element_text(size=15)) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

num.plotTFLA.R2 <- ggplot(TFLA.R2, aes(Treatment, R2.recruit)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + theme_bw() +
  theme(axis.title = element_text(size=15)) +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") 

#library(ggpubr)
pcombTF.R2 <- ggarrange(prop.plotTFLA.R2, num.plotTFLA.R2,
                       labels = c("C", "D"),
                       ncol=1, nrow=2, 
                       common.legend = TRUE, legend = "right")
pcombTF.R2

###Add title to figure
annotate_figure(pcombTF.R2, top = text_grob("Response 2 in Tierra Firme", 
                                           color = "black", face = "bold", size = 20))

#Combine responses on grid
pcombTFR1R2 <- ggarrange(prop.plotTFLA.R2, p1TF, num.plotTFLA.R2, p2TF,
                        labels = c("A", "B", "C", "D"),
                        ncol=2, nrow=4, 
                        common.legend = TRUE, legend = "right")

pcombTFR1R2


###Combine data frames for both responses####
head(TFLA.R1)
head(TFLA.R2)

####Merge R1 and R2 responses into one data frame####
TFLA.Rcomb <- merge(TFLA.R1, TFLA.R2, by= c("Trial", "Exemplar", "Treatment", "Forest.type"))
head(TFLA.Rcomb)

###Order data frame
TFLA.Rcomb <- TFLA.Rcomb[, c(1,3,2,4,5,6,7,8,9,10,11,12)]
TFLA.Rcomb$Trial <- as.numeric(TFLA.Rcomb$Trial)
TFLA.Rcomb <- arrange(TFLA.Rcomb, Trial)
#TFLA.Rcomb <- TFLA.Rcomb[order(TFLA.Rcomb$Trial),]

####Now combine R1 and R2 recruits####
TFLA.Rcomb$Total.Recs <- TFLA.Rcomb$R2.recruit+TFLA.Rcomb$R1.recruit

num.plotTFLAcomb <- ggplot(TFLA.Rcomb, aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw()+
  theme(axis.title = element_text(size = 15)) 

####Now combine R1 and R2 spp per trial, spp rec per trial, prop recs####
head(TFLA.Rcomb)
TFLA.Rcomb$Total.spp.trial <- TFLA.Rcomb$R2.spp.per.trial+TFLA.Rcomb$R1.spp.per.trial
TFLA.Rcomb$Total.spp.rec.trial <- TFLA.Rcomb$R2.spp.rec.per.trial+TFLA.Rcomb$R1.spp.rec.per.trial
TFLA.Rcomb$Total.prop.rec <- TFLA.Rcomb$Total.spp.rec.trial/TFLA.Rcomb$Total.spp.trial

###Order data frame
#TFLA.Rcomb <- TFLA.Rcomb[, c(1,2,3,4,5,6,7,15,8,9,16,17,12,13,14)]

###Add column for solo types
TFLA.Rcomb$Tr.Type <- gsub('[0-9.]', '', TFLA.Rcomb$Exemplar)

###Add trial location column
head(TFLA.Rcomb)
TFLA.Rcomb <- merge(TFLAmaster, TFLA.Rcomb, by="Trial")
###Order data frame
TFLA.Rcomb <- TFLA.Rcomb[, c(1,3,4,2,5,6,7,8,9,10,11,12,13,14,15,16,17)]

####Read TFLA.Rcomb in here####
write.csv(TFLA.Rcomb, "TFLA.Rcomb.csv", row.names = FALSE)
TFLA.Rcomb <- read.csv("TFLA.Rcomb.csv")
head(TFLA.Rcomb)
sapply(TFLA.Rcomb, class)
#reorder the groups order : I change the order of the factor data$names to order plot
TFLA.Rcomb$Tr.Type <- factor(TFLA.Rcomb$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

###Plot TFLA comb prop
prop.plotTFLAcomb <- ggplot(TFLA.Rcomb, aes(x=Treatment, y=Total.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") +
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) 

prop.plotTFLAcomb

pcombTFRcomb <- ggarrange(prop.plotTFLAcomb, num.plotTFLAcomb,
                         labels = c("A", "B"),
                         ncol=1, nrow=2, 
                         common.legend = TRUE, legend = "right")
pcombTFRcomb

###Add title to figure
annotate_figure(pcombTFRcomb, top = text_grob("Response 1 & 2 in Tierra Firme", 
                                             color = "black", face = "bold", size = 20))


###Plot response 1&2 by treatment type####
num.plotTF.type <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 15)) 

###Plot TFLA proportion response 1&2 by treatment type####
prop.plotTF.type <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="Proportion of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +theme_bw() +
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) 

pcombTFR.type <- ggarrange(prop.plotTF.type, num.plotTF.type,
                          labels = c("A", "B"),
                          ncol=1, nrow=2, 
                          legend = "none")
pcombTFR.type

###Add title to figure
annotate_figure(pcombTFR.type, top = text_grob("Response 1 & 2 in Tierra Firme", 
                                              color = "black", face = "bold", size = 20))


###Net Distance####
safetyTFLA.R2.comb <- read.csv("safetyTFLA.R2.comb.csv")
head(safetyTFLA.R2.comb)

#check class of variables
sapply(safetyTFLA.R2.comb,class)

#change pre.dist to numeric variable
safetyTFLA.R2.comb$Trial <- as.numeric(safetyTFLA.R2.comb$Trial)
safetyTFLA.R2.comb$Pre.Dist <- as.numeric(safetyTFLA.R2.comb$Pre.Dist)
safetyTFLA.R2.comb$Post.Dist <- as.numeric(safetyTFLA.R2.comb$Post.Dist)

#Remove spp not seen in both pre and post
net.dist.TFLA <- na.omit(safetyTFLA.R2.comb)
head(net.dist.TFLA)

#Post.Dist - Pre.Dist to get net dist change from pre to post for all individuals seen in both 
#negative number is away, positive number is toward
net.dist.TFLA$Net.Dist <- net.dist.TFLA$Pre.Dist - net.dist.TFLA$Post.Dist

###Add column for solo types
net.dist.TFLA$Tr.Type <- gsub('[0-9.]', '', net.dist.TFLA$Exemplar)

###Add column for trial location
net.dist.TFLA <- merge(TFLAmaster, net.dist.TFLA, by="Trial")
head(net.dist.TFLA)

###Order data frame
net.dist.TFLA <- net.dist.TFLA[, c(1,2,3,12,4,10,5,6,7,8,9,11)]


###Read net.dist.TFLA csv in here####
write.csv(net.dist.TFLA, "net.dist.TFLA23.csv", row.names = FALSE)
net.dist.TFLA <- read.csv("net.dist.TFLA23.csv")

###plot tfla net dist by treatment group####
P.net.dist.TF <- ggplot(net.dist.TFLA, aes(Treatment, Net.Dist)) + geom_boxplot(aes(fill=Treatment), show.legend = FALSE) +labs(x="Treatment", y="Net Dist Moved (m)") + theme_bw() +
  theme(axis.title = element_text(size=15)) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add title to figure
annotate_figure(P.net.dist.TF, top = text_grob("Net Distance Change in Tierra Firme", 
                                              color = "black", face = "bold", size = 20))

#Try violin plot
PV.net.dist.TF <- ggplot(net.dist.TFLA, aes(Treatment, Net.Dist)) + geom_violin(aes(fill=Treatment), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3)

###Add title to figure
annotate_figure(PV.net.dist.TF, top = text_grob("Net Distance Change in Tierra Firme", 
                                                color = "black", face = "bold", size = 20))

###TF plots net dist by treatment type####

#reorder the groups order : I change the order of the factor data$names to order plot
net.dist.TFLA$Tr.Type <- factor(net.dist.TFLA$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

P.net.dist.TF.type <- ggplot(net.dist.TFLA, aes(Tr.Type, Net.Dist)) + geom_boxplot(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black")

###Add title to figure
annotate_figure(P.net.dist.TF.type, top = text_grob("Net Distance Change in Tierra Firme", 
                                                color = "black", face = "bold", size = 20))


#Try violin plot
PV.net.dist.TF <- ggplot(net.dist.TFLA, aes(Tr.Type, Net.Dist)) + geom_violin(aes(fill=Tr.Type), show.legend = FALSE) + theme_bw() + theme(axis.title = element_text(size=15)) +
  labs(x="Treatment", y="Net Dist Moved (m)") + guides(fill=guide_legend(title="Treatment Group")) + stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3)

###Add title to figure
annotate_figure(PV.net.dist.TF, top = text_grob("Net Distance Change in Tierra Firme", 
                                                    color = "black", face = "bold", size = 20))


