#Chelsey Hunts
#19 September 2022
#Thesis work -- TFLA
#Getting 22 summer field data in, tidy it up
#Make a box and whisker plot
#upload safety data frame > make new data frame with only variables trial, treatment, stage, spp, distance >
# > make distance numerical > separate trials >eliminate duplicates in pre and post
#in single trial > do this for each trial > separate treatments > combine FL and SS and Silent treatments  >
# >find difference in spp richness pre to post > 

# Clear R's brain
rm(list = ls()) 

#Set working directory
setwd("~/Documents/CSULB Documents/Thesis Work/Data/")

#read in TFLA csv file and create object
safetyTFLA<-read.csv("22safetycueTFLA.csv", header=TRUE)

#Quick look over
head(safetyTFLA)

#See summary of current data frame
summary(safetyTFLA)
#Need to remove date, observer, time.in.rec, audio or visual, notes, X
#Want to keep trial, treatment, stage, spp, distance, time
#Need to change distance to numeric from character

#subset data to include only trial, treatment, stage, spp, distance variables
#Rewrite over original safety data frame
safetyTFLA <- subset(safetyTFLA, select = c("trial", "time", "treatment","stage","spp", "distance"))

#Now there are only 5 columns with info we want to look at
tail(safetyTFLA)

#Need to change distance to numeric value
safetyTFLA$distance <- as.numeric(as.character(safetyTFLA$distance))
as.
#Change time to time value:
help("DateTimeClasses")
?strptime
?format
safetyTFLA$time <- format(as.POSIXct(safetyTFLA$time), format="%H:%M")
safetyTFLA$time <- strptime(safetyTFLA$time,format= "%H:%M")
?strptime
myData$time <- strptime(myData$time,"%Y-%m-%d %H:%M:%S")
#Check to see if it is how we want
sapply(safetyTFLA, class)
#Looks good, treatment, stage, and spp are characters, distance is numeric

spp.list<-unique(safetyTFLA$spp)

treatment.list<-sort(unique(safetyTFLA$treatment))
time.list <-safetyTFLA$time

treatment<-NULL
counts<- NULL
species<-NULL
trials<-NULL

for(i in treatment.list){
  s1<-subset(safetTFLA, safetyTFLA$treatment == i) #i=treatment
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
pre.v.post<-as.data.frame(cbind(trials, treatment, species, counts))
names(pre.v.post)<-c("trial", "treatments", "species", "Pre.count", "Post.count")
head(pre.v.post)
write.csv(pre.v.post, "pre.v.post.csv")
?write.csv

pre.v.post <- read.csv("pre.v.post.csv", header=T)
unique(pre.v.post$treatments)

pre.v.post
prevec <- as.numeric(pre.v.post$Pre.count)
postvec <- as.numeric(pre.v.post$Post.count)
pre.v.post$recruit <- ifelse(pre.v.post$Pre.count >= pre.v.post$Post.count, 0, postvec-prevec)
write.csv(pre.v.post, "pre.v.post.csv")

head(pre.v.post)
unique(pre.v.post$treatments)
###TRIAL
#create for loop to add treatment group for each treatment
pre.v.post$t.group <- gsub("[12345]", "", as.character(pre.v.post$treatments))
pre.v.post$treatments[which(pre.v.post$treatments=="SILENT")] <- "CONTROL"

pre.v.post$trial[which(pre.v.post$treatments=="CONTROL")]


trial.total <- NULL


for(i in unique(pre.v.post$trial)){
  trial.total <- as.numeric(c(trial.total, sum(pre.v.post$recruit[which(pre.v.post$trial== i)])))
  
}

names(trial.total) <- unique(pre.v.post$trial)

per.trial <- as.data.frame(cbind(unique(pre.v.post$trial), trial.total))
per.trial$trial.total <- as.numeric(as.character(per.trial$trial.total))
summary(per.trial)

per.trial$Treatment <- 

boxplot(recruit.treatment$recruit.total~recruit.treatment$Treatment, xlab = "Treatment Group", 
        ylab = "Total Recruits", col = "thistle4")




sum(pre.v.post$recruit[which(pre.v.post$trial=="3")])
#####END TRIAL
#create for loop to sum total recruitment for each treatment
recruit.total <- NULL

for(i in unique(pre.v.post$treatments)){
  recruit.total <- as.numeric(c(recruit.total, sum(pre.v.post$recruit[which(pre.v.post$treatments== i)])))
  
}
names(recruit.total) <- unique(pre.v.post$treatments)

recruit.treatment <- as.data.frame(cbind(unique(pre.v.post$treatments), recruit.total))
recruit.treatment$recruit.total <- as.numeric(as.character(recruit.treatment$recruit.total))
summary(recruit.treatment)

recruit.treatment$Treatment <- c("FL", "FL", "FL", "FL", "FL", "C", "C", "C", "C", "C", "NF", "NF", "NF", "NF", "NF")

boxplot(recruit.treatment$recruit.total~recruit.treatment$Treatment, xlab = "Treatment Group", 
        ylab = "Total Response", col = "thistle4")

write.csv(recruit.treatment, "recruit.treatment.csv")

Treatment.totals <- c(sum(recruit.treatment$recruit.total[1:5]), recruit.treatment$recruit.total[6:10],
                                 sum(recruit.treatment$recruit.total[11:15]))
Treatment.names <- c("FL", "Control", "NF")

#Check results of for loop:
unique(pre.v.post$treatments)
sum(pre.v.post$recruit[which(pre.v.post$treatments=="FL1")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="FL2")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="FL3")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="FL4")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="FL5")])
10+16+20+21+25 #92

sum(pre.v.post$recruit[which(pre.v.post$treatments=="SS1")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="SS2")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="SS3")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="SS4")])
sum(pre.v.post$recruit[which(pre.v.post$treatments=="SS5")])
12+19+22+10+12 #75

sum(pre.v.post$recruit[which(pre.v.post$treatments=="SILENT")])
#69

summary(recruit.treatment)
as.numeric(recruit.treatment$recruit.total)
#treatment>trial>pre/post trial>add spp list>presence/not (0/1)>pre only=0, pre/post=1, post=1
is.numeric(recruit.treatment$recruit.total)
  
#If the pre is >30m and post is <=30m = 1 
#If the pre is >30m and post is >30m = 0  
if(dim(s3)[1] == 0){deltad<-c(0,NA)} else{deltad<-c(s3$distance[which(s3$stage=="PRE" & s3$distance >30)], 
                                                    s3$distance[which(s3$stage=="POST" & s3$distance <=30)])}

#Compare FL responses between varzea and tf
recruit.treatmentTFLA <- read.csv("recruit.treatmentTFLA.csv", header=T)
recruit.treatmentVALA <- read.csv("recruit.treatmentVALA.csv", header=T)

FL.TF <- recruit.treatmentTFLA$recruit.total[which(recruit.treatmentTFLA$Treatment=="FL")]
FL.VA <- recruit.treatmentVALA$total.responses[which(recruit.treatmentTFLA$Treatment=="FL")]
TF.VA <- c(FL.TF, FL.VA)
Treatment <- c("TF","TF","TF","TF","TF","VA","VA","VA","VA","VA")
TF.VA <- data.frame(TF.VA, Treatment)
#Rename treatment column
names(TF.VA)[names(TF.VA) == "TF.VA"] <- "Responses"

TF.VA <- subset(TF.VA, select=c("Responses", "Treatment"))
TF.VA$Responses <- as.numeric(as.character(TF.VA$Responses))

boxplot(TF.VA$Responses~TF.VA$Treatment, xlab = "Forest Type", 
        ylab = "Total FL Response", col = "thistle4")
