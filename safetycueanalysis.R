#Chelsey Hunts
#19 September 2022
#Thesis work
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

#read in csv file and create object
safety<-read.csv("22safetycuePB.csv", header=TRUE)

#Quick look over
head(safety)

#See summary of current data frame
summary(safety)
#Need to remove date, observer, time.in.rec, audio or visual, notes, X
#Want to keep treatment, stage, spp, distance.
#Need to change distance to numeric from character

#subset data to include only trial, treatment, stage, spp, distance variables
#Rewrite over original safety data frame
safety <- subset(safety, select = c("trial", "treatment","stage","spp", "distance"))

#Now there are only 4 columns with info we want to look at
tail(safety)

#Need to change distance to numeric value
safety$distance <- as.numeric(as.character(safety$distance))

#Check to see if it is how we want
sapply(safety, class)
#Looks good, treatment, stage, and spp are characters, distance is numeric

#Now create two subsets of data
#sub1 will contain only species that appear in post only within 30m
#sub2 will contain delta distance data, delta distance pre-post

#$Subset 1--duplicate subset
#Start by separating each trial
t9<-subset(safety, safety$trial==9) 

#Then need to remove any repeats in pre and post stage
#Show birds that are only in post and not in pre and within 30m, so if there in both pre and post then they get deleted
unique(t9$spp, which(t9$stage=="PRE"))
unique(t9$spp, which(t9$stage=="POST"))
t7[!duplicated(t7$spp), ]
t7
#Separate by treatment, then pre and post
#Need to set up for loop 
#subset pre and post for each trial



safety$tts<-paste(safety$trial, safety$treatment, safety$stage, sep='_')

safety$ttss<-paste(safety$trial, safety$treatment, safety$stage, safety$spp, sep='_')

sub.id<-safety[which(safety$ttss=="7_FL2_POST_CECI"), ]

sub.id$id<-1:length(sub.id$ttss)

sub.id$spp.id<-paste(sub.id$spp, sub.id$id, sep='_')

#for loops to add in all spp to each trial pre and post
richness<-NULL
treatment<-NULL
for(i in unique(safety$treatment)){
  s1<-subset(safety, safety$treatment==i)
  s2<-subset(s1, s1$stage=="PRE")
  numspp<-dim(s2)[1]
  s3<-subset(s1, s1$stage=="POST")
  numspp2<-dim(s3)[1]
  richness<-rbind(richness, c(numspp, numspp2))
  treatment<-c(treatment, s3$treatment[1])
}

#PRoblem here is that CECI duplicate is being removed only in post..?
#count all post species that are left after removing duplicates within 30m IN POST

#Gives number of species responded per trial

#sum of all trials

#Now save only within post species observed within 30m 

##Subset 2 contains birds that are seen in both pre and post seen coming on from outside of 30m to within 30m

#sub1 spp only observed in post....how to...
?subset
subset(safety$trial["1TFLA"], safety$spp !=)
fish.mr$MO2[which(fish.mr$Species=="Goby")]

#sub2 subset using if/else statement, if pre distance is greater than 30 AND post is 30 or less, include
sub2<- safety$spp[which(safety$distance<=30 & safety$distance>30)]



safety$trial[which(safety$distance>30)] 
safety$trial[which(safety$distance>30)] 

subset(safety$stage["PRE"][which(safety$spp)
                           safety$distance>30)

unique(safety$treatment) 

#for loops to add in all spp to each trial pre and post
richness<-NULL
treatment<-NULL
for(i in unique(safety$treatment)){
  s1<-subset(safety, safety$treatment==i)
  s2<-subset(s1, s1$stage=="PRE")
  numspp<-dim(s2)[1]
  s3<-subset(s1, s1$stage=="POST")
  numspp2<-dim(s3)[1]
  richness<-rbind(richness, c(numspp, numspp2))
  treatment<-c(treatment, s3$treatment[1])
}
richness
locations

summary.data<-as.data.frame(cbind(treatment, richness))
names(summary.data)=c("treatment", "pre", "post")
summary.data
#third column for how many spp in post are unique
#

#Fake prelim expected results
#Create data frame and table of observed Prairie Dog births and non-births per mating
Spresp.<-c(5, 6, 4, 3, 7, 2, 5, 2, 3, 2, 1, 1, 3, 2, 1) #creates vector of all data in table
rnames<-c(1:5)
cnames<- c("Flock", "nonFlock", "Control") #names columns
#Create table using matrix fxn. Give matrix fxn -- dataframe, want organized by row, how many rows, how many columns, 
#dimension names come from previously made objects above
SppResponse<-matrix(Spresp., byrow = FALSE, nrow = 5, ncol=3, dimnames= list(rnames, cnames))


boxplot(SppResponse, xlab = "Treatment", 
        ylab = "# Species Response", col = "thistle4")

#Proprtion response by degradation level
Spresp.<-c(5, 6, 4, 3, 7, 4, 5, 4, 3, 2, 1, 1, 3, 2, 1) #creates vector of all data in table
pr.resp<-Spresp./15
rnames<-c(1:5)
cnames<- c("Low", "Med", "High") #names columns
#Create table using matrix fxn. Give matrix fxn -- dataframe, want organized by row, how many rows, how many columns, 
#dimension names come from previously made objects above
SppResponse<-matrix(pr.resp, byrow = FALSE, nrow = 5, ncol=3, dimnames= list(rnames, cnames))


boxplot(SppResponse, xlab = "Treatment", 
        ylab = "Proportion Species Response", col = "thistle4")

#Proprtion response by forest type
Spresp.<-c(5, 6, 4, 3, 7, 1, 5, 3, 2, 4) #creates vector of all data in table
pr.resp<-Spresp./15
rnames<-c(1:5)
cnames<- c("Tierra Firme", "Varzea") #names columns
#Create table using matrix fxn. Give matrix fxn -- dataframe, want organized by row, how many rows, how many columns, 
#dimension names come from previously made objects above
SppResponse<-matrix(pr.resp, byrow = FALSE, nrow = 5, ncol=2, dimnames= list(rnames, cnames))


boxplot(SppResponse, xlab = "Treatment", 
        ylab = "Proportion Species Response", col = "thistle4")
