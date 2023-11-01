####Build loops to keep all data required for modeling later
head(safetyTFLA30)

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

###Create vectors of pre and post counts to the find response totals
Prevec <- as.numeric(pre.v.postTFLA30$Pre.count)
Postvec <- as.numeric(pre.v.postTFLA30$Post.count)
pre.v.postTFLA30$Recruit <- ifelse(pre.v.postTFLA30$Pre.count >= pre.v.postTFLA30$Post.count, 0, Postvec-Prevec)
head(pre.v.postTFLA30)
sapply(pre.v.postTFLA30, class)
pre.v.postTFLA30$Trial <- as.integer(pre.v.postTFLA30$Trial)
