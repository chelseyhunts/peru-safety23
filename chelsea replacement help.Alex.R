

pre.v.postTFLA1$trtmt.grp <- substr(pre.v.postTFLA1$treatments, 1, 1)
tail(pre.v.postTFLA1)

# create vector
trmt.grp <- pre.v.postTFLA1$trtmt.grp

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

pre.v.postTFLA1$trtmt.grp <- new

unique(pre.v.postTFLA1$trtmt.grp)
###Adjust recruit per trial dataframe


recruit.per.trial$trtmt.grp <- substr(recruit.per.trial$treatment, 1, 1)
tail(recruit.per.trial)

# create vector
trmt.grp <- recruit.per.trial$trtmt.grp

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

recruit.per.trial$trtmt.grp <- new
head(recruit.per.trial)


###Adjust rec.trialTFLA1 dataframe


rec.trialTFLA1$trtmt.grp <- substr(rec.trialTFLA1$treatment, 1, 1)
tail(rec.trialTFLA1)

# create vector
trmt.grp <- rec.trialTFLA1$trtmt.grp

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

rec.trialTFLA1$trtmt.grp <- new
head(rec.trialTFLA1)

