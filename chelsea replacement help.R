



# create vector
test <- c("FFF", "FFF", "FFF", "CL2", "CL2", "GG1", "GG1")

# replacement vector
new <- NULL

# start for loop, using 1:length(test) lets us index the values for replacement; 
# take the 1st spot and put in "fff", second spot = "fff" and so on. 
for (i in 1:length(test)) {
  
  # if the first value is "FFF" then take the ith value and pop in "fff"
  # OR IF FALSE (else) MOVE ON,
  if (test[i] == "FFF") {new[i] <- "fff"} else 
    
    # if the ith value is "CL2" then take ith of "new" object and pop in "cl2"
    # OR, IF STILL FALSE, MOVE ON (else)
    if (test[i] == "CL2") {new[i] <- "cl2"} else 
      
      # finally, if ith value of test is "GG1", then take ith value of new and 
      # put in "gg1". 
      if (test[i] == "GG1") {new[i] <- "gg1"} 
  
  # you don't need a final "else" here because all options are consumed by the above if/else conditions. if there are options leftover, let's say "test" ended with "CCC" you'd get an error with this code, but that's okay because you'd want to be alerted to the fact that there's a value you haven't told R what to do with (in this case "CCC").
  
}

# output should be all lowercase now,
# then you can just "$" it into the database! 
new

