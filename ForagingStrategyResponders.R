#Foraging Strategies Responders to THAR and MYLO

#This code is to run Fisher's Exact test to test whether foraging strategies of responders to significant treatments MYLO and THAR mirror those of the playback species, or are no different than expected based on the available community. 

# Load Libraries ----------------------------------------------------------

library(tidyverse)

# Load dataframes ---------------------------------------------------------

for.stgy <- read.csv("Responders_Foraging_Ecology.csv", header = TRUE)
head(for.stgy)
for.stgy <- for.stgy %>% rename(Maneuver = Manuever)
unique(for.stgy$Maneuver)

# Trim leading and trailing spaces in the Maneuver column
for.stgy$Maneuver <- str_trim(for.stgy$Maneuver)

nrow(for.stgy) #59


THAR.man <- read.csv("THAR_Responders_Foraging_Ecology.csv")
head(THAR.man)
nrow(THAR.man) #45
MYLO.man <- read.csv("MYLO_Responders_Foraging_Ecology.csv")
head(MYLO.man)
nrow(MYLO.man) #35


# Combine foraging maneuver with spp specific dfs -------------------------

THAR.man <- THAR.man %>%
  left_join(for.stgy, by = "Spp") %>%
  select(Spp, Maneuver= Maneuver.y)

MYLO.man <- MYLO.man %>%
  left_join(for.stgy, by = "Spp") %>%
  select(Spp, Maneuver = Maneuver.y)


# Chi-Squared Goodness of Fit Test ----------------------------------------
#Hypothesis: Null -- Spp respond equally across all foraging strategies
#Alternative -- Spp do not respond equally across all foraging strategies
#Ecological significance of hyps: 

#For THAR 
unique(THAR.man$Maneuver)
sum(THAR.man$Maneuver=="Sally", na.rm = TRUE) #8
#pr.sally <- 8/45
sum(THAR.man$Maneuver=="Sally-hover", na.rm = TRUE) #7
#pr.salgl <- 7/45
sum(THAR.man$Maneuver=="Perch-Glean", na.rm = TRUE)  #1
#pr.pergl <- 1/45
sum(THAR.man$Maneuver=="Glean", na.rm = TRUE) #16

sum(THAR.man$Maneuver=="Sally-Pounce", na.rm = TRUE) #1
sum(THAR.man$Manuever=="Probe", na.rm = TRUE) #0
sum(THAR.man$Maneuver=="Ground", na.rm = TRUE) #5
sum(THAR.man$Maneuver=="Pounce", na.rm = TRUE) #1
sum(THAR.man$Maneuver=="Hover", na.rm = TRUE) #0

obs1 <- 8+7
obs2 <- nrow(THAR.man)-obs1
observed <- c(obs1, obs2)

exp1 <- nrow(THAR.man)/7 #7 different categories if combining sally and sally hover
exp2 <- exp1 * 6
expected <- c(exp1, exp2)

#perform chi-sq goodness of fit test
chisq_test <- chisq.test(x = observed, p = expected/sum(expected))
print(chisq_test)

#For MYLO 
unique(MYLO.man$Maneuver)
sum(MYLO.man$Maneuver=="Sally", na.rm = TRUE) #3
sum(MYLO.man$Maneuver=="Sally-hover", na.rm = TRUE) #7
sum(MYLO.man$Maneuver=="Perch-glean", na.rm = TRUE)  #1
sum(MYLO.man$Maneuver=="Glean", na.rm = TRUE) #11
sum(MYLO.man$Maneuver=="Sally-pounce", na.rm = TRUE) #3
sum(MYLO.man$Manuever=="Probe", na.rm = TRUE) #0
sum(MYLO.man$Maneuver=="Ground", na.rm = TRUE) #5
sum(MYLO.man$Maneuver=="Pounce", na.rm = TRUE) #1
sum(MYLO.man$Maneuver=="Hover", na.rm = TRUE) #1

obs1 <- 11
obs2 <- nrow(MYLO.man)-obs1
observed <- c(obs1, obs2)

exp1 <- nrow(MYLO.man)/8 #8 different categories
exp2 <- exp1 * 7
expected <- c(exp1, exp2)
expected2 <- c(5,30)

#perform chi-sq goodness of fit test
chisq_test <- chisq.test(x = observed, p = expected2/sum(expected2))
print(chisq_test)



# THAR Fisher Exact ---------------------------------------------------

THAR.spp.det <- read.csv("THAR_Spp_Detected_Foraging_Ecology.csv")
# Combine foraging maneuver with spp specific dfs -------------------------
head(THAR.spp.det)
unique(THAR.spp.det$Maneuver)

# Create new column for number that did not respond for each species

THAR.spp.det <- THAR.spp.det %>%
  rename(responded = total_recruit, total_obs = count) %>% 
  mutate(dn_respond = total_obs - responded) 
head(THAR.spp.det)

# Summarize by Maneuver
maneuver_summary <- THAR.spp.det %>%
  group_by(Maneuver) %>%
  summarise(
    Responded = sum(responded, na.rm = TRUE),
    Did_not_Respond = sum(dn_respond, na.rm = TRUE),
    Total_obs = sum(total_obs, na.rm = TRUE)
  )

print(maneuver_summary)

# Calculate proportions of each maneuver type
maneuver_summary <- maneuver_summary %>%
  mutate(
    total_obs_sum = sum(Total_obs),  # Total observed sum for the community
    proportion = Total_obs / total_obs_sum  # Proportion of each maneuver type
  )


#Prepare data frame for analysis:
#Fisher's Exact Test
#H0 = observed responses are consistent with the expected responses based on community proportions
#HA = observed responses are different than the expected responses based on community proportions

#Table set up:
#row1 = responded
#row2 = did not respond

#col1 = # sally/sallyhover/sallyglean that responded or did not respond
#col2 = # all others that responded or did not respond

# calculate observed for sally group
sally_group <- c("Sally", "Sally-glean", "Sally-hover")
observed_sally <- sum(maneuver_summary$Responded[maneuver_summary$Maneuver %in% sally_group])
observed_non_sally <- sum(maneuver_summary$Responded) - observed_sally

# calculate expected for sally maneuvers

expected_sally <- sum(maneuver_summary$proportion[maneuver_summary$Maneuver %in% sally_group]) * sum(maneuver_summary$Responded)
expected_non_sally <- sum(maneuver_summary$Responded) - expected_sally

# Create table for fisher test
fisher_table <- matrix(
  c(observed_sally, observed_non_sally,
    sum(maneuver_summary$Did_not_Respond[maneuver_summary$Maneuver %in% sally_group]),
    sum(maneuver_summary$Did_not_Respond) - sum(maneuver_summary$Did_not_Respond[maneuver_summary$Maneuver %in% sally_group])),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Responded", "Did_not_Respond"), c("Sally", "Non_Sally"))
)

fisher_test <- fisher.test(fisher_table)
print(fisher_test)


# Visualize THAR results --------------------------------------------------

# Plot mosaic
mosaicplot(fisher_table, color = c("skyblue", "salmon"),
           main = "Mosaic Plot of Foraging Strategies",
           sub = "Fisher’s p = 0.79 (No significant difference)")

# Convert data into long format for ggplot2
df_long <- maneuver_summary %>%
  rename("Did not Respond" = Did_not_Respond) %>% 
  pivot_longer(cols = c(Responded, "Did not Respond"),
               names_to = "Response",
               values_to = "Count") 

# Create the bar plot
ggplot(df_long, aes(x = Maneuver, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked proportions
  scale_y_continuous(labels = scales::percent) +  # Convert to percentage scale  
  scale_fill_manual(values = c("Responded" = "#0072B2", "Did not Respond" = "#009E73")) +  # Blue & Green
  labs(x = "Foraging Maneuver", y = "Proportion",
       fill = "Response Type") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank())


# MYLO Fisher Exact Test --------------------------------------------------

MYLO.spp.det <- read.csv("MYLO_Spp_Detected_Foraging_Ecology.csv")
head(MYLO.spp.det)
MYLO.spp.det <- MYLO.spp.det %>%
  left_join(THAR.spp.det, by = "Spp") %>%
  select(Spp, responded = total_recruit, total_obs = count, Maneuver= Maneuver.y) %>% 
  mutate(dn_respond = total_obs - responded)

unique(MYLO.spp.det$Maneuver)
head(MYLO.spp.det)

# Summarize by Maneuver
maneuver_summary <- MYLO.spp.det %>%
  group_by(Maneuver) %>%
  summarise(
    Responded = sum(responded, na.rm = TRUE),
    Did_not_Respond = sum(dn_respond, na.rm = TRUE),
    Total_obs = sum(total_obs, na.rm = TRUE)
  )

print(maneuver_summary)

# Calculate proportions of each maneuver type
maneuver_summary <- maneuver_summary %>%
  mutate(
    total_obs_sum = sum(Total_obs),  # Total observed sum for the community
    proportion = Total_obs / total_obs_sum  # Proportion of each maneuver type
  )


#Prepare data frame for analysis:
#Fisher's Exact Test
#H0 = observed responses are consistent with the expected responses based on community proportions
#HA = observed responses are different than the expected responses based on community proportions

#Table set up:
#row1 = responded
#row2 = did not respond

#col1 = # glean that responded or did not respond
#col2 = # all others that responded or did not respond

# calculate observed for sally group
glean_group <- "Glean"
observed_glean <- sum(maneuver_summary$Responded[maneuver_summary$Maneuver %in% glean_group])
observed_non_glean <- sum(maneuver_summary$Responded) - observed_glean

# calculate expected for sally maneuvers

expected_glean <- sum(maneuver_summary$proportion[maneuver_summary$Maneuver %in% glean_group]) * sum(maneuver_summary$Responded)
expected_non_glean <- sum(maneuver_summary$Responded) - expected_glean

# Create table for fisher test
fisher_table <- matrix(
  c(observed_glean, observed_non_glean,
    sum(maneuver_summary$Did_not_Respond[maneuver_summary$Maneuver %in% glean_group]),
    sum(maneuver_summary$Did_not_Respond) - sum(maneuver_summary$Did_not_Respond[maneuver_summary$Maneuver %in% glean_group])),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Responded", "Did_not_Respond"), c("Glean", "Non_Glean"))
)

fisher_test <- fisher.test(fisher_table)
print(fisher_test)


# Visualize MYLO fishers --------------------------------------------------

# Convert data into long format for ggplot2
df_long <- maneuver_summary %>%
  rename("Did not Respond" = Did_not_Respond) %>% 
  pivot_longer(cols = c(Responded, "Did not Respond"),
               names_to = "Response",
               values_to = "Count") 

# Create the bar plot
ggplot(df_long, aes(x = Maneuver, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked proportions
  scale_y_continuous(labels = scales::percent) +  # Convert to percentage scale  
  scale_fill_manual(values = c("Responded" = "#0072B2", "Did not Respond" = "#009E73")) +  # Blue & Green
  labs(x = "Foraging Maneuver", y = "Proportion",
       fill = "Response Type") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank())
