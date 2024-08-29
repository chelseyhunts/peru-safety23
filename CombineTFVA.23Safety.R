##################Code created by Chelsey October 31, 2023

# Preparing data for analysis -- combining data frames from both forest types
# Exploring data with figures


# Clear the environment
rm(list = ls())

# Install and load the packages
install.packages("tidyverse")
library(tidyverse) #used for tidying data and plotting

# Setup Data --------------------------------------------------------------

###Combine plots for side by side of no and prop recruits for both forest types
#First need to remove axis titles

P1 <- prop.plotTFLAcomb + theme(axis.title.x = element_blank())
P2 <- ggplot(subset(VLA.Rcomb,Treatment%in% c("MSF","CTRL","NF")),
             aes(x=Treatment, y=Total.prop.rec, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y="") + theme_bw() +
  theme(axis.title.x = element_blank(), axis.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Paired")+
  guides(fill=guide_legend(title="Treatment Group")) + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") 
N1 <- ggplot(subset(TFLA.Rcomb, Treatment %in% c("MSF", "CTRL", "NF")), 
             aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Tierra Firme", y="No. Recruits") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  scale_fill_brewer(palette = "Paired") +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw()+
  theme(axis.title = element_text(size = 20))+
  ylim(NA, 12.5)
N2 <- ggplot(subset(VLA.Rcomb,Treatment%in% c("MSF","CTRL","NF")), aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Varzea",y="") + 
  guides(fill=guide_legend(title="Treatment Group")) + 
  scale_fill_brewer(palette = "Paired")+
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size=20)) +
  ylim(NA,12.5)

pcombRcomb <- ggarrange(P1, P2, N1, N2,
                         labels = c("A","B","C", "D"),
                         ncol=2, nrow=2, 
                         common.legend = TRUE, legend = "right")
pcombRcomb

###Plot TFLA response 1&2 by treatment type####
N3 <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Tierra Firme", y="No. Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 16)

###Plot TFLA proportion response 1&2 by treatment type####
P3 <- ggplot(TFLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x= "", y="Proportion Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") +theme_bw() +
  theme(axis.title = element_text(size = 20), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.7)

pcombTFR.type <- ggarrange(P3, N3,
                           labels = c("A", "B"),
                           ncol=1, nrow=2, 
                           legend = "none")
pcombTFR.type

###Plot VLA response 1&2 by treatment type####
N4 <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Varzea", y="") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 16)

###Plot VLA proportion response 1&2 by treatment type####
P4 <- ggplot(VLA.Rcomb, aes(x=Tr.Type, y=Total.prop.rec, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x= "", y="") + 
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="black", fill="black") + theme_bw() +
  theme(axis.title = element_text(size = 20), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.7)

pcombVR.type <- ggarrange(prop.plotV.type, num.plotV.type,
                          labels = c("C", "D"),
                          ncol=1, nrow=2, 
                          legend = "none")
pcombVR.type

pcombRtypecomb <- ggarrange(P3, P4, N3, N4,
                        labels = c("A","B","C", "D"),
                        ncol=2, nrow=2, 
                        common.legend = TRUE, legend = "right")
pcombRtypecomb


VLA.Rcomb <- read.csv("VLA.Rcomb.csv", stringsAsFactors = TRUE)

###Plot with mean and standard errors instead of boxplots TFLA####
# Use dplyr to group by 'Group' and calculate mean and standard error
head(TFLA.Rcomb)
summary_statsTFLA <- TFLA.Rcomb %>%
  group_by(Treatment, Tr.Type) %>%
  summarize(
    Mean = mean(Total.Recs),
    SD = sd(Total.Recs),
    SE = sd(Total.Recs) / sqrt(n())
  )

head(TFLA.Rcomb)
summary_stats.prTFLA <- TFLA.Rcomb %>%
  group_by(Treatment, Tr.Type) %>%
  summarize(
    Mean = mean(Total.prop.rec),
    SD = sd(Total.prop.rec),
    SE = sd(Total.prop.rec) / sqrt(n())
  )

#reorder the groups order : I change the order of the factor data$names to order plot
summary_statsTFLA <- summary_statsTFLA %>%
  mutate(Tr.Type = fct_relevel(Tr.Type, "CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

summary_stats.prTFLA <- summary_stats.prTFLA %>%
  mutate(Tr.Type = fct_relevel(Tr.Type, "CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

MeanNoTFLA <- ggplot(summary_statsTFLA, aes(Tr.Type, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="Tierra Firme", y="Mean No. Recruits")+
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 8)

MeanPrTFLA <- ggplot(summary_stats.prTFLA, aes(Tr.Type, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="", y="Mean Proportion Recruits")+
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.4)


###Plot with mean and standard errors instead of boxplots VLA####
# Use dplyr to group by 'Group' and calculate mean and standard error
head(VLA.Rcomb)
summary_statsVLA <- VLA.Rcomb %>%
  group_by(Treatment, Tr.Type) %>%
  summarize(
    Mean = mean(Total.Recs),
    SD = sd(Total.Recs),
    SE = sd(Total.Recs) / sqrt(n())
  )

head(VLA.Rcomb)
summary_stats.prVLA <- VLA.Rcomb %>%
  group_by(Treatment, Tr.Type) %>%
  summarize(
    Mean = mean(Total.prop.rec),
    SD = sd(Total.prop.rec),
    SE = sd(Total.prop.rec) / sqrt(n())
  )

#reorder the groups order : I change the order of the factor data$names to order plot
summary_statsVLA$Tr.Type <- factor(summary_statsVLA$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

summary_stats.prVLA <- summary_stats.prVLA %>%
  mutate(Tr.Type = fct_relevel(Tr.Type, "CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

MeanNoVLA <- ggplot(summary_statsVLA, aes(Tr.Type, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="Varzea", y="")+
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
        axis.text.y = element_text(size = 15)) + ylim(NA, 8)

MeanPrVLA <- ggplot(summary_stats.prVLA, aes(Tr.Type, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="", y="")+
  theme(axis.title = element_text(size = 15), axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=15)) + ylim(NA, 0.4)

####Plot all means together comparing TF and VA mean numbers and proportions###
P.Means.All <- ggarrange(MeanPrTFLA, MeanPrVLA,MeanNoTFLA, MeanNoVLA, 
                            labels = c("A","B","C", "D"),
                            ncol=2, nrow=2, 
                            common.legend = TRUE, legend = "right")
P.Means.All


# Combine R1 and R2 for TF and VA -----------------------------------------

###Response 1 & 2 Combine####
head(TFLA.Rcomb)
TFLA.Rcomb <- read.csv("TFLA.Rcomb.csv")
head(VLA.Rcomb)
VLA.Rcomb <- read.csv("VLA.Rcomb.csv")
Safety23 <- rbind(TFLA.Rcomb, VLA.Rcomb)
head(Safety23)
unique(Safety23$Treatment)
write.csv(Safety23, "Safety23.csv", row.names = FALSE)
Safety23 <- read.csv("Safety23.csv", header = TRUE, stringsAsFactors = TRUE)
glimpse(Safety23)

#reorder the groups order : I change the order of the factor data$names to order plot
Safety23$Tr.Type <- factor(Safety23$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

####Plot all number combined response 1&2 by treatment####
num.plot.all <- ggplot(Safety23, aes(x=Treatment, y=Total.Recs, fill=Treatment)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15))

####Plot all proportion response 1&2 by treatment type####
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

####Plot all number combined response 1&2 by treatment type####
num.plot.all.type <- ggplot(Safety23, aes(x=Tr.Type, y=Total.Recs, fill=Tr.Type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Number of Recruits") + 
  stat_summary(fun=mean, geom="point", shape=15, size=4, color="black", fill="black") +
  theme_bw() +
  theme(axis.title = element_text(size = 15)) 

####Plot all proportion combined response 1&2 by treatment type####
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


####Plot combined data with mean and standard errors instead of boxplots####
# Use dplyr to group by 'Group' and calculate mean and standard error
head(Safety23)
summary_stats <- Safety23 %>%
  group_by(Treatment, Tr.Type) %>%
  summarize(
    Mean = mean(Total.Recs),
    SD = sd(Total.Recs),
    SE = sd(Total.Recs) / sqrt(n())
  )

#reorder the groups order : I change the order of the factor data$names to order plot
summary_stats$Tr.Type <- factor(summary_stats$Tr.Type , levels=c("CTRL", "MSF", "HARU", "MYLO", "MYSC", "THAR", "TUOC", "NF", "MOMO", "LAHY", "LECO", "PLCO", "HEGR"))

ggplot(summary_stats, aes(Tr.Type, Mean, colour=Treatment))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) +
  geom_point() + theme_bw() + labs(x="Treatment", y="Mean No. Recruits")


# Combine Net Distance for TF and VA --------------------------------------

###Net dist combine####
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


####plot net dist each treatment combined forest type####
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

####plot net dist each treatment type####
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

