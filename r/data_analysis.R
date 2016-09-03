#########################################################################################################
#' Analysis for the manuscript submitted to Journal of Cognitive Psychology
#' Erik Marsja, Gregory Neely, and Jessica K. Ljungberg
#'
#' 1) Handling the data: loading, removing warm up trials, etc.
#' 2) Data analysis: t-tests, anovas
#'    
#'
#'
#########################################################################################################

rm(list=ls())
##Contrasts "like SPSS"
options(contrasts=c("contr.sum", "contr.poly"))

removeWarmupTrials <-function(df){
  df <- subset(df, Condition != "practice")
  df <- droplevels(df)
}

#1. Handling the data

##Load the data
dataFrame <- read.csv("./Data/_STUDY1_post-dev-std.csv", sep=";")

dataFrame$Sub_id <- as.factor(dataFrame$Sub_id)
dataFrame <- removeWarmupTrials(dataFrame)

dataFrame$BlockModality <- as.factor(dataFrame$BlockModality)
dataFrame <- droplevels(dataFrame)

#Exclude response latencies < 200 ms.
dataFrame <- subset(dataFrame, RT >= 200)
#Hit only for response latencies analysis (our DV of interest)
dataFrame.rt <- subset(dataFrame, Accuracy == 1)

dataFrame.rt$BlockModality <- as.numeric((dataFrame.rt$BlockModality))

##2. Data analysis
#Aggregate data
require(dplyr)
require(tidyr)
#  Acc
grp <- group_by(dataFrame, Sub_id, BlockModality, TrialType, Modality)
mean.acc <- as.data.frame(summarise(grp, ACC = mean(Accuracy)))
mean.acc <- spread(mean.acc, TrialType, ACC)
mean.acc$Deviance <- mean.acc[,4] - mean.acc[,6]
mean.acc$Pdeviance <- mean.acc[,5] - mean.acc[,6]

# Anova on deviance distraction (Accuracy Deviant trials - Accuracy Standard Trials)
require(afex)
accdistraction.aov <- aov_ez("Sub_id", 
                          "Deviance", 
                          mean.acc,
                          within = c("Modality", "BlockModality"),
                          anova_table=list(correction = "none", es ='pes'),
                          print.formula = T
)
accdistraction.aov
dist.lsm <- lsmeans(accdistraction.aov, ~ BlockModality | Modality, options = list(estName="means"))


# RT
grp <- group_by(dataFrame.rt, Sub_id, BlockModality, TrialType, Modality)
mean.rts <- as.data.frame(summarise(grp, RT = mean(RT)))
mean.rts <- spread(mean.rts, TrialType, RT)
mean.rts$Deviance <- mean.rts[,4] - mean.rts[,6]
mean.rts$Pdeviance <- mean.rts[,5] - mean.rts[,6]
grp.mod <- group_by(mean.rts, Modality)
aggregated.rts <- as.data.frame(summarise(grp.mod, Deviance=mean(Deviance), Pdeviance=mean(Pdeviance)))


# Check if we have some distraction: i.e., is the deviance distraction different from zero
# Sound deviance distraction?
t.test.data <- as.data.frame(group_by(mean.rts, Sub_id, Modality) %>% summarise(dev=mean(Deviance), pdev=mean(Pdeviance)))
t.test(t.test.data[t.test.data$Modality == "Sound",]$dev, mu=0)
# Tactile de viance distraction?
t.test(t.test.data[t.test.data$Modality == "Vibration",]$dev, mu=0)


# Sound post-deviance distraction?
t.test(t.test.data[t.test.data$Modality == "Sound",]$pdev, mu=0)
# Vibration post-deviance distraction?
t.test(t.test.data[t.test.data$Modality == "Vibration",]$pdev, mu=0)

require(ggplot2);require(ggthemes)

# Anova on deviance distraction (RT Deviant trials - RT Standard Trials)
require(afex)
distraction.aov <- aov_ez("Sub_id", 
                          "Deviance", 
                          mean.rts,
                          within = c("Modality", "BlockModality"),
                          anova_table=list(correction = "none", es ='pes'),
                          print.formula = T
)
distraction.aov


## Setting up a grid
dist.lsm <- lsmeans(distraction.aov, ~ BlockModality | Modality, options = list(estName="means"))
pairs(dist.lsm)

## Polynomial contrasts
summary(contrast(dist.lsm, method = "poly", by = c("Modality")))

# Anova on post-deviance distraction (RT in Deviant trials - RT Standard trials)
pdistraction.aov <- aov_ez("Sub_id", 
                          "Pdeviance", 
                          mean.rts,
                          within = c("Modality", "BlockModality"),
                          anova_table=list(correction = "none", es ='pes'),
                          print.formula = T
)
pdistraction.aov
pdist.lsm <- lsmeans(pdistraction.aov, ~ BlockModality | Modality, options = list(estName="means"))
pdev.dat <- as.data.frame(summary(pdist.lsm))
pdev.dat$Type <- "Post-Deviance Distraction"

# PLOT data
dev.dat <- as.data.frame(summary(dist.lsm))
dev.dat$Type <- "Deviance Distraction"
# 1 Combine the pdev and dev dataframes
plot.dat <- rbind(pdev.dat, dev.dat)
# Data t plot 
plot.dat$Block <- as.numeric(plot.dat$Block)

lineplot <- ggplot(data = plot.dat, aes(x=Block, y=means, colour=Type, fill=Type) ) + 

  geom_point(aes(shape=Type), size=5, position=position_dodge(.2)) + 
  geom_line(position=position_dodge(.2)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width=0.2, position=position_dodge(.2)) +
  scale_y_continuous("Mean ms.", limits = c(-30,70), breaks=seq(-30, 70, by = 10)) + 
  scale_colour_grey()+
  
  
  geom_hline(aes(yintercept=0))+
  theme_bw() + 
  facet_wrap(~Modality, ncol=2,nrow=1, scales="free") +
  theme_tufte(base_family="Helvetica") + theme(axis.line=element_line(),
                                               plot.title = element_text(face="bold", size=12), 
                                               axis.title.x = element_text(face="bold", size=10),
                                               axis.title.y = element_text(face="bold", size=10, angle=90),
                                               panel.grid.major = element_blank(), # switch off major gridlines
                                               panel.grid.minor = element_blank(), # switch off minor gridlines
                                               strip.background = element_blank(),
                                               strip.text.x = element_text(size=10),
                                               legend.position = c(.8,.85), # manually position the legend (numbers being from 0,0 at bottom left of whole plot to 1,1 at top right)
                                               legend.title = element_blank(), # switch off the legend title
                                               legend.text = element_text(size=10),
                                               legend.key.size = unit(1, "lines"),
                                               legend.key = element_blank() # switch off the rectangle around symbols in the legend
  )+ 
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5)) 

## Saving the figure
tiff("Figs/Figure-2.tiff", width = 6.320, height = 4.247, units = 'in', res = 300)
lineplot
dev.off()




