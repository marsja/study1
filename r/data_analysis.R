#########################################################################################################
#' Analysis for the manuscript submitted to Journal of Cognitive Psychology
#' Erik Marsja, Gregory Neely, and Jessica K. Ljungberg
#'
#' 
#'
#'
#########################################################################################################

rm(list=ls())

removeWarmupTrials <-function(df){
  df <- subset(df, Condition != "practice")
  df <- droplevels(df)
}
#1. Handling the data

##Contrasts "like SPSS"
options(contrasts=c("contr.sum", "contr.poly"))

##Load the data
dataFrame <- read.csv("./Data/_STUDY1_post-dev-std.csv", sep=";")

dataFrame$Sub_id <- as.factor(dataFrame$Sub_id)
dataFrame <- removeWarmupTrials(dataFrame)


##Subsetting the data frame
dataFrame <- subset(dataFrame, BlockModality == 1 | BlockModality == 4)
dataFrame$BlockModality <- as.factor(dataFrame$BlockModality)
dataFrame <- droplevels(dataFrame)
#Exclude response latencies < 200 ms.
dataFrame <- subset(dataFrame, RT >= 200)
#Hit only for response latencies analysis (our DV of interest)
dataFrame.rt <- subset(dataFrame, Accuracy == 1)

##2. Data analysis

##2.1 Omnibus ANOVA using afex.
require(afex)
rt.aov <- aov_ez("Sub_id", 
                 "RT", 
                 dataFrame.rt,
                 within = c("Modality", "TrialType", "BlockModality"),
                 anova_table=list(correction = "none", es ='pes'),
                 print.formula = T
)
rt.aov

##2.2 Following up the three-way interaction with two seperate ANOVAs in each modality
###2.2.1 Auditory Modality
Sound.dataFrame <- subset(dataFrame.rt, Modality == "Sound")
Sound.dataFrame <- droplevels(Sound.dataFrame)
soundMod <-  aov_ez("Sub_id", "RT", 
                    Sound.dataFrame,
                    within = c("TrialType", "BlockModality"),
                    anova_table=list(correction = "none", es = "pes"),
                    print.formula = T
)

####2.2.1Pairwise comparisons using lsmeans
lsmeans(soundMod, "TrialType", contr="pairwise", adjust="holm")

###2.2.2 Tactile Modality

Tactile.dataFrame <- subset(dataFrame.rt, Modality == "Vibration")
Tactile.dataFrame  <- droplevels(Tactile.dataFrame )
tactileMod <-  aov_ez("Sub_id", 
                      "RT", 
                      Tactile.dataFrame,
                      within = c("TrialType", "BlockModality"),
                      anova_table=list(correction = "none", es = "pes"),
                      print.formula = T
)
###2.2.3 Pairwise comparsions
lsmeans(one.tactileMod, "TrialType", contr="pairwise", adjust="holm")

###2.3 Test of Main effects in block 1 and block 4 seperately
###2.3.1 Block 1
one.tactile <- subset(Tactile.dataFrame, BlockModality == 1)
one.tactileMod <-  aov_ez("Sub_id", 
                      "RT", 
                      one.tactile,
                      within = "TrialType",
                      anova_table=list(correction = "none", es = "pes"),
                      print.formula = T
)

###2.3.1 Block 4
four.tactile <- subset(Tactile.dataFrame, BlockModality == 4)
four.tactileMod <-  aov_ez("Sub_id", 
                          "RT", 
                          four.tactile,
                          within = "TrialType",
                          anova_table=list(correction = "none", es = "pes"),
                          print.formula = T
)


###3.3.2 Pairwise comparsions in block 1 (due to sig. main effect)
lsmeans(one.tactileMod, "TrialType", contr="pairwise", adjust="holm")

#########################################################################################################
#4. Accuracy Analysis (note, analysis reported FIRST in the manuscript)

#Overall performance
require(dplyr)
acc.descriptives <- summarise(dataFrame, M = mean(Accuracy), sd = sd(Accuracy))

accuracy.aov <-  aov_ez("Sub_id", 
                 "Accuracy", 
                 dataFrame,
                 within = c("Modality", "TrialType"),
                 anova_table=list(correction = "none", es = "pes"),
                 print.formula = T
)

#Pairwise comparisons
lsmeans(accuracy.aov, "TrialType" , contr="pairwise", adjust="holm")



