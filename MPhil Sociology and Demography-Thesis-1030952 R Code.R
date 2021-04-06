#MPhil Sociology and Demography Thesis
#Paternal Involvement in Early Childhood and Subsequent Child Outcomes in British-born Children: 
#Evidence From the Millennium Cohort Study
#Candidate Number: 1030952

##################################################################################################### DATA ####
require(tidyverse)
#please set your working directory with the data files in it
#setwd()

#MCS1 9 Months
MCS1_parent <- read.table("mcs1_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS1_derived <- read.table("mcs1_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_derived)[1] <- "mcsid"

#MCS2 3 Years
MCS2_parent <- read.table("mcs2_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS2_derived <- read.table("mcs2_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS2_derived)[1] <- "mcsid"

#MCS3 5 Years
MCS3_parent <- read.table("mcs3_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS3_derived <- read.table("mcs3_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS3_derived)[1] <- "mcsid"

#MCS4 7 Years
MCS4_parent <- read.table("mcs4_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS4_derived <- read.table("mcs4_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_derived)[1] <- "mcsid"
MCS4_aspirations <- read.table("mcs4_aspirations_classifications.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_aspirations)[1] <- "mcsid"

#MCS5 11 Years
MCS5_parent <- read.table("mcs5_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_parent)[1] <- "mcsid"
MCS5_cm_derived <- read.table("mcs5_cm_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_cm_derived)[1] <- "mcsid"
MCS5_family_derived <- read.table("mcs5_family_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_family_derived)[1] <- "mcsid"
MCS5_parent_cm_interview <- read.table("mcs5_parent_cm_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_parent_cm_interview)[1] <- "mcsid"

#MCS6 14 Years
MCS6_parent <- read.table("mcs6_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS6_parent)[1] <- "mcsid"
MCS6_cm_derived <- read.table("mcs6_cm_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS6_cm_derived)[1] <- "mcsid"
MCS6_family_derived <- read.table("mcs6_family_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS6_family_derived)[1] <- "mcsid"
MCS6_parent_derived <- read.table("mcs6_parent_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS6_parent_derived)[1] <- "mcsid"
MCS6_parent_cm_interview <- read.table("mcs6_parent_cm_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS6_parent_cm_interview)[1] <- "mcsid"

################################################################################################# TYPOLOGY ####
MCS <- full_join(MCS3_parent, MCS4_parent, by = "mcsid")
table(MCS$cpreofa0)

MCS3full <- subset(MCS, cmpsex00 == 2 & cppsex00 == 1)

MCS3 <- data.frame(NULL)
MCS3 <- as.data.frame(MCS3full$mcsid)
MCS3 <- cbind(MCS3, MCS3full$cpreofa0) #How often do you read to child
MCS3 <- cbind(MCS3, MCS3full$cpsitsa0) #How often do you tell stories to child
MCS3 <- cbind(MCS3, MCS3full$cpplmua0) #How often does musical activities with child
MCS3 <- cbind(MCS3, MCS3full$cppamaa0) #How often draws/paints with child
MCS3 <- cbind(MCS3, MCS3full$cpactia0) #How often plays physically active games with child
MCS3 <- cbind(MCS3, MCS3full$cpgamea0) #How often plays games/toys indoors with child 
MCS3 <- cbind(MCS3, MCS3full$cpwalka0) #How often takes child to playground/park
#MCS3 <- cbind(MCS3, MCS3full$cpbedra0) #How often puts child to bed
#MCS3 <- cbind(MCS3, MCS3full$cplooka0) #How often looks after child alone
colnames(MCS3) <- c("mcsid", "reads3p", "story3p", "music3p", "draws3p", "physi3p", "indor3p", "plgnd3p")

MCS3 <- subset(MCS3, reads3p > 0 & story3p > 0 & music3p > 0 & draws3p > 0 & physi3p > 0 & indor3p > 0 & plgnd3p > 0)

#bar plots for mcs3 fathers (use str to find total N)
names = c("Every day", "Several times a week", "Once or twice a week",
          "Once or twice a month", "Less often than that", "Not at all")
par(mar=c(10.5,4,4,0.5))

thing <- table(MCS3$reads3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

#100*round(prop.table(table(MCS3$reads3p)), 3)
#prop.table(table(MCS3$reads3p))

thing <- table(MCS3$story3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3$music3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3$draws3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3$physi3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3$indor3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3$plgnd3p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)
### end of bar plots for mcs3 fathers ###

MCS4full <- subset(MCS, dmpsex00 == 2 & dppsex00 == 1)

MCS4 <- data.frame(NULL)
MCS4 <- as.data.frame(MCS4full$mcsid)
MCS4 <- cbind(MCS4, MCS4full$dpreofa0) #How often do you read to child
MCS4 <- cbind(MCS4, MCS4full$dpsitsa0) #How often do you tell stories to child
MCS4 <- cbind(MCS4, MCS4full$dpplmua0) #How often does musical activities with child
MCS4 <- cbind(MCS4, MCS4full$dppamaa0) #How often draws/paints with child
MCS4 <- cbind(MCS4, MCS4full$dpactia0) #How often plays physically active games with child
MCS4 <- cbind(MCS4, MCS4full$dpgamea0) #How often plays games/toys indoors with child 
MCS4 <- cbind(MCS4, MCS4full$dpwalka0) #How often takes child to playground/park
#MCS4 <- cbind(MCS4, MCS4full$dpbedra0) #How often puts child to bed
#MCS4 <- cbind(MCS4, MCS4full$dplooka0) #How often looks after child alone
colnames(MCS4) <- c("mcsid", "reads4p", "story4p", "music4p", "draws4p", "physi4p", "indor4p", "plgnd4p")

MCS4 <- subset(MCS4, reads4p > 0 & story4p > 0 & music4p > 0 & draws4p > 0 & physi4p > 0 & indor4p > 0 & plgnd4p > 0)

#bar plots for mcs4 fathers (use str to find total N)
names = c("Every day", "Several times a week", "Once or twice a week",
          "Once or twice a month", "Less often than that", "Not at all")
par(mar=c(10.5,4,4,0.5))

thing <- table(MCS4$reads4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$story4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$music4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$draws4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$physi4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$indor4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4$plgnd4p)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)
### end of bar plots for mcs4 fathers ###

MCS3full <- subset(MCS, cmpsex00 == 2 & cppsex00 == 1)

MCS3main <- data.frame(NULL)
MCS3main <- as.data.frame(MCS3full$mcsid)
MCS3main <- cbind(MCS3main, MCS3full$cmreofa0) #How often do you read to child
MCS3main <- cbind(MCS3main, MCS3full$cmsitsa0) #How often do you tell stories to child
MCS3main <- cbind(MCS3main, MCS3full$cmplmua0) #How often does musical activities with child
MCS3main <- cbind(MCS3main, MCS3full$cmpamaa0) #How often draws/paints with child
MCS3main <- cbind(MCS3main, MCS3full$cmactia0) #How often plays physically active games with child
MCS3main <- cbind(MCS3main, MCS3full$cmgamea0) #How often plays games/toys indoors with child 
MCS3main <- cbind(MCS3main, MCS3full$cmwalka0) #How often takes child to playground/park
#MCS3main <- cbind(MCS3main, MCS3full$cmbedra0) #How often puts child to bed
#MCS3main <- cbind(MCS3main, MCS3full$cmlooka0) #How often looks after child alone
colnames(MCS3main) <- c("mcsid", "reads3m", "story3m", "music3m", "draws3m", "physi3m", "indor3m", "plgnd3m")

MCS3main <- subset(MCS3main, reads3m > 0 & story3m > 0 & music3m > 0 & draws3m > 0 & physi3m > 0 & indor3m > 0 & plgnd3m > 0)

#bar plots for mcs3 mothers (use str to find total N)
names = c("Every day", "Several times a week", "Once or twice a week",
          "Once or twice a month", "Less often than that", "Not at all")
par(mar=c(10.5,4,4,0.5))

thing <- table(MCS3main$reads3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$story3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$music3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$draws3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$physi3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$indor3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS3main$plgnd3m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)
### end of bar plots for mcs3 mothers ###

MCS4full <- subset(MCS, dmpsex00 == 2 & dppsex00 == 1)

MCS4main <- data.frame(NULL)
MCS4main <- as.data.frame(MCS4full$mcsid) # PLEASE CHECK IT IS M AND NOT P
MCS4main <- cbind(MCS4main, MCS4full$dmreofa0) #How often do you read to child
MCS4main <- cbind(MCS4main, MCS4full$dmsitsa0) #How often do you tell stories to child
MCS4main <- cbind(MCS4main, MCS4full$dmplmua0) #How often does musical activities with child
MCS4main <- cbind(MCS4main, MCS4full$dmpamaa0) #How often draws/paints with child
MCS4main <- cbind(MCS4main, MCS4full$dmactia0) #How often plays physically active games with child
MCS4main <- cbind(MCS4main, MCS4full$dmgamea0) #How often plays games/toys indoors with child 
MCS4main <- cbind(MCS4main, MCS4full$dmwalka0) #How often takes child to playground/park
#MCS4main <- cbind(MCS4main, MCS4full$dmbedra0) #How often puts child to bed
#MCS4main <- cbind(MCS4main, MCS4full$dmlooka0) #How often looks after child alone
colnames(MCS4main) <- c("mcsid", "reads4m", "story4m", "music4m", "draws4m", "physi4m", "indor4m", "plgnd4m")

MCS4main <- subset(MCS4main, reads4m > 0 & story4m > 0 & music4m > 0 & draws4m > 0 & physi4m > 0 & indor4m > 0 & plgnd4m > 0)

#bar plots for mcs4 mothers (use str to find total N)
names = c("Every day", "Several times a week", "Once or twice a week",
          "Once or twice a month", "Less often than that", "Not at all")
par(mar=c(10.5,4,4,0.5))

thing <- table(MCS4main$reads4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$story4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$music4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$draws4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$physi4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$indor4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

thing <- table(MCS4main$plgnd4m)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)
### end of bar plots for mcs4 mothers ###

MCS_compare <- full_join(MCS3, MCS4, by = "mcsid")
MCS_compare <- full_join(MCS_compare, MCS3main, by = "mcsid")
MCS_compare <- full_join(MCS_compare, MCS4main, by = "mcsid")

#Typology (P and M) at MCS3
table(MCS_compare$reads3p)
table(MCS_compare$story3p)
table(MCS_compare$music3p)
table(MCS_compare$draws3p)
table(MCS_compare$physi3p)
table(MCS_compare$indor3p)
table(MCS_compare$plgnd3p)

MCS_compare$mcs3pall <- MCS_compare$reads3p + MCS_compare$story3p + MCS_compare$music3p + MCS_compare$draws3p + 
  MCS_compare$physi3p + MCS_compare$indor3p + MCS_compare$plgnd3p
table(MCS_compare$mcs3pall)
summary(MCS_compare$mcs3pall)
sd(MCS_compare$mcs3pall, na.rm = T)
NROW(na.omit(MCS_compare$mcs3pall))
sum(table(MCS_compare$mcs3pall))

table(MCS_compare$reads3m)
table(MCS_compare$story3m)
table(MCS_compare$music3m)
table(MCS_compare$draws3m)
table(MCS_compare$physi3m)
table(MCS_compare$indor3m)
table(MCS_compare$plgnd3m)

MCS_compare$mcs3mall <- MCS_compare$reads3m + MCS_compare$story3m + MCS_compare$music3m + MCS_compare$draws3m + 
  MCS_compare$physi3m + MCS_compare$indor3m + MCS_compare$plgnd3m
table(MCS_compare$mcs3mall)
summary(MCS_compare$mcs3mall)
sd(MCS_compare$mcs3mall, na.rm = T)

MCS_compare$typegrp3[MCS_compare$mcs3pall >= 21 & MCS_compare$mcs3mall >= 21] <- 0
MCS_compare$typegrp3[MCS_compare$mcs3pall >= 21 & MCS_compare$mcs3mall < 21] <- 1
MCS_compare$typegrp3[MCS_compare$mcs3pall < 21 & MCS_compare$mcs3mall >= 21] <- 2
MCS_compare$typegrp3[MCS_compare$mcs3pall < 21 & MCS_compare$mcs3mall < 21] <- 3
table(MCS_compare$typegrp3)
prop.table(table(MCS_compare$typegrp3))
sum(table(MCS_compare$typegrp3))

#Typology at MCS4
MCS_compare$mcs4pall <- MCS_compare$reads4p + MCS_compare$story4p + MCS_compare$music4p + MCS_compare$draws4p + 
  MCS_compare$physi4p + MCS_compare$indor4p + MCS_compare$plgnd4p
table(MCS_compare$mcs4pall)
summary(MCS_compare$mcs4pall)
sd(MCS_compare$mcs4pall, na.rm = T)

MCS_compare$mcs4mall <- MCS_compare$reads4m + MCS_compare$story4m + MCS_compare$music4m + MCS_compare$draws4m + 
  MCS_compare$physi4m + MCS_compare$indor4m + MCS_compare$plgnd4m
table(MCS_compare$mcs4mall)
summary(MCS_compare$mcs4mall)
sd(MCS_compare$mcs4mall, na.rm = T)

MCS_compare$typegrp4[MCS_compare$mcs4pall >= 21 & MCS_compare$mcs4mall >= 21] <- 0
MCS_compare$typegrp4[MCS_compare$mcs4pall >= 21 & MCS_compare$mcs4mall < 21] <- 1
MCS_compare$typegrp4[MCS_compare$mcs4pall < 21 & MCS_compare$mcs4mall >= 21] <- 2
MCS_compare$typegrp4[MCS_compare$mcs4pall < 21 & MCS_compare$mcs4mall < 21] <- 3
table(MCS_compare$typegrp4)
prop.table(table(MCS_compare$typegrp4))
sum(table(MCS_compare$typegrp4))

#Typology combined between MCS3 and 4
MCS_compare$mcscombinep <- MCS_compare$mcs3pall + MCS_compare$mcs4pall
MCS_compare$mcscombinem <- MCS_compare$mcs3mall + MCS_compare$mcs4mall
table(MCS_compare$mcscombinep)
table(MCS_compare$mcscombinem)
summary(MCS_compare$mcscombinep)
sd(MCS_compare$mcscombinep, na.rm = T)
summary(MCS_compare$mcscombinem)
sd(MCS_compare$mcscombinem, na.rm = T)

MCS_compare$typecombine[MCS_compare$mcscombinep >= 42 & MCS_compare$mcscombinem >= 42] <- 0
MCS_compare$typecombine[MCS_compare$mcscombinep >= 42 & MCS_compare$mcscombinem < 42] <- 1
MCS_compare$typecombine[MCS_compare$mcscombinep < 42 & MCS_compare$mcscombinem >= 42] <- 2
MCS_compare$typecombine[MCS_compare$mcscombinep < 42 & MCS_compare$mcscombinem < 42] <- 3
table(MCS_compare$typecombine)
prop.table(table(MCS_compare$typecombine))
sum(table(MCS_compare$typecombine))

table(MCS_compare$typegrp3)
table(MCS_compare$typegrp4)

MCS_compare$typegrp3 <- as.factor(MCS_compare$typegrp3)
MCS_compare$typegrp4 <- as.factor(MCS_compare$typegrp4)
MCS_compare$typecombine <- as.factor(MCS_compare$typecombine)

Data <- MCS1_derived[, c("mcsid"), drop = F]
Data <- full_join(Data, MCS_compare[c("mcsid", "mcs3pall", "mcs3mall", "mcs4pall", "mcs4mall", "mcscombinep", "mcscombinem", 
                                      "typegrp3", "typegrp4", "typecombine")], by = "mcsid")
table(Data$typegrp4)

Data$typegrp3 <- as.factor(Data$typegrp3)
Data$typegrp4 <- as.factor(Data$typegrp4)
Data$typecombine <- as.factor(Data$typecombine)

################################################################################ CONTROL/OUTCOME VARIABLES ####
###Outcome Variables
#Aspiration
df <- MCS4_aspirations[, c("mcsid", "DCASOC0A", "DCAMFX0A", "DCAEXT0A")]
table(df$DCASOC0A) #Social Occupation Grouping (SOC2000 Major Groups)
table(df$DCAMFX0A) #Whether aspiration is masculine or feminine (Hakim 1998)
table(df$DCAEXT0A) #Aspiration goal (Extrinsic, Instrinsic or Otherwise) (Flouri 1999)
df <- subset(df, DCASOC0A >= 0)

df$aspiresoci <- df$DCASOC0A
df$aspiremasc <- df$DCAMFX0A
df$aspiregoal <- df$DCAEXT0A
table(df$DCAEXT0A)

Data <- full_join(Data, df[c("mcsid", "aspiresoci", "aspiremasc", "aspiregoal")], by = "mcsid")

Data$aspiresoci <- as.factor(Data$aspiresoci) #social 'class' of occupation listed
Data$aspiremasc <- as.factor(Data$aspiremasc) #how masculine or feminine 
Data$aspiregoal <- as.factor(Data$aspiregoal) #extrinsic or intrinsic goal

#Socio-emotional development (SDQ and subscales)
#Externalising = hyper + conduct
#Internalising = peer + emotion
#Prosocial
#SDQ tds

#MCS3: CDEMOTA0 CDCONDA0 CDHYPEA0 CDPEERA0 CDPROSA0 CDEBDTA0
df <- MCS3_derived[, c("mcsid", "CDEMOTA0", "CDCONDA0", "CDHYPEA0", "CDPEERA0", "CDPROSA0", "CDEBDTA0")]
df <- subset(df, CDEMOTA0 >= 0)
df <- subset(df, CDCONDA0 >= 0)
df <- subset(df, CDHYPEA0 >= 0)
df <- subset(df, CDPEERA0 >= 0)
df <- subset(df, CDPROSA0 >= 0)
df <- subset(df, CDEBDTA0 >= 0)
colnames(df) <- c("mcsid", "emotion3", "conduct3", "hyper3", "peer3", "prosocial3", "tds3")
Data <- full_join(Data, df[c("mcsid", "emotion3", "conduct3", "hyper3", "peer3", "prosocial3", "tds3")], by = "mcsid")

#MCS4: DDEMOTA0 DDCONDA0 DDHYPEA0 DDPEERA0 DDPROSA0 DDEBDTA0
df <- MCS4_derived[, c("mcsid", "DDEMOTA0", "DDCONDA0", "DDHYPEA0", "DDPEERA0", "DDPROSA0", "DDEBDTA0")]
df <- subset(df, DDEMOTA0 >= 0)
df <- subset(df, DDCONDA0 >= 0)
df <- subset(df, DDHYPEA0 >= 0)
df <- subset(df, DDPEERA0 >= 0)
df <- subset(df, DDPROSA0 >= 0)
df <- subset(df, DDEBDTA0 >= 0)
colnames(df) <- c("mcsid", "emotion4", "conduct4", "hyper4", "peer4", "prosocial4", "tds4")
Data <- full_join(Data, df[c("mcsid", "emotion4", "conduct4", "hyper4", "peer4", "prosocial4", "tds4")], by = "mcsid")

#MCS5: EDEMOT00 EDCOND00 EDHYPE00 EDPEER00 EDPROS00 EDEBDTAA
df <- MCS5_cm_derived[, c("mcsid", "EDEMOT00", "EDCOND00", "EDHYPE00", "EDPEER00", "EDPROS00", "EDEBDTAA")]
df <- subset(df, EDEMOT00 >= 0)
df <- subset(df, EDCOND00 >= 0)
df <- subset(df, EDHYPE00 >= 0)
df <- subset(df, EDPEER00 >= 0)
df <- subset(df, EDPROS00 >= 0)
df <- subset(df, EDEBDTAA >= 0)
colnames(df) <- c("mcsid", "emotion5", "conduct5", "hyper5", "peer5", "prosocial5", "tds5")
Data <- full_join(Data, df[c("mcsid", "emotion5", "conduct5", "hyper5", "peer5", "prosocial5", "tds5")], by = "mcsid")

#MCS6: FEMOTION FCONDUCT FHYPER FPEER FPROSOC FEBDTOT
df <- MCS6_cm_derived[, c("mcsid", "FEMOTION", "FCONDUCT", "FHYPER", "FPEER", "FPROSOC", "FEBDTOT")]
df <- subset(df, FEMOTION >= 0)
df <- subset(df, FCONDUCT >= 0)
df <- subset(df, FHYPER >= 0)
df <- subset(df, FPEER >= 0)
df <- subset(df, FPROSOC >= 0)
df <- subset(df, FEBDTOT >= 0)
colnames(df) <- c("mcsid", "emotion6", "conduct6", "hyper6", "peer6", "prosocial6", "tds6")
Data <- full_join(Data, df[c("mcsid", "emotion6", "conduct6", "hyper6", "peer6", "prosocial6", "tds6")], by = "mcsid")

###Control Variables

### -- Model 1 baseline variables --
###Var: Family income
#Permanent family income: averageing equivalised household income over first six surveys expressed in logarithmic form
dfa <- MCS1_derived[, c("mcsid", "ADOEDE00")]
dfa <- subset(dfa, ADOEDE00 > 0)
dfa$"logincomea" <- log(dfa$ADOEDE00)

dfb <- MCS2_derived[, c("mcsid", "BDOEDE00")]
dfb <- subset(dfb, BDOEDE00 > 0)
dfb$"logincomeb" <- log(dfb$BDOEDE00)
df <- full_join(dfa, dfb, by = "mcsid")

dfc <- MCS3_derived[, c("mcsid", "CDOEDE00")]
dfc <- subset(dfc, CDOEDE00 > 0)
dfc$"logincomec" <- log(dfc$CDOEDE00)
df <- full_join(df, dfc, by = "mcsid")

dfd <- MCS4_derived[, c("mcsid", "DOEDE000")]
dfd <- subset(dfd, DOEDE000 > 0)
dfd$"logincomed" <- log(dfd$DOEDE000)
df <- full_join(df, dfd, by = "mcsid")

dfe <- MCS5_family_derived[, c("mcsid", "EOEDE000")]
dfe <- subset(dfe, EOEDE000 > 0)
dfe$"logincomee" <- log(dfe$EOEDE000)
df <- full_join(df, dfe, by = "mcsid")

dff <- MCS6_family_derived[, c("mcsid", "FOEDE000")]
dff <- subset(dff, FOEDE000 > 0)
dff$"logincomef" <- log(dff$FOEDE000)
df <- full_join(df, dff, by = "mcsid")

df$"meanincomea" <- (df$logincomea + df$logincomeb + df$logincomec + df$logincomed + df$logincomee + df$logincomef)/6

Data <- full_join(Data, df[, c("mcsid", "meanincomea")], by = "mcsid")

dfa <- MCS1_derived[, c("mcsid", "ADOEDE00")]
dfa <- subset(dfa, ADOEDE00 > 0)
dfb <- MCS2_derived[, c("mcsid", "BDOEDE00")]
dfb <- subset(dfb, BDOEDE00 > 0)
dfc <- MCS3_derived[, c("mcsid", "CDOEDE00")]
dfc <- subset(dfc, CDOEDE00 > 0)
dfd <- MCS4_derived[, c("mcsid", "DOEDE000")]
dfd <- subset(dfd, DOEDE000 > 0)
dfe <- MCS5_family_derived[, c("mcsid", "EOEDE000")]
dfe <- subset(dfe, EOEDE000 > 0)
dff <- MCS6_family_derived[, c("mcsid", "FOEDE000")]
dff <- subset(dff, FOEDE000 > 0)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df <- full_join(df, dfd, by = "mcsid")
df <- full_join(df, dfe, by = "mcsid")
df <- full_join(df, dff, by = "mcsid")

df$meanincomeb <- log((df$ADOEDE00 + df$BDOEDE00 + df$CDOEDE00 + df$DOEDE000 + df$EOEDE000 + df$FOEDE000)/6)

Data <- full_join(Data, df[, c("mcsid", "meanincomeb")], by = "mcsid")

###Var: Poverty persistence
#Frequency of poverty across the first five surveys: never, one/two surveys, three/four surveys, five/six surveys
dfa <- MCS1_derived[, c("mcsid", "ADOEDP00")]
dfa <- subset(dfa, ADOEDP00 != -1)

dfb <- MCS2_derived[, c("mcsid", "BDOEDP00")]
dfb <- subset(dfb, BDOEDP00 != -1)
df <- full_join(dfa, dfb, by = "mcsid")

dfc <- MCS3_derived[, c("mcsid", "CDOEDP00")]
dfc <- subset(dfc, CDOEDP00 != -1)
df <- full_join(df, dfc, by = "mcsid")

dfd <- MCS4_derived[, c("mcsid", "DOEDP000")]
dfd <- subset(dfd, DOEDP000 != -1)
df <- full_join(df, dfd, by = "mcsid")

dfe <- MCS5_family_derived[, c("mcsid", "EOEDP000")]
dfe <- subset(dfe, EOEDP000 != -1)
df <- full_join(df, dfe, by = "mcsid")

dff <- MCS6_family_derived[, c("mcsid", "FOEDP000")]
dff <- subset(dff, FOEDP000 != -1)
df <- full_join(df, dff, by = "mcsid")

#df$povertya <- NA
#df$povertya <- df$ADOEDP00 + df$BDOEDP00 + df$CDOEDP00 + df$DOEDP000 + df$EOEDP000 + df$FOEDP000
#table(df$povertya)

#Data <- full_join(Data, df[, c("mcsid", "povertya")], by = "mcsid")
#Data$povertya <- as.factor(Data$povertya)

df$povertyb <- NA
df$povertyb[df$povertya == 0] <- 0
df$povertyb[df$povertya == 1] <- 1
df$povertyb[df$povertya == 2] <- 1
df$povertyb[df$povertya == 3] <- 2
df$povertyb[df$povertya == 4] <- 2
df$povertyb[df$povertya == 5] <- 3
df$povertyb[df$povertya == 6] <- 3
table(df$povertyb)

#Data <- full_join(Data, df[, c("mcsid", "povertyb")], by = "mcsid")
#Data$povertyb <- as.factor(Data$povertyb)

### -- Model 2 additional variables --
###Var: Child age at MCS6 FCCAGE00
df <- MCS6_cm_derived[, c("mcsid", "FCCAGE00")]
table(df$FCCAGE00)
df$cage <- df$FCCAGE00

Data <- full_join(Data, df[, c("mcsid", "cage")], by = "mcsid")
Data$cage <- as.factor(Data$cage)

###Var: Child sex
df <- MCS6_cm_derived[, c("mcsid", "FCCSEX00")] #1 = M, 2 = F
table(df$FCCSEX00)

df$csex[df$FCCSEX00 == 1] <- 1
df$csex[df$FCCSEX00 == 2] <- 0

Data <- full_join(Data, df[, c("mcsid", "csex")], by = "mcsid")
Data$csex <- as.factor(Data$csex)

###Var: Child ethnicity FDCE0600 FDCE0800 FDCE1100
df <- MCS6_cm_derived[, c("mcsid", "FDCE0600", "FDCE0800", "FDCE1100")]
df <- subset(df, FDCE0600 > 0)
table(df$FDCE0800)
table(df$FDCE1100)

for (i in c(1:6)) {
  df$cethnic6[df$FDCE0600 == i] <- i - 1
}
table(df$cethnic6)
for (i in c(1:8)) {
  df$cethnic8[df$FDCE0800 == i] <- i - 1
}
table(df$cethnic8)
for (i in c(1:11)) {
  df$cethnic11[df$FDCE1100 == i] <- i - 1
}
table(df$cethnic11)

Data <- full_join(Data, df[, c("mcsid", "cethnic6", "cethnic8", "cethnic11")], by = "mcsid")
Data$cethnic6 <- as.factor(Data$cethnic6)
Data$cethnic8 <- as.factor(Data$cethnic8)
Data$cethnic11 <- as.factor(Data$cethnic11)

###Var: Number of siblings FDOTHS00
df <- MCS6_family_derived[, c("mcsid", "FDOTHS00")]
table(df$FDOTHS00)
df$siblinga <- df$FDOTHS00
df$siblingb <- df$FDOTHS00
df$siblingb[df$FDOTHS00 >= 3] <- 3
table(df$siblinga)
table(df$siblingb)

Data <- full_join(Data, df[, c("mcsid", "siblinga", "siblingb")], by = "mcsid")
Data$siblinga <- as.factor(Data$siblinga)
Data$siblingb <- as.factor(Data$siblingb)

###Var: Highest maternal academic/educational qualification FDACAQ00
df <- MCS6_parent_derived[, c("mcsid", "FDACAQ00")]
table(df$FDACAQ00)
df <- subset(df, FDACAQ00 > 0)

df$education[df$FDACAQ00 == 96] <- 0 #None of these
df$education[df$FDACAQ00 == 1] <- 1 #NVQ Level 1
df$education[df$FDACAQ00 == 2] <- 2 #NVQ Level 2
df$education[df$FDACAQ00 == 3] <- 3 #NVQ Level 3
df$education[df$FDACAQ00 == 4] <- 4 #NVQ Level 4
df$education[df$FDACAQ00 == 5] <- 5 #NVQ Level 5
df$education[df$FDACAQ00 == 95] <- 6 #Overseas qual only
table(df$education)

Data <- full_join(Data, df[, c("mcsid", "education")], by = "mcsid")
Data$education <- as.factor(Data$education)

###Var: Maternal age at child's birth AMDGAB00 AMDAGB00
df <- MCS1_derived[, c("mcsid", "AMDAGB00", "AMDGAB00")]
table(df$AMDAGB00)
table(df$AMDGAB00)
df <- subset(df, AMDAGB00 > 0)
df <- subset(df, AMDGAB00 > 0)

df$magecont <- df$AMDAGB00
df$magecat <- df$AMDGAB00

for (i in c(1:4)) {
  df$magecat[df$AMDGAB00 == i] <- i - 1
}
table(df$magecat)
table(df$magecont)

Data <- full_join(Data, df[, c("mcsid", "magecat", "magecont")], by = "mcsid")
Data$magecat <- as.factor(Data$magecat)

###Var: Paternal age MCS3
df <- MCS3_parent[, c("mcsid", "cppage00")]
table(df$cppage00)
str(df$cppage00)
df <- subset(df, cppage00 > 0)
df$pagecont3 <- df$cppage00
Data <- full_join(Data, df[, c("mcsid", "pagecont3")], by = "mcsid")

df <- MCS1_derived[, c("mcsid", "APDAGB00", "APDGAB00")]
df <- subset(df, APDAGB00 > 0)
df <- subset(df, APDGAB00 > 0)
table(df$APDAGB00)
table(df$APDGAB00)

df$pagecont <- df$APDAGB00
df$pagecat <- df$APDGAB00

for (i in c(1:4)) {
  df$pagecat[df$APDGAB00 == i] <- i - 1
}
table(df$pagecat)
table(df$pagecont)

Data <- full_join(Data, df[, c("mcsid", "pagecat", "pagecont")], by = "mcsid")
Data$pagecat <- as.factor(Data$pagecat)

###Child health endowment variables:
###Var: Child birthweight
df <- MCS1_derived[c("mcsid", "ADBWGTA0")]
df <- subset(df, ADBWGTA0 > 0)
df$birthweight[df$ADBWGTA0 <= 2.5] <- 1 #underweight
df$birthweight[df$ADBWGTA0 > 2.5] <- 0 #normal weight

Data <- full_join(Data, df[, c("mcsid", "birthweight")], by = "mcsid")
table(Data$birthweight)
Data$birthweight <- as.factor(Data$birthweight)

###Var: Child gestational age 
df <- MCS1_derived[c("mcsid", "ADGESTA0")]
df <- subset(df, ADGESTA0 > 0)
df$gestation[df$ADGESTA0 < 259] <- 1 #under 37 weeks
df$gestation[df$ADGESTA0 >= 259] <- 0 #more than or equal 37 weeks

Data <- full_join(Data, df[, c("mcsid", "gestation")], by = "mcsid")
table(Data$gestation)
Data$gestation <- as.factor(Data$gestation)

###Var: Child longstanding illness FPCLSI00
df <- MCS6_parent_cm_interview[c("mcsid", "FPCLSI00")]
table(df$FPCLSI00)
df <- subset(df, FPCLSI00 > 0)
df$longill[df$FPCLSI00 == 1] <- 1 #yes
df$longill[df$FPCLSI00 == 2] <- 0 #no

Data <- full_join(Data, df[, c("mcsid", "longill")], by = "mcsid")
Data$longill <- as.factor(Data$longill)

###Var: Mother drinking during pregnancy amdrof00
df <- MCS1_parent[c("mcsid", "amdrof00")]
table(df$amdrof00)
df <- subset(df, amdrof00 > 0)
df$pregalcohola[df$amdrof00 == 7] <- 0
df$pregalcohola[df$amdrof00 == 6] <- 1
df$pregalcohola[df$amdrof00 == 5] <- 2
df$pregalcohola[df$amdrof00 == 4] <- 3
df$pregalcohola[df$amdrof00 == 3] <- 4
df$pregalcohola[df$amdrof00 == 2] <- 5
df$pregalcohola[df$amdrof00 == 1] <- 6
table(df$pregalcohola)

df$pregalcoholb[df$amdrof00 == 7] <- 0 #never
df$pregalcoholb[df$amdrof00 == 6] <- 1 #light
df$pregalcoholb[df$amdrof00 == 5] <- 1
df$pregalcoholb[df$amdrof00 == 4] <- 2 #moderate
df$pregalcoholb[df$amdrof00 == 3] <- 3 #heavy
df$pregalcoholb[df$amdrof00 == 2] <- 3
df$pregalcoholb[df$amdrof00 == 1] <- 3
table(df$pregalcoholb)

Data <- full_join(Data, df[, c("mcsid", "pregalcohola", "pregalcoholb")], by = "mcsid")
Data$pregalcohola <- as.factor(Data$pregalcohola)
Data$pregalcoholb <- as.factor(Data$pregalcoholb)

###Var: Mother's smoking during pregnancy
df <- MCS1_parent[c("mcsid", "amsmus0a", "amsmty00", "amsmev00", "amsmch00", "amcich00")]
df <- subset(df, amsmus0a > 0) #1 = no, all others = yes
table(df$amsmus0a)
table(df$amsmty00)
table(df$amsmev00)
table(df$amcich00)

df$pregsmoke <- NA
df$pregsmoke[df$amsmus0a > 1] <- 2
df$pregsmoke[df$amsmus0a == 1] <- 0
df$pregsmoke[df$amcich00 == 0] <- 1
table(df$pregsmoke)
any(is.na(df) == T)

Data <- full_join(Data, df[, c("mcsid", "pregsmoke")], by = "mcsid")
table(Data$pregsmoke)
Data$pregsmoke <- as.factor(Data$pregsmoke)

###Var: Maternal breastfeeding
df <- MCS1_parent[c("mcsid", "ambfeva0", "ambfeaa0", "ambfeda0", "ambfewa0", "ambfema0")]
table(df$ambfeda0)

df$breastfeed <- NA
df$breastfeed[df$ambfeda0 <= 7] <- 1
df$breastfeed[df$ambfeva0 == 2] <- 0
df$breastfeed[df$ambfema0 > 6] <- 4
df$breastfeed[df$ambfema0 > 3 & df$ambfema0 <= 6] <- 3
df$breastfeed[df$ambfewa0 > 1 & df$ambfema0 <= 3] <- 2
table(df$breastfeed)

Data <- full_join(Data, df[, c("mcsid", "breastfeed")], by = "mcsid")
table(Data$breastfeed)
Data$breastfeed <- as.factor(Data$breastfeed)

### -- Model 3 additional variable -- 
###Var: Maternal psychological distress, all waves (RMI for MCS1 and K6 for the rest)
#MCS1
dfa <- MCS1_parent[c("mcsid", "amtire00", "amdepr00", "amworr00", "amrage00", "amscar00", "amupse00", "amkeyd00", "amnerv00", "amhera00")]
table(dfa$amtire00)

dfa <- subset(dfa, amtire00 > 0)
dfa$armi1 <- NA
dfa$armi1[dfa$amtire00 == 2] <- 0 #No = 0
dfa$armi1[dfa$amtire00 == 1] <- 1 #Yes = 1

dfa <- subset(dfa, amdepr00 > 0)
dfa$armi2 <- NA
dfa$armi2[dfa$amdepr00 == 2] <- 0
dfa$armi2[dfa$amdepr00 == 1] <- 1

dfa <- subset(dfa, amworr00 > 0)
dfa$armi3 <- NA
dfa$armi3[dfa$amworr00 == 2] <- 0
dfa$armi3[dfa$amworr00 == 1] <- 1

dfa <- subset(dfa, amrage00 > 0)
dfa$armi4 <- NA
dfa$armi4[dfa$amrage00 == 2] <- 0
dfa$armi4[dfa$amrage00 == 1] <- 1

dfa <- subset(dfa, amscar00 > 0)
dfa$armi5 <- NA
dfa$armi5[dfa$amscar00 == 2] <- 0
dfa$armi5[dfa$amscar00 == 1] <- 1

dfa <- subset(dfa, amupse00 > 0)
dfa$armi6 <- NA
dfa$armi6[dfa$amupse00 == 2] <- 0
dfa$armi6[dfa$amupse00 == 1] <- 1

dfa <- subset(dfa, amkeyd00 > 0)
dfa$armi7 <- NA
dfa$armi7[dfa$amkeyd00 == 2] <- 0
dfa$armi7[dfa$amkeyd00 == 1] <- 1

dfa <- subset(dfa, amnerv00 > 0)
dfa$armi8 <- NA
dfa$armi8[dfa$amnerv00 == 2] <- 0
dfa$armi8[dfa$amnerv00 == 1] <- 1

dfa <- subset(dfa, amhera00 > 0)
dfa$armi9 <- NA
dfa$armi9[dfa$amhera00 == 2] <- 0
dfa$armi9[dfa$amhera00 == 1] <- 1

dfa$armi <- NA
dfa$armi <- dfa$armi1 + dfa$armi2 + dfa$armi3 + dfa$armi4 + dfa$armi5 + dfa$armi6 + dfa$armi7 + dfa$armi8 + dfa$armi9
table(dfa$armi)

dfa$adistress <- NA
dfa$adistress[dfa$armi >= 4] <- 1
dfa$adistress[dfa$armi < 4] <- 0

#MCS2
dfb <- MCS2_derived[c("mcsid", "BMKESS00")]
dfb <- subset(dfb, BMKESS00 >= 0)
dfb$bkess <- NA
dfb$bkess <- dfb$BMKESS00

dfb$bdistress <- NA
dfb$bdistress[dfb$bkess >= 6] <- 1
dfb$bdistress[dfb$bkess < 6] <- 0

#MCS3
dfc <- MCS3_derived[c("mcsid", "CMKESS00")]
dfc <- subset(dfc, CMKESS00 >= 0)
dfc$ckess <- NA
dfc$ckess <- dfc$CMKESS00

dfc$cdistress <- NA
dfc$cdistress[dfc$ckess >= 6] <- 1
dfc$cdistress[dfc$ckess < 6] <- 0

#MCS4
dfd <- MCS4_derived[c("mcsid", "DMKESS00")]
dfd <- subset(dfd, DMKESS00 >= 0)
dfd$dkess <- NA
dfd$dkess <- dfd$DMKESS00

dfd$ddistress <- NA
dfd$ddistress[dfd$dkess >= 6] <- 1
dfd$ddistress[dfd$dkess < 6] <- 0

#MCS5
dfe <- MCS5_parent[c("mcsid", "EPPHDE00", "EPPHHO00", "EPPHRF00", "EPPHEE00", "EPPHWO00", "EPPHNE00")]
table(dfe$EPPHDE00)

dfe$ekess1 <- NA
dfe$ekess1[dfe$EPPHDE00 == 1] <- 4
dfe$ekess1[dfe$EPPHDE00 == 2] <- 3
dfe$ekess1[dfe$EPPHDE00 == 3] <- 2
dfe$ekess1[dfe$EPPHDE00 == 4] <- 1
dfe$ekess1[dfe$EPPHDE00 == 5] <- 0
dfe$ekess1[dfe$EPPHDE00 == 6] <- -1
dfe <- subset(dfe, ekess1 != -1)

dfe$ekess2 <- NA
dfe$ekess2[dfe$EPPHHO00 == 1] <- 4
dfe$ekess2[dfe$EPPHHO00 == 2] <- 3
dfe$ekess2[dfe$EPPHHO00 == 3] <- 2
dfe$ekess2[dfe$EPPHHO00 == 4] <- 1
dfe$ekess2[dfe$EPPHHO00 == 5] <- 0
dfe$ekess2[dfe$EPPHHO00 == 6] <- -1
dfe <- subset(dfe, ekess2 != -1)

dfe$ekess3 <- NA
dfe$ekess3[dfe$EPPHRF00 == 1] <- 4
dfe$ekess3[dfe$EPPHRF00 == 2] <- 3
dfe$ekess3[dfe$EPPHRF00 == 3] <- 2
dfe$ekess3[dfe$EPPHRF00 == 4] <- 1
dfe$ekess3[dfe$EPPHRF00 == 5] <- 0
dfe$ekess3[dfe$EPPHRF00 == 6] <- -1
dfe <- subset(dfe, ekess3 != -1)

dfe$ekess4 <- NA
dfe$ekess4[dfe$EPPHEE00 == 1] <- 4
dfe$ekess4[dfe$EPPHEE00 == 2] <- 3
dfe$ekess4[dfe$EPPHEE00 == 3] <- 2
dfe$ekess4[dfe$EPPHEE00 == 4] <- 1
dfe$ekess4[dfe$EPPHEE00 == 5] <- 0
dfe$ekess4[dfe$EPPHEE00 == 6] <- -1
dfe <- subset(dfe, ekess4 != -1)

dfe$ekess5 <- NA
dfe$ekess5[dfe$EPPHWO00 == 1] <- 4
dfe$ekess5[dfe$EPPHWO00 == 2] <- 3
dfe$ekess5[dfe$EPPHWO00 == 3] <- 2
dfe$ekess5[dfe$EPPHWO00 == 4] <- 1
dfe$ekess5[dfe$EPPHWO00 == 5] <- 0
dfe$ekess5[dfe$EPPHWO00 == 6] <- -1
dfe <- subset(dfe, ekess5 != -1)

dfe$ekess6 <- NA
dfe$ekess6[dfe$EPPHNE00 == 1] <- 4
dfe$ekess6[dfe$EPPHNE00 == 2] <- 3
dfe$ekess6[dfe$EPPHNE00 == 3] <- 2
dfe$ekess6[dfe$EPPHNE00 == 4] <- 1
dfe$ekess6[dfe$EPPHNE00 == 5] <- 0
dfe$ekess6[dfe$EPPHNE00 == 6] <- -1
dfe <- subset(dfe, ekess6 != -1)

dfe$ekess <- NA
dfe$ekess <- dfe$ekess1 + dfe$ekess2 + dfe$ekess3 + dfe$ekess4 + dfe$ekess5 + dfe$ekess6
table(dfe$ekess)

dfe$edistress <- NA
dfe$edistress[dfe$ekess >= 6] <- 1
dfe$edistress[dfe$ekess < 6] <- 0

#MCS6
dff <- MCS6_parent_derived[c("mcsid", "FDKESSL")]
table(dff$FDKESSL)

dff$fdistress <- NA
dff$fdistress[dff$FDKESSL >= 6] <- 1
dff$fdistress[dff$FDKESSL < 6] <- 0

#Combine
df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df <- full_join(df, dfd, by = "mcsid")
df <- full_join(df, dfe, by = "mcsid")
df <- full_join(df, dff, by = "mcsid")

df$distress <- NA
df$distress <- df$armi1 + df$bdistress + df$cdistress + df$ddistress + df$edistress + df$fdistress
table(df$distress) #represents at how many waves the respondent was distressed

Data <- full_join(Data, df[, c("mcsid", "distress")], by = "mcsid")
Data$distress <- as.factor(Data$distress)

###Var: Ever been diagnosed with depression/serious anxiety? FPDEAN00
df <- MCS6_parent[c("mcsid", "FPDEAN00")]
table(df$FPDEAN00) #1 = yes, 2 = no, 3 = do not want to answer (treat as NA)
df <- subset(df, FPDEAN00 > 0)
df <- subset(df, FPDEAN00 < 3)

df$depress[df$FPDEAN00 == 1] <- 1
df$depress[df$FPDEAN00 == 2] <- 0

Data <- full_join(Data, df[, c("mcsid", "depress")], by = "mcsid")
Data$depress <- as.factor(Data$depress)

###Var: OCEAN personality sub-scales FDOPEN FDCONSC FDEXTRAV FDAGREE FDNEUROT
df <- MCS6_parent_derived[c("mcsid", "FDOPEN", "FDCONSC", "FDEXTRAV", "FDAGREE", "FDNEUROT")]
table(df$FDOPEN)
table(df$FDNEUROT)

df <- subset(df, FDOPEN >= 0)
df <- subset(df, FDCONSC >= 0)
df <- subset(df, FDEXTRAV >= 0)
df <- subset(df, FDAGREE >= 0)
df <- subset(df, FDNEUROT >= 0)

df$open <- df$FDOPEN
df$consc <- df$FDCONSC
df$extra <- df$FDEXTRAV
df$agree <- df$FDAGREE
df$neuro <- df$FDNEUROT

Data <- full_join(Data, df[, c("mcsid", "open", "consc", "extra", "agree", "neuro")], by = "mcsid")

### -- Model 4 additional variables --
###Var: Change in mother's relationship between MCS5 and 6
dfe <- MCS5_family_derived[c("mcsid", "EDHTYS00")]
dff <- MCS6_family_derived[c("mcsid", "FDHTYS00")]
table(dfe$EDHTYS00) #1 = two-parent/carers, 2 = single-parent/carers
table(dff$FDHTYS00)
dff <- subset(dff, FDHTYS00 > 0)

df <- full_join(dfe, dff, by = "mcsid")
df$rchange <- NA
df$rchange[df$EDHTYS00 == df$FDHTYS00] <- 0 #no change
df$rchange[df$EDHTYS00 == 2 & df$FDHTYS00 == 1] <- 1 #became partnered
df$rchange[df$EDHTYS00 == 1 & df$FDHTYS00 == 2] <- 2 #became single
table(df$rchange)

Data <- full_join(Data, df[, c("mcsid", "rchange")], by = "mcsid")
Data$rchange <- as.factor(Data$rchange)

###Var: Regular bedtime on weekdays (MCS5) EPBERE00
df <- MCS5_parent_cm_interview[c("mcsid", "EPBERE00")]
table(df$EPBERE00) #1 = never, 2 = sometimes, 3 = usually, 4 = always
df <- subset(df, EPBERE00 > 0)

for (i in c(1:4)) {
  df$regbedtime[df$EPBERE00 == i] <- i - 1
}
table(df$regbedtime)

Data <- full_join(Data, df[, c("mcsid", "regbedtime")], by = "mcsid")
Data$regbedtime <- as.factor(Data$regbedtime)

###Var: Discipline strategies 
#MCS3 mother: cmdiiga0 cmdisma0 cmdisha0 cmdibna0 cmditra0 cmditea0 cmdibra0 cmdirea0 (mcs3_parent_interview)
df <- MCS3_parent[c("mcsid", "cmdiiga0", "cmdisma0", "cmdisha0", "cmdibna0", "cmditra0", "cmditea0", "cmdibra0", "cmdirea0")]

df$ignore <- df$cmdiiga0
df$ignore[df$ignore == 6] <- 0
df <- subset(df, ignore >= 0)
table(df$ignore)

df$smack <- df$cmdisma0
df$smack[df$smack == 6] <- 0
df <- subset(df, smack >= 0)
table(df$smack)

df$shout <- df$cmdisha0
df$shout[df$shout == 6] <- 0
df <- subset(df, shout >= 0)
table(df$shout)

df$bedroom <- df$cmdibna0
df$bedroom[df$bedroom == 6] <- 0
df <- subset(df, bedroom >= 0)
table(df$bedroom)

df$takes <- df$cmditra0
df$takes[df$takes == 6] <- 0
df <- subset(df, takes >= 0)
table(df$takes)

df$tells <- df$cmditea0
df$tells[df$tells == 6] <- 0
df <- subset(df, tells >= 0)
table(df$tells)

df$bribes <- df$cmdibra0
df$bribes[df$bribes == 6] <- 0
df <- subset(df, bribes >= 0)
table(df$bribes)

df$reason <- df$cmdirea0
df$reason[df$reason == 6] <- 0
df <- subset(df, reason >= 0)
table(df$reason)

df$discipline3m <- df$ignore + df$smack + df$shout + df$bedroom + df$takes + df$tells + df$bribes + df$reason
table(df$discipline3m) #higher scores represent greater degrees of discipline

Data <- full_join(Data, df[, c("mcsid", "discipline3m")], by = "mcsid")

#MCS4 mother: dmdiiga0 dmdisma0 dmdisha0 dmdibna0 dmditra0 dmditea0 dmdibra0 dmdirea0 (mcs4_parent_interview)
df <- MCS4_parent[c("mcsid", "dmdiiga0", "dmdisma0", "dmdisha0", "dmdibna0", "dmditra0", "dmditea0", "dmdibra0", "dmdirea0")]

df$ignore <- df$dmdiiga0
df$ignore[df$ignore == 6] <- 0
df <- subset(df, ignore >= 0)
table(df$ignore)

df$smack <- df$dmdisma0
df$smack[df$smack == 6] <- 0
df <- subset(df, smack >= 0)
table(df$smack)

df$shout <- df$dmdisha0
df$shout[df$shout == 6] <- 0
df <- subset(df, shout >= 0)
table(df$shout)

df$bedroom <- df$dmdibna0
df$bedroom[df$bedroom == 6] <- 0
df <- subset(df, bedroom >= 0)
table(df$bedroom)

df$takes <- df$dmditra0
df$takes[df$takes == 6] <- 0
df <- subset(df, takes >= 0)
table(df$takes)

df$tells <- df$dmditea0
df$tells[df$tells == 6] <- 0
df <- subset(df, tells >= 0)
table(df$tells)

df$bribes <- df$dmdibra0
df$bribes[df$bribes == 6] <- 0
df <- subset(df, bribes >= 0)
table(df$bribes)

df$reason <- df$dmdirea0
df$reason[df$reason == 6] <- 0
df <- subset(df, reason >= 0)
table(df$reason)

df$discipline4m <- df$ignore + df$smack + df$shout + df$bedroom + df$takes + df$tells + df$bribes + df$reason
table(df$discipline4m) #higher scores represent greater degrees of discipline

Data <- full_join(Data, df[, c("mcsid", "discipline4m")], by = "mcsid")

###Var: Mother longstanding illness FPLOIL00
df <- MCS6_parent[c("mcsid", "FPLOIL00")]
table(df$FPLOIL00) #1 = yes, 2 = no
df <- subset(df, FPLOIL00 > 0)

df$longillm[df$FPLOIL00 == 1] <- 1
df$longillm[df$FPLOIL00 == 2] <- 0

Data <- full_join(Data, df[, c("mcsid", "longillm")], by = "mcsid")
Data$longillm <- as.factor(Data$longillm)

###Var: Mother smoking/tobacco use FPSMUS0A
df <- MCS6_parent[c("mcsid", "FPSMUS0A")]
table(df$FPSMUS0A)
df <- subset(df, FPSMUS0A >= 0)

df$smoke[df$FPSMUS0A == 0] <- 1
df$smoke[df$FPSMUS0A == 1] <- 0

Data <- full_join(Data, df[, c("mcsid", "smoke")], by = "mcsid")
Data$smoke <- as.factor(Data$smoke)

###Var: Mother alcohol intake FPALDR00
df <- MCS6_parent[c("mcsid", "FPALDR00")]
table(df$FPALDR00)
df <- subset(df, FPALDR00 > 0)
df <- subset(df, FPALDR00 < 6)

df$alcohol[df$FPALDR00 == 5] <- 0
df$alcohol[df$FPALDR00 == 4] <- 1
df$alcohol[df$FPALDR00 == 3] <- 2
df$alcohol[df$FPALDR00 == 2] <- 3
df$alcohol[df$FPALDR00 == 1] <- 4
table(df$alcohol)

Data <- full_join(Data, df[, c("mcsid", "alcohol")], by = "mcsid")
Data$alcohol <- as.factor(Data$alcohol)

###Var: Mother's employment status MCS3 CMDWRK00
df <- MCS3_derived[, c("mcsid", "CMDWRK00")] #1 = in work, 2 = not in work
table(df$CMDWRK00)
df$mwork3[df$CMDWRK00 == 1] <- 0 #in work
df$mwork3[df$CMDWRK00 == 2] <- 1 #not in work
Data <- full_join(Data, df[c("mcsid", "mwork3")], by = "mcsid")
Data$mwork3 <- as.factor(Data$mwork3)

###Var: Mother's employment status MCS4
df <- MCS4_derived[, c("mcsid", "DMDWRK00")] #1 = in work, 2 = not in work
table(df$DMDWRK00)
df$mwork4[df$DMDWRK00 == 1] <- 0
df$mwork4[df$DMDWRK00 == 2] <- 1
Data <- full_join(Data, df[c("mcsid", "mwork4")], by = "mcsid")
Data$mwork4 <- as.factor(Data$mwork4)

###Var: Mother's employment status MCS6 FDWRK00
df <- MCS6_parent_derived[, c("mcsid", "FDWRK00")] #1 = in work, 2 = not in work
table(df$FDWRK00)
df$mwork6[df$FDWRK00 == 1] <- 0
df$mwork6[df$FDWRK00 == 2] <- 1
Data <- full_join(Data, df[c("mcsid", "mwork6")], by = "mcsid")
Data$mwork6 <- as.factor(Data$mwork6)

###Var: Paternal employment #1 = in work, 2 = not in work
df <- MCS3_parent[, c("mcsid", "cppjob00")]
table(df$cppjob00)
df <- subset(df, cppjob00 > 0)
df$pwork3 <- df$cppjob00
Data <- full_join(Data, df[c("mcsid", "pwork3")], by = "mcsid")
Data$pwork3 <- as.factor(Data$pwork3)

df <- MCS4_parent[, c("mcsid", "dppjob00")]
table(df$dppjob00)
df <- subset(df, dppjob00 > 0)
df$pwork4 <- df$dppjob00
Data <- full_join(Data, df[c("mcsid", "pwork4")], by = "mcsid")
Data$pwork4 <- as.factor(Data$pwork4)

###
DataOriginal <- Data

#Remove all cases with missing data
#DataNoNA <- DataOriginal[complete.cases(DataOriginal), ]

#Remove duplicates
Data <- distinct(DataOriginal, mcsid, .keep_all = TRUE)

#Clean by removing NA
DataClean <- Data[complete.cases(Data), ]

str(Data)

############################################################################## Correlates of Parental Time ####

#p (partner) = father
modelp <- lm(mcscombinep ~ meanincomea + pwork3 + mwork3 + education + cage + csex + cethnic8 + siblingb + pagecont + magecont +
               birthweight + gestation + longill + pregalcohola + pregsmoke + breastfeed + 
               open + consc + extra + agree + neuro + 
               depress + regbedtime + discipline3m + longillm, data = Data)
summary(modelp)

modelpm <- lm(mcscombinep ~ mcscombinem + meanincomea + pwork3 + mwork3 + education + cage + csex + cethnic8 + siblingb + pagecont + magecont +
                birthweight + gestation + longill + pregalcohola + pregsmoke + breastfeed + 
                open + consc + extra + agree + neuro + 
                depress + regbedtime + discipline3m + longillm, data = Data)
summary(modelpm)
length(modelp$residuals)
length(fitted(modelpm))

#m (main) = mother
modelm <- lm(mcscombinem ~ meanincomea + pwork3 + mwork3 + education + cage + csex + cethnic8 + siblingb + pagecont + magecont +
               birthweight + gestation + longill + pregalcohola + pregsmoke + breastfeed + 
               open + consc + extra + agree + neuro + 
               depress + regbedtime + discipline3m + longillm, data = Data)
summary(modelm)

modelmp <- lm(mcscombinem ~ mcscombinep + meanincomea + pwork3 + mwork3 + education + cage + csex + cethnic8 + siblingb + pagecont + magecont +
                birthweight + gestation + longill + pregalcohola + pregsmoke + breastfeed + 
                open + consc + extra + agree + neuro + 
                depress + regbedtime + discipline3m + longillm, data = Data)
summary(modelmp)

################################################################################ Models for SDQ/Aspiration ####
str(DataClean)

#install.packages("gmodels") #use for crosstabs
require("gmodels")

#For MCS3, 4, 5, and 6, we need a tds, external, internal, and prosocial score each. tds and prosocial is already done.
Data2 <- Data

Data2$external3 <- Data2$hyper3 + Data2$conduct3
Data2$external4 <- Data2$hyper4 + Data2$conduct4
Data2$external5 <- Data2$hyper5 + Data2$conduct5
Data2$external6 <- Data2$hyper6 + Data2$conduct6
Data2$internal3 <- Data2$emotion3 + Data2$peer3
Data2$internal4 <- Data2$emotion4 + Data2$peer4
Data2$internal5 <- Data2$emotion5 + Data2$peer5
Data2$internal6 <- Data2$emotion6 + Data2$peer6

#Turn the aspiration variables into binary ones, or try to work around it for aspiresoci

names = c("Managers and Senior Officials", "Professional Occupations", "Professional and Technical",
          "Administrative and Secretarial", "Skilled Trades Occupations", "Personal Service Occupations",
          "Customer Service Occupations", "Machine Operatives", "Elementary Occupations")
par(mar=c(14,4,4,0.5))
thing <- table(Data2Clean$aspiresoci)
#ylim <- c(0, 1.1*max(thing))
#xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue")
#text(x = xx, y = thing, label = thing, pos = 3, cex = 0.9)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

names = c("Feminine", "Ultra-Feminine", "Integrated", "Masculine")
par(mar=c(9,4,4,1))
thing <- table(Data2Clean$aspiremasc)
#ylim <- c(0, 1.1*max(thing))
#xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue")
#text(x = xx, y = thing, label = thing, pos = 3, cex = 0.9)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

names = c("Extrinsic", "Extrinsic-intermediate", "Intrinsic-intermediate", "Neutral or Otherwise")
par(mar=c(11,4,4,1))
thing <- table(Data2Clean$aspiregoal)
#ylim <- c(0, 1.1*max(thing))
#xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue")
#text(x = xx, y = thing, label = thing, pos = 3, cex = 0.9)
ylim <- c(0, 1.2*max(thing))
xx <- barplot(thing, names.arg = names, ylim = ylim, las = 2, horiz = F, col = "skyblue", 
              ylab = "Raw Sample Counts", main = "Note: Bolded numbers are percentages", font.main = 1, cex.main = 1)
text(x = xx, y = thing, label = thing, pos = 3, cex = 1)
text(x = xx, y = 1.1*max(thing), label = 100*round(prop.table(thing), 3), pos = 3, cex = 1, font = 2)

prop.table(table(Data2Clean$aspiresoci))
prop.table(table(Data2Clean$aspiremasc))
prop.table(table(Data2Clean$aspiregoal))


table(Data2$aspiresoci) 
#Value = 1.0	Label = Managers and Senior Officials  
#Value = 2.0	Label = Professional Occupations       
#Value = 3.0	Label = Associate Professional and Technical   
#Value = 4.0	Label = Administrative and Secretarial 
#Value = 5.0	Label = Skilled Trades Occupations     
#Value = 6.0	Label = Personal Service Occupations   
#Value = 7.0	Label = Sales and Customer Service Occupations 
#Value = 8.0	Label = Process Plant and Machine Operatives  
#Value = 9.0	Label = Elementary Occupations 

table(Data2$aspiremasc)
#Value = 1.0	Label = Feminine       
#Value = 2.0	Label = Ultra-Feminine 
#Value = 3.0	Label = Integrated     
#Value = 4.0	Label = Masculine

Data2$aspiremascbi[Data2$aspiremasc == 3] <- 0
Data2$aspiremascbi[Data2$aspiremasc == 4] <- 0
Data2$aspiremascbi[Data2$aspiremasc == 1] <- 1
Data2$aspiremascbi[Data2$aspiremasc == 2] <- 1 #0 is masc, 1 is fem
Data2$aspiremascbi <- as.factor(Data2$aspiremascbi)

table(Data2$aspiregoal)
#Value = 1.0	Label = Extrinsic aspiration   
#Value = 2.0	Label = Extrinsic-Intermediate aspiration      
#Value = 3.0	Label = Intrinsic-Intermediate aspiration      
#Value = 4.0	Label = Otherwise

Data2$aspiregoalbi[Data2$aspiregoal == 1] <- 0
Data2$aspiregoalbi[Data2$aspiregoal == 2] <- 0
Data2$aspiregoalbi[Data2$aspiregoal == 3] <- 1
Data2$aspiregoalbi[Data2$aspiregoal == 4] <- 2 #0 is extrinsic, 1 is intrinsic (2 to be removed just before regressing this)

###Please use Data2 not Data for analysis

Data2Clean <- Data2[complete.cases(Data2), ]
str(Data2Clean)

table(Data2Clean$tds6)

########tds
modeltds1 <- lm(tds6 ~ mcscombinep + mcscombinem
                , data = Data2Clean)
summary(modeltds1)

modeltds2 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8
                , data = Data2Clean)
summary(modeltds2)

modeltds3 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8 +
                  meanincomea + mwork6
                , data = Data2Clean)
summary(modeltds3)

modeltds4 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8 +
                  education
                , data = Data2Clean)
summary(modeltds4)

modeltds5 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8 +
                  open + consc + extra + agree + neuro + 
                  distress + depress + longillm + rchange + smoke + alcohol + regbedtime + discipline4m
                , data = Data2Clean)
summary(modeltds5)

modeltds6 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8 +
                  birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke
                , data = Data2Clean)
summary(modeltds6)

modeltds7 <- lm(tds6 ~ mcscombinep + mcscombinem + 
                  pagecont + magecont + cage + csex + siblingb + cethnic8 +
                  tds5 + tds4 + tds3
                , data = Data2Clean)
summary(modeltds7)

########external
modelexternal1 <- lm(external6 ~ mcscombinep + mcscombinem
                     , data = Data2Clean)
summary(modelexternal1)

modelexternal2 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8
                     , data = Data2Clean)
summary(modelexternal2)

modelexternal3 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       meanincomea + mwork6
                     , data = Data2Clean)
summary(modelexternal3)

modelexternal4 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       education
                     , data = Data2Clean)
summary(modelexternal4)

modelexternal5 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       open + consc + extra + agree + neuro + 
                       distress + depress + longillm + rchange + smoke + alcohol + regbedtime + discipline4m
                     , data = Data2Clean)
summary(modelexternal5)

modelexternal6 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke
                     , data = Data2Clean)
summary(modelexternal6)

modelexternal7 <- lm(external6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       external5 + external4 + external3
                     , data = Data2Clean)
summary(modelexternal7)

########internal
modelinternal1 <- lm(internal6 ~ mcscombinep + mcscombinem
                     , data = Data2Clean)
summary(modelinternal1)

modelinternal2 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8
                     , data = Data2Clean)
summary(modelinternal2)

modelinternal3 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       meanincomea + mwork6
                     , data = Data2Clean)
summary(modelinternal3)

modelinternal4 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       education
                     , data = Data2Clean)
summary(modelinternal4)

modelinternal5 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       open + consc + extra + agree + neuro + 
                       distress + depress + longillm + rchange + smoke + alcohol + regbedtime + discipline4m
                     , data = Data2Clean)
summary(modelinternal5)

modelinternal6 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke
                     , data = Data2Clean)
summary(modelinternal6)

modelinternal7 <- lm(internal6 ~ mcscombinep + mcscombinem + 
                       pagecont + magecont + cage + csex + siblingb + cethnic8 +
                       internal5 + internal4 + internal3
                     , data = Data2Clean)
summary(modelinternal7)

########prosocial
modelprosocial1 <- lm(prosocial6 ~ mcscombinep + mcscombinem
                      , data = Data2Clean)
summary(modelprosocial1)

modelprosocial2 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8
                      , data = Data2Clean)
summary(modelprosocial2)

modelprosocial3 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8 +
                        meanincomea + mwork6
                      , data = Data2Clean)
summary(modelprosocial3)

modelprosocial4 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8 +
                        education
                      , data = Data2Clean)
summary(modelprosocial4)

modelprosocial5 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8 +
                        open + consc + extra + agree + neuro + 
                        distress + depress + longillm + rchange + smoke + alcohol + regbedtime + discipline4m
                      , data = Data2Clean)
summary(modelprosocial5)

modelprosocial6 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8 +
                        birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke
                      , data = Data2Clean)
summary(modelprosocial6)

modelprosocial7 <- lm(prosocial6 ~ mcscombinep + mcscombinem + 
                        pagecont + magecont + cage + csex + siblingb + cethnic8 +
                        prosocial5 + prosocial4 + prosocial3
                      , data = Data2Clean)
summary(modelprosocial7)

###Aspiration
table(Data2$aspiresoci)

require(gmodels)
CrossTable(Data2Clean$aspiregoalbi, Data2Clean$aspiremascbi)
CrossTable(Data2Clean$aspiregoal, Data2Clean$typecombine)

CrossTable(Data2Clean$typecombine, Data2Clean$aspiresoci)
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 0], Data2Clean$aspiresoci[Data2Clean$csex == 0]) #Female
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 1], Data2Clean$aspiresoci[Data2Clean$csex == 1]) #Male

CrossTable(Data2Clean$typecombine, Data2Clean$aspiremasc)
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 0], Data2Clean$aspiremasc[Data2Clean$csex == 0]) #Female
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 1], Data2Clean$aspiremasc[Data2Clean$csex == 1]) #Male

CrossTable(Data2Clean$typecombine, Data2Clean$aspiregoal)
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 0], Data2Clean$aspiregoal[Data2Clean$csex == 0]) #Female
CrossTable(Data2Clean$typecombine[Data2Clean$csex == 1], Data2Clean$aspiregoal[Data2Clean$csex == 1]) #Male

crosstab1 <- xtabs(~ csex + typecombine + aspiresoci, data = Data2Clean)
#crosstab1
ftable(crosstab1)
summary(crosstab1)

crosstab2 <- xtabs(~ csex + typecombine + aspiremasc, data = Data2Clean)
#crosstab2
ftable(crosstab2)
summary(crosstab2)

crosstab3 <- xtabs(~ csex + typecombine + aspiregoal, data = Data2Clean)
#crosstab3
ftable(crosstab3)
summary(crosstab3)

###
#exp(coef(m))

modelmascbi1 <- glm(aspiremascbi ~ mcscombinep + mcscombinem, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi1)
exp(coef(modelmascbi1))

modelmascbi2 <- glm(aspiremascbi ~ mcscombinep + mcscombinem + 
                      cage + csex + cethnic8 + siblingb + pagecont + magecont, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi2)
exp(coef(modelmascbi2))

modelmascbi3 <- glm(aspiremascbi ~ mcscombinep + mcscombinem + 
                      meanincomea + pwork3 + mwork3, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi3)
exp(coef(modelmascbi3))

modelmascbi4 <- glm(aspiremascbi ~ mcscombinep + mcscombinem + 
                      education, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi4)
exp(coef(modelmascbi4))

modelmascbi5 <- glm(aspiremascbi ~ mcscombinep + mcscombinem + 
                      open + consc + extra + agree + neuro + depress + regbedtime + discipline3m + longillm, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi5)
exp(coef(modelmascbi5))

modelmascbi6 <- glm(aspiremascbi ~ mcscombinep + mcscombinem + 
                      birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke, 
                    family = binomial, data = Data2Clean)
summary(modelmascbi6)
exp(coef(modelmascbi6))

length(modelmascbi6$model$aspiremascbi)

###

table(Data2Clean$aspiregoalbi)
Data2CleanGoal <- Data2Clean
Data2CleanGoal <- subset(Data2Clean, aspiregoalbi != 2)
table(Data2CleanGoal$aspiregoalbi)
Data2CleanGoal$aspiregoalbi <- as.factor(Data2CleanGoal$aspiregoalbi)

modelgoalbi1 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi1)
exp(coef(modelgoalbi1))

modelgoalbi2 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem + 
                      cage + csex + cethnic8 + siblingb + pagecont + magecont, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi2)
exp(coef(modelgoalbi2))

modelgoalbi3 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem + 
                      meanincomea + pwork3 + mwork3, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi3)
exp(coef(modelgoalbi3))

modelgoalbi4 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem + 
                      education, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi4)
exp(coef(modelgoalbi4))

modelgoalbi5 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem + 
                      open + consc + extra + agree + neuro + depress + regbedtime + discipline3m + longillm, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi5)
exp(coef(modelgoalbi5))

modelgoalbi6 <- glm(aspiregoalbi ~ mcscombinep + mcscombinem + 
                      birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke, 
                    family = binomial, data = Data2CleanGoal)
summary(modelgoalbi6)
exp(coef(modelgoalbi6))

length(modelgoalbi6$model$aspiregoalbi)

require(gmodels)

######################################################################################Specification Curves ####

str(Data2)
#model4m <- glm(paste("sdqdummy ~ mpaidwork", a, b, c, d, e, f, g, h, i), family = binomial, data = Data4boys)

#TDS
p1 <- c("mcscombinep")
p2 <- c("", "+ mcscombinem")
p3 <- c("", "+ meanincomea", "+ meanincomeb")
p4 <- c("", "+ mwork6")
p5 <- c("", "+ education")
p6 <- c("", "+ pagecont + magecont + cage")
p7 <- c("", "+ csex + siblingb", "+ csex + siblinga")
p8 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p9 <- c("", "+ birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke", 
        "+ birthweight + gestation + longill + breastfeed + pregalcoholb + pregsmoke")
p10 <- c("", "+ open + consc + extra + agree + neuro", "+ open + consc + extra + agree + neuro + distress + depress")
p11 <- c("", "+ longillm", "+ longillm + rchange + smoke + alcohol + regbedtime + discipline4m")
p12 <- c("", "+ tds5", "+ tds5 + tds4", "+ tds5 + tds4 + tds3")

coefftds <- data.frame(NULL)
setds <- data.frame(NULL)
ntds <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    for (k in p11) {
                      for (l in p12) {
                        modeltds <- lm(paste("tds6 ~", a, b, c, d, e, f, g, h, i, j, k, l), data = Data2)
                        coefftds <- rbind(coefftds, summary(modeltds)$coefficients[2, 1])
                        setds <- rbind(setds, summary(modeltds)$coefficients[2, 2])
                        ntds <- rbind(ntds, length(modeltds$model$tds6))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coefftds)[1] <- c("involve")
colnames(ntds)[1] <- c("n")
colnames(setds)[1] <- c("se")

tabletds <- cbind(coefftds, setds)
tabletds <- cbind(tabletds, ntds)

tabletds$UB <- tabletds$involve + 1.96*(tabletds$se)
tabletds$LB <- tabletds$involve - 1.96*(tabletds$se)

tabletds <- tabletds %>% arrange(involve) 
num <- rep(1:nrow(tabletds))
tabletds <- cbind(tabletds, num)

setwd()
#write.table(tabletds, file = "tabletds.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tabletds <- read.table("tabletds.txt", header = T, sep = "\t", fill = TRUE)

plot(tabletds$num, tabletds$involve, ylim = range(c(tabletds$LB, tabletds$UB)), pch =".", col = "blue",
     ylab = " Paternal Involvement Coefficient Size", xlab = "Models", main = "Note: blue area shows the 95% confidence interval (total difficulties)", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tabletds))) {
  segments(tabletds$num[i], tabletds$LB[i], tabletds$num[i], tabletds$UB[i], 
           col = "light blue", lwd = 0.01)
}
points(tabletds$num, tabletds$involve, pch = ".", col = "blue")
abline(h = 0)

#points(16350, -0.23, pch = 19, col = "red") #red is unadjusted
#points(20900, -0.044, pch = 19, col = "brown") #brown is adjusted
#abline(h = -0.044, col = "brown") #replace with study's value

plot(tabletds$n, pch =".", col = "red")

#Externalising
p1 <- c("mcscombinep")
p2 <- c("", "+ mcscombinem")
p3 <- c("", "+ meanincomea", "+ meanincomeb")
p4 <- c("", "+ mwork6")
p5 <- c("", "+ education")
p6 <- c("", "+ pagecont + magecont + cage")
p7 <- c("", "+ csex + siblingb", "+ csex + siblinga")
p8 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p9 <- c("", "+ birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke", 
        "+ birthweight + gestation + longill + breastfeed + pregalcoholb + pregsmoke")
p10 <- c("", "+ open + consc + extra + agree + neuro", "+ open + consc + extra + agree + neuro + distress + depress")
p11 <- c("", "+ longillm", "+ longillm + rchange + smoke + alcohol + regbedtime + discipline4m")
p12 <- c("", "+ external5", "+ external5 + external4", "+ external5 + external4 + external3")

coeffext <- data.frame(NULL)
seext <- data.frame(NULL)
nex <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    for (k in p11) {
                      for (l in p12) {
                        modelext <- lm(paste("external6 ~", a, b, c, d, e, f, g, h, i, j, k, l), data = Data2)
                        coeffext <- rbind(coeffext, summary(modelext)$coefficients[2, 1])
                        seext <- rbind(seext, summary(modelext)$coefficients[2, 2])
                        nex <- rbind(nex, length(modelext$model$external6))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeffext)[1] <- c("involve")
colnames(nex)[1] <- c("n")
colnames(seext)[1] <- c("se")

tableext <- cbind(coeffext, seext)
tableext <- cbind(tableext, nex)

tableext$UB <- tableext$involve + 1.96*(tableext$se)
tableext$LB <- tableext$involve - 1.96*(tableext$se)

tableext <- tableext %>% arrange(involve) 
num <- rep(1:nrow(tableext))
tableext <- cbind(tableext, num)

#write.table(tableext, file = "tableext.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tableext <- read.table("tableext.txt", header = T, sep = "\t", fill = TRUE)

plot(tableext$num, tableext$involve, ylim = range(c(tableext$LB, tableext$UB)), pch =".", col = "blue",
     ylab = " Paternal Involvement Coefficient Size", xlab = "Models", main = "Note: blue area shows the 95% confidence interval (externalising difficulties)", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tableext))) {
  segments(tableext$num[i], tableext$LB[i], tableext$num[i], tableext$UB[i], 
           col = "light blue", lwd = 0.01)
}
points(tableext$num, tableext$involve, pch = ".", col = "blue")
abline(h = 0)

#points(16350, -0.23, pch = 19, col = "red") #red is unadjusted
#points(20900, -0.044, pch = 19, col = "brown") #brown is adjusted
#abline(h = -0.044, col = "brown") #replace with study's value

plot(tableext$n, pch =".", col = "red")

#Internalising
p1 <- c("mcscombinep")
p2 <- c("", "+ mcscombinem")
p3 <- c("", "+ meanincomea", "+ meanincomeb")
p4 <- c("", "+ mwork6")
p5 <- c("", "+ education")
p6 <- c("", "+ pagecont + magecont + cage")
p7 <- c("", "+ csex + siblingb", "+ csex + siblinga")
p8 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p9 <- c("", "+ birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke", 
        "+ birthweight + gestation + longill + breastfeed + pregalcoholb + pregsmoke")
p10 <- c("", "+ open + consc + extra + agree + neuro", "+ open + consc + extra + agree + neuro + distress + depress")
p11 <- c("", "+ longillm", "+ longillm + rchange + smoke + alcohol + regbedtime + discipline4m")
p12 <- c("", "+ internal5", "+ internal5 + internal4", "+ internal5 + internal4 + internal3")

coeffint <- data.frame(NULL)
seint <- data.frame(NULL)
nint <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    for (k in p11) {
                      for (l in p12) {
                        modelint <- lm(paste("internal6 ~", a, b, c, d, e, f, g, h, i, j, k, l), data = Data2)
                        coeffint <- rbind(coeffint, summary(modelint)$coefficients[2, 1])
                        seint <- rbind(seint, summary(modelint)$coefficients[2, 2])
                        nint <- rbind(nint, length(modelint$model$internal6))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeffint)[1] <- c("involve")
colnames(nint)[1] <- c("n")
colnames(seint)[1] <- c("se")

tableint <- cbind(coeffint, seint)
tableint <- cbind(tableint, nint)

tableint$UB <- tableint$involve + 1.96*(tableint$se)
tableint$LB <- tableint$involve - 1.96*(tableint$se)

tableint <- tableint %>% arrange(involve) 
num <- rep(1:nrow(tableint))
tableint <- cbind(tableint, num)

#write.table(tableint, file = "tableint.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tableint <- read.table("tableint.txt", header = T, sep = "\t", fill = TRUE)

plot(tableint$num, tableint$involve, ylim = range(c(tableint$LB, tableint$UB)), pch =".", col = "blue",
     ylab = " Paternal Involvement Coefficient Size", xlab = "Models", main = "Note: blue area shows the 95% confidence interval (internalising difficulties)", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tableint))) {
  segments(tableint$num[i], tableint$LB[i], tableint$num[i], tableint$UB[i], 
           col = "light blue", lwd = 0.01)
}
points(tableint$num, tableint$involve, pch = ".", col = "blue")
abline(h = 0)

#points(16350, -0.23, pch = 19, col = "red") #red is unadjusted
#points(20900, -0.044, pch = 19, col = "brown") #brown is adjusted
#abline(h = -0.044, col = "brown") #replace with study's value

plot(tableint$n, pch =".", col = "red")

#Prosocial
p1 <- c("mcscombinep")
p2 <- c("", "+ mcscombinem")
p3 <- c("", "+ meanincomea", "+ meanincomeb")
p4 <- c("", "+ mwork6")
p5 <- c("", "+ education")
p6 <- c("", "+ pagecont + magecont + cage")
p7 <- c("", "+ csex + siblingb", "+ csex + siblinga")
p8 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p9 <- c("", "+ birthweight + gestation + longill + breastfeed + pregalcohola + pregsmoke", 
        "+ birthweight + gestation + longill + breastfeed + pregalcoholb + pregsmoke")
p10 <- c("", "+ open + consc + extra + agree + neuro", "+ open + consc + extra + agree + neuro + distress + depress")
p11 <- c("", "+ longillm", "+ longillm + rchange + smoke + alcohol + regbedtime + discipline4m")
p12 <- c("", "+ prosocial5", "+ prosocial5 + prosocial4", "+ prosocial5 + prosocial4 + prosocial3")

coeffpro <- data.frame(NULL)
sepro <- data.frame(NULL)
npro <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    for (k in p11) {
                      for (l in p12) {
                        modelpro <- lm(paste("prosocial6 ~", a, b, c, d, e, f, g, h, i, j, k, l), data = Data2)
                        coeffpro <- rbind(coeffpro, summary(modelpro)$coefficients[2, 1])
                        sepro <- rbind(sepro, summary(modelpro)$coefficients[2, 2])
                        npro <- rbind(npro, length(modelpro$model$prosocial6))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeffpro)[1] <- c("involve")
colnames(npro)[1] <- c("n")
colnames(sepro)[1] <- c("se")

tablepro <- cbind(coeffpro, sepro)
tablepro <- cbind(tablepro, npro)

tablepro$UB <- tablepro$involve + 1.96*(tablepro$se)
tablepro$LB <- tablepro$involve - 1.96*(tablepro$se)

tablepro <- tablepro %>% arrange(involve) 
num <- rep(1:nrow(tablepro))
tablepro <- cbind(tablepro, num)

#write.table(tablepro, file = "tablepro.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tablepro <- read.table("tablepro.txt", header = T, sep = "\t", fill = TRUE)

plot(tablepro$num, tablepro$involve, ylim = range(c(tablepro$LB, tablepro$UB)), pch =".", col = "blue",
     ylab = " Paternal Involvement Coefficient Size", xlab = "Models", main = "Note: blue area shows the 95% confidence interval (prosocial difficulties)", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tablepro))) {
  segments(tablepro$num[i], tablepro$LB[i], tablepro$num[i], tablepro$UB[i], 
           col = "light blue", lwd = 0.01)
}
points(tablepro$num, tablepro$involve, pch = ".", col = "blue")
abline(h = 0)

plot(tablepro$n, pch =".", col = "red")

#For occupational aspiration
require(tidyverse)

str(Data2)
Data2$aspiregoalbi <- as.factor(Data2$aspiregoalbi)
table(Data2$aspiregoalbi)

Data2a <- subset(Data2, aspiregoalbi != 2)
table(Data2$aspiregoalbi)
str(Data2a)
table(Data2a$aspiregoalbi) 
#use Data2a for goal, but Data2 for masc

###masc

p1 <- c("", "+ mcscombinem")
p2 <- c("", "+ cage + csex + siblinga", "+ cage + csex + siblingb")
p3 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p4 <- c("", "+ pagecont + magecont")
p5 <- c("", "+ meanincomea + pwork3 + mwork3", "+ meanincomeb + pwork3 + mwork3")
p6 <- c("", "+ education")
p7 <- c("", "+ open + consc + extra + agree + neuro + discipline3m")
p8 <- c("", "+ depress + regbedtime + longillm")
p9 <- c("", "+ birthweight + gestation + longill")
p10 <- c("", "+ breastfeed + pregsmoke + pregalcohola", "+ breastfeed + pregsmoke + pregalcoholb")

coeffmasc <- data.frame(NULL)
semasc <- data.frame(NULL)
nmasc <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    modelmasc <- glm(paste("aspiremascbi ~ mcscombinep", a, b, c, d, e, f, g, h, i, j),
                                     family = binomial, data = Data2)
                    coeffmasc <- rbind(coeffmasc, summary(modelmasc)$coefficients[2, 1])
                    semasc <- rbind(semasc, summary(modelmasc)$coefficients[2, 2])
                    nmasc <- rbind(nmasc, length(modelmasc$model$aspiremascbi))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeffmasc)[1] <- c("Involve")
colnames(semasc)[1] <- c("SE")
colnames(nmasc)[1] <- c("N")
tablemasc <- cbind(coeffmasc, semasc)
tablemasc <- cbind(tablemasc, nmasc)
tablemasc$UB <- tablemasc$Involve + 1.96*(tablemasc$SE)
tablemasc$LB <- tablemasc$Involve - 1.96*(tablemasc$SE)
tablemasc <- tablemasc %>% arrange(Involve) 
num <- rep(1:nrow(tablemasc))
tablemasc <- cbind(tablemasc, num)

#write.table(tablemasc, file = "tablemasc.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tablemasc <- read.table("tablemasc.txt", header = T, sep = "\t", fill = TRUE)

###goal

p1 <- c("", "+ mcscombinem")
p2 <- c("", "+ cage + csex + siblinga", "+ cage + csex + siblingb")
p3 <- c("", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p4 <- c("", "+ pagecont + magecont")
p5 <- c("", "+ meanincomea + pwork3 + mwork3", "+ meanincomeb + pwork3 + mwork3")
p6 <- c("", "+ education")
p7 <- c("", "+ open + consc + extra + agree + neuro + discipline3m")
p8 <- c("", "+ depress + regbedtime + longillm")
p9 <- c("", "+ birthweight + gestation + longill")
p10 <- c("", "+ breastfeed + pregsmoke + pregalcohola", "+ breastfeed + pregsmoke + pregalcoholb")

coeffgoal <- data.frame(NULL)
segoal <- data.frame(NULL)
ngoal <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  for (j in p10) {
                    modelgoal <- glm(paste("aspiregoalbi ~ mcscombinep", a, b, c, d, e, f, g, h, i, j),
                                     family = binomial, data = Data2a)
                    coeffgoal <- rbind(coeffgoal, summary(modelgoal)$coefficients[2, 1])
                    segoal <- rbind(segoal, summary(modelgoal)$coefficients[2, 2])
                    ngoal <- rbind(ngoal, length(modelgoal$model$aspiregoalbi))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeffgoal)[1] <- c("Involve")
colnames(segoal)[1] <- c("SE")
colnames(ngoal)[1] <- c("N")
tablegoal <- cbind(coeffgoal, segoal)
tablegoal <- cbind(tablegoal, ngoal)
tablegoal$UB <- tablegoal$Involve + 1.96*(tablegoal$SE)
tablegoal$LB <- tablegoal$Involve - 1.96*(tablegoal$SE)
tablegoal <- tablegoal %>% arrange(Involve) 
num <- rep(1:nrow(tablegoal))
tablegoal <- cbind(tablegoal, num)

#write.table(tablegoal, file = "tablegoal.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#tablegoal <- read.table("tablegoal.txt", header = T, sep = "\t", fill = TRUE)

###Processing and plotting

tablemasc$OR <- exp(tablemasc$Involve)
tablemasc$ORUB <- exp(tablemasc$UB)
tablemasc$ORLB <- exp(tablemasc$LB)

tablegoal$OR <- exp(tablegoal$Involve)
tablegoal$ORUB <- exp(tablegoal$UB)
tablegoal$ORLB <- exp(tablegoal$LB)

setwd("C:/Users/yongc/Documents/Work Files/University of Oxford/Course Material/Year 2/Thesis/Data To Use")
tablemasc <- read.table("tablemasc.txt", header = T, sep = "\t", fill = TRUE)
tablegoal <- read.table("tablegoal.txt", header = T, sep = "\t", fill = TRUE)

plot(tablemasc$num, tablemasc$OR, ylim = range(c(tablemasc$ORLB, tablemasc$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Feminine Over Masculine Aspiration)", xlab = "Models", 
     main = "Note: blue area shows the 95% confidence interval", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tablemasc))) {
  segments(tablemasc$num[i], tablemasc$ORLB[i], tablemasc$num[i], tablemasc$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(tablemasc$num, tablemasc$OR, pch = ".", col = "blue")
abline(h = 1)

plot(tablegoal$num, tablegoal$OR, ylim = range(c(tablegoal$ORLB, tablegoal$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Intrinsic Over Extrinsic Aspiration)", xlab = "Models", 
     main = "Note: blue area shows the 95% confidence interval", font.main = 1, cex.main = 1)
for (i in c(1:nrow(tablegoal))) {
  segments(tablegoal$num[i], tablegoal$ORLB[i], tablegoal$num[i], tablegoal$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(tablegoal$num, tablegoal$OR, pch = ".", col = "blue")
abline(h = 1)
