###Candidate Number: 1030952

#Set working directory
setwd("Please specify file path if you have downloaded the data")

require(tidyverse)
require(VIM)
require(Hmisc)
require(ISLR)

#Please see code used for each study.

########Study 1:####
#PAPER 1 Rajyaguru et al. 2019 Journal of Psychiatry

#MCS1 Data
MCS1_derived <- read.table("mcs1_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_derived)[1] <- "mcsid"
MCS1_parent <- read.table("mcs1_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS1_hhgrid <- read.table("mcs1_hhgrid.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_hhgrid)[1] <- "mcsid"

#MCS2 Data
MCS2_derived <- read.table("mcs2_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS2_derived)[1] <- "mcsid"
MCS2_parent <- read.table("mcs2_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS3 Data
MCS3_derived <- read.table("mcs3_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS3_derived)[1] <- "mcsid"
MCS3_parent <- read.table("mcs3_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS4 Data
MCS4_derived <- read.table("mcs4_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_derived)[1] <- "mcsid"
MCS4_parent <- read.table("mcs4_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS5 Data
MCS5_derived <- read.table("mcs5_family_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_derived)[1] <- "mcsid"
MCS5_cm_derived <- read.table("mcs5_cm_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_cm_derived)[1] <- "mcsid"
MCS5_parent <- read.table("mcs5_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_parent)[1] <- "mcsid"
MCS5_hhgrid <- read.table("mcs5_hhgrid.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_hhgrid)[1] <- "mcsid"
MCS5_parent_derived <- read.table("mcs5_parent_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_parent_derived)[1] <- "mcsid"

#Clean the hhgrid data for only data on the children themselves
MCS1_hhgrid2 <- subset(MCS1_hhgrid, AHCNUM00 != -1)
MCS5_hhgrid2 <- subset(MCS5_hhgrid, ECNUM00 != -1)

###Create new dataframe with mcsid from MCS1, initial N = 18818
Data1 <- MCS5_derived[, c("mcsid"), drop = F]

#Subsetting for natural mothers
df <- MCS2_derived[, c("mcsid", "BMDREL00")]
df <- subset(df, BMDREL00 == 1)
df$natmother[df$BMDREL00 == 1] <- 1
Data1 <- semi_join(Data1, df[c("mcsid", "natmother")], by = "mcsid") #note semi instead of inner join

###Var: Disciplinary style (Active vs. Withdrawal) and overall discipline use at MCS2
df <- MCS2_parent[, c("mcsid", "bmdism00", "bmdish00", "bmdite00", "bmdiig00", "bmditr00", "bmdibn00", "bmdibr00")]
df <- subset(df, bmdism00 != -1)
df <- subset(df, bmdish00 != -1)
df <- subset(df, bmdite00 != -1)
df <- subset(df, bmdiig00 != -1)
df <- subset(df, bmditr00 != -1)
df <- subset(df, bmdibn00 != -1)
df <- subset(df, bmdibr00 != -1)

#Active (Smacking, Shouting, Telling Off)
df$smack <- df$bmdism00
df$smack[df$bmdism00 == 6] <- 0
df$shout <- df$bmdish00
df$shout[df$bmdish00 == 6] <- 0
df$telloff <- df$bmdite00
df$telloff[df$bmdite00 == 6] <- 0

df$active <- df$smack + df$shout + df$telloff
table(df$active)

#Withdrawal (Ignoring, Removal of Treats, Sending to Bedroom)
df$ignore <- df$bmdiig00
df$ignore[df$bmdiig00 == 6] <- 0
df$remove <- df$bmditr00
df$remove[df$bmditr00 == 6] <- 0
df$sendto <- df$bmdibn00
df$sendto[df$bmdibn00 == 6] <- 0

df$withdrawal <- df$ignore + df$remove + df$sendto
table(df$withdrawal)

#for continuous var (Bribe)
df$bribe <- df$bmdibr00
df$bribe[df$bmdibr00 == 6] <- 0

df$punishmentcont <- df$active + df$withdrawal + df$bribe
table(df$punishmentcont)

Data1 <- full_join(Data1, df[c("mcsid", "active", "withdrawal", "punishmentcont")], by = "mcsid")

###Var: Parent-reported SDQ at MCS5
df <- MCS5_cm_derived[, c("mcsid", "EDEBDTAA")]
df <- subset(df, EDEBDTAA >= 0)

df$sdqcont <- df$EDEBDTAA
table(df$sdqcont)

df$sdqdummy[df$EDEBDTAA >= 17] <- 1
df$sdqdummy[df$EDEBDTAA < 17] <- 0
table(df$sdqdummy)

Data1 <- full_join(Data1, df[c("mcsid", "sdqcont", "sdqdummy")], by = "mcsid")

###Var: Infant Temperament at MCS1 (to sum all the variables)
id <- c("mcsid")
names <- c("amhapna0", "amunfaa0", "ambrusa0", "amfeeda0", "aminjua0", "ambatha0", "amwarya0", "ambshya0", "amfreta0", "amsleea0",
           "ammilka0", "amsltia0", "amnapsa0", "amsofoa0")
df <- MCS1_parent[, c(id, names)]

df <- subset(df, amhapna0 > 0) #Mood Questions (5)
df$mooda <- df$amhapna0
df$mooda[df$amhapna0 == 6] <- 0
df <- subset(df, amunfaa0 > 0)
df$moodb <- df$amunfaa0
df$moodb[df$amunfaa0 == 6] <- 0
df <- subset(df, ambrusa0 > 0)
df$moodc <- df$ambrusa0
df$moodc[df$ambrusa0 == 6] <- 0
df <- subset(df, amfeeda0 > 0)
df$moodd <- df$amfeeda0
df$moodd[df$amfeeda0 == 6] <- 0
df <- subset(df, aminjua0 > 0)
df$moode <- df$aminjua0
df$moode[df$aminjua0 == 6] <- 0

df <- subset(df, ambatha0 > 0) #Approach/withdrawal Questions (3)
df$approacha <- df$ambatha0
df$approacha[df$ambatha0 == 6] <- 0
df <- subset(df, amwarya0 > 0)
df$approachb <- df$amwarya0
df$approachb[df$amwarya0 == 6] <- 0
df <- subset(df, ambshya0 > 0)
df$approachc <- df$ambshya0
df$approachc[df$ambshya0 == 6] <- 0

df <- subset(df, amfreta0 > 0) #Adaptability Questions (3)
df$adapta <- df$amfreta0
df$adapta[df$amfreta0 == 6] <- 0
df <- subset(df, amsleea0 > 0)
df$adaptb <- df$amsleea0
df$adaptb[df$amsleea0 == 6] <- 0
df <- subset(df, ammilka0 > 0)
df$adaptc <- df$ammilka0
df$adaptc[df$ammilka0 == 6] <- 0

df <- subset(df, amsltia0 > 0) #Regularity Questions (3)
df$regulara <- df$amsltia0
df$regulara[df$amsltia0 == 6] <- 0
df <- subset(df, amnapsa0 > 0)
df$regularb <- df$amnapsa0
df$regularb[df$amnapsa0 == 6] <- 0
df <- subset(df, amsofoa0 > 0)
df$regularc <- df$amsofoa0
df$regularc[df$amsofoa0 == 6] <- 0

df$inftemper <- df$mooda + df$moodb + df$moodc + df$moodd + df$moode + df$approacha + df$approachb + df$approachc +
  df$adapta + df$adaptb + df$adaptc + df$regulara + df$regularb + df$regularc
table(df$inftemper)
summary(df$inftemper)

df$inftemperdummy[df$inftemper < 45] <- 0
df$inftemperdummy[df$inftemper >= 45] <- 1

Data1 <- full_join(Data1, df[c("mcsid", "inftemper", "inftemperdummy")], by = "mcsid")

###Var: Maternal age (paper has used grouped age AMDGAB00 as opposed to continous age AMDAGB00)
df <- MCS1_derived[, c("mcsid", "AMDGAB00", "AMDAGB00")]
df <- subset(df, AMDGAB00 > 0)
table(df$AMDGAB00)
for (i in c(1:4)) {
  df$agecat[df$AMDGAB00 == i] <- i - 1
}
table(df$agecat)

table(df$AMDAGB00)
df$agecont <- df$AMDAGB00

Data1 <- full_join(Data1, df[c("mcsid", "agecat", "agecont")], by = "mcsid")
table(Data1$agecat)
table(Data1$agecont)

###Var: Child sex
df <- MCS5_hhgrid2[, c("mcsid", "EHCSEX00")]
table(df$EHCSEX00) #1 = M, 2 = F
df$csex[df$EHCSEX00 == 1] <- 1
df$csex[df$EHCSEX00 == 2] <- 0
Data1 <- full_join(Data1, df[c("mcsid", "csex")], by = "mcsid")
table(Data1$csex)

###Var: Maternal depression
df <- MCS5_parent[, c("mcsid", "EPDEAN00")]
df <- subset(df, EPDEAN00 == 1 | EPDEAN00 == 2) #1 = yes, 2 = no
df$mdepress[df$EPDEAN00 == 2] <- 0
df$mdepress[df$EPDEAN00 == 1] <- 1
table(df$mdepress)

length(unique(df$mcsid))
length(unique(df$mcsid)) == nrow(df)

n_occur <- data.frame(table(df$mcsid))
n_occur[n_occur$Freq > 1, ]
str(n_occur)

dupe <- n_occur[n_occur$Freq > 1, ]
colnames(dupe)[1] <- "mcsid"
str(dupe)

df <- anti_join(df, dupe, by = "mcsid")

Data1 <- full_join(Data1, df[c("mcsid", "mdepress")], by = "mcsid")

###Var: Maternal psycho-social distress
df <- MCS1_parent[, c("mcsid", 
                      "amtire00", "amdepr00", "amworr00", "amrage00", "amscar00", "amupse00", "amkeyd00", "amnerv00", "amhera00")]
df <- subset(df, amtire00 > 0)
df$mdistressa[df$amtire00 == 2] <- 0
df$mdistressa[df$amtire00 == 1] <- 1
df <- subset(df, amdepr00 > 0)
df$mdistressb[df$amdepr00 == 2] <- 0
df$mdistressb[df$amdepr00 == 1] <- 1
df <- subset(df, amworr00 > 0)
df$mdistressc[df$amworr00 == 2] <- 0
df$mdistressc[df$amworr00 == 1] <- 1
df <- subset(df, amrage00 > 0)
df$mdistressd[df$amrage00 == 2] <- 0
df$mdistressd[df$amrage00 == 1] <- 1
df <- subset(df, amscar00 > 0)
df$mdistresse[df$amscar00 == 2] <- 0
df$mdistresse[df$amscar00 == 1] <- 1
df <- subset(df, amupse00 > 0)
df$mdistressf[df$amupse00 == 2] <- 0
df$mdistressf[df$amupse00 == 1] <- 1
df <- subset(df, amkeyd00 > 0)
df$mdistressg[df$amkeyd00 == 2] <- 0
df$mdistressg[df$amkeyd00 == 1] <- 1
df <- subset(df, amnerv00 > 0)
df$mdistressh[df$amnerv00 == 2] <- 0
df$mdistressh[df$amnerv00 == 1] <- 1
df <- subset(df, amhera00 > 0)
df$mdistressi[df$amhera00 == 2] <- 0
df$mdistressi[df$amhera00 == 1] <- 1

df$distress <- df$mdistressa + df$mdistressb + df$mdistressc + df$mdistressd + df$mdistresse + df$mdistressf +
  df$mdistressg + df$mdistressh + df$mdistressi
table(df$distress)

df$distressdummy[df$distress >= 4] <- 1
df$distressdummy[df$distress < 4] <- 0

Data1 <- full_join(Data1, df[c("mcsid", "distress", "distressdummy")], by = "mcsid")
table(Data1$distressdummy)

###Var: Maternal self-esteem
df <- MCS1_parent[, c("mcsid", "amsati00", "amgood00", "amwell00", "amusel00", "amfail00", "amposi00")]
df <- subset(df, amposi00 > 0)
df$esteema[df$amposi00 == 1] <- 3
df$esteema[df$amposi00 == 2] <- 2
df$esteema[df$amposi00 == 3] <- 1
df$esteema[df$amposi00 == 4] <- 0
df <- subset(df, amwell00 > 0)
df$esteemb[df$amwell00 == 1] <- 3
df$esteemb[df$amwell00 == 2] <- 2
df$esteemb[df$amwell00 == 3] <- 1
df$esteemb[df$amwell00 == 4] <- 0
df <- subset(df, amsati00 > 0)
df$esteemc[df$amsati00 == 1] <- 3
df$esteemc[df$amsati00 == 2] <- 2
df$esteemc[df$amsati00 == 3] <- 1
df$esteemc[df$amsati00 == 4] <- 0
df <- subset(df, amusel00 > 0)
df$esteemd[df$amusel00 == 1] <- 0
df$esteemd[df$amusel00 == 2] <- 1
df$esteemd[df$amusel00 == 3] <- 2
df$esteemd[df$amusel00 == 4] <- 3
df <- subset(df, amfail00 > 0)
df$esteeme[df$amfail00 == 1] <- 0
df$esteeme[df$amfail00 == 2] <- 1
df$esteeme[df$amfail00 == 3] <- 2
df$esteeme[df$amfail00 == 4] <- 3
df <- subset(df, amgood00 > 0)
df$esteemf[df$amgood00 == 1] <- 0
df$esteemf[df$amgood00 == 2] <- 1
df$esteemf[df$amgood00 == 3] <- 2
df$esteemf[df$amgood00 == 4] <- 3

df$esteem <- df$esteema + df$esteemb + df$esteemc + df$esteemd + df$esteeme + df$esteemf
table(df$esteem)

df$esteemdummy[df$esteem < 9] <- 1
df$esteemdummy[df$esteem >= 9] <- 0
table(df$esteemdummy)

Data1 <- full_join(Data1, df[c("mcsid", "esteem", "esteemdummy")], by = "mcsid")

###Var: Maternal childbearing parity
df <- MCS5_derived[, c("mcsid", "EDOTHS00", "EDNSIB00")]
table(df$EDNSIB00)
df$parity[df$EDNSIB00 == 2] <- 0
df$parity[df$EDNSIB00 == 1] <- 1
Data1 <- full_join(Data1, df[c("mcsid", "parity")], by = "mcsid")

###Var: Maternal education -- amacqu00 or DMDNVQ00? Yes.
df <- MCS1_parent[, c("mcsid", "amacqu00")]
df <- subset(df, amacqu00 > 0)
table(df$amacqu00)
df$meducation1[df$amacqu00 == 1] <- 0
df$meducation1[df$amacqu00 == 2] <- 1
df$meducation1[df$amacqu00 == 3] <- 2
df$meducation1[df$amacqu00 == 4] <- 3
df$meducation1[df$amacqu00 == 5] <- 4
df$meducation1[df$amacqu00 == 6] <- 5
df$meducation1[df$amacqu00 == 95] <- 6
df$meducation1[df$amacqu00 == 96] <- 7
table(df$meducation1)

Data1 <- full_join(Data1, df[c("mcsid", "meducation1")], by = "mcsid") 

df <- MCS4_derived[, c("mcsid", "DMDNVQ00")]
table(df$DMDNVQ00)
df <- subset(df, DMDNVQ00 >= 0)
df$meducation4[df$DMDNVQ00 == 1] <- 0
df$meducation4[df$DMDNVQ00 == 2] <- 1
df$meducation4[df$DMDNVQ00 == 3] <- 2
df$meducation4[df$DMDNVQ00 == 4] <- 3
df$meducation4[df$DMDNVQ00 == 5] <- 4
df$meducation4[df$DMDNVQ00 == 95] <- 5
df$meducation4[df$DMDNVQ00 == 96] <- 6
table(df$meducation4)

Data1 <- full_join(Data1, df[c("mcsid", "meducation4")], by = "mcsid")

###Var: Maternal socio-economic status (study uses 5-category grouping)
df <- MCS5_parent_derived[, c("mcsid", "EDD05S00", "EDD07S00", "EDD13S00")]
table(df$EDD05S00)
table(df$EDD07S00)
table(df$EDD13S00)
df <- subset(df, EDD05S00 != -1)

for (i in c(1:5)) {
  df$socstatus5[df$EDD05S00 == i] <- i - 1
}
table(df$socstatus5)
for (i in c(1:7)) {
  df$socstatus7[df$EDD07S00 == i] <- i - 1
}
table(df$socstatus7)
for (i in c(1:13)) {
  df$socstatus13[df$EDD13S00 == i] <- i - 1
}
table(df$socstatus13)

Data1 <- full_join(Data1, df[c("mcsid", "socstatus5", "socstatus7", "socstatus13")], by = "mcsid")

Data1$agecat <- as.factor(Data1$agecat)
Data1$meducation1 <- as.factor(Data1$meducation1)
Data1$meducation4 <- as.factor(Data1$meducation4)
Data1$socstatus5 <- as.factor(Data1$socstatus5)
Data1$socstatus7 <- as.factor(Data1$socstatus7)
Data1$socstatus13 <- as.factor(Data1$socstatus13)

#Initial replications
s1m1 <- lm(sdqcont ~ active + withdrawal, data = Data1)
s1m3 <- lm(sdqcont ~ active + withdrawal + punishmentcont + inftemperdummy + agecat +
             mdepress + esteemdummy + distressdummy + meducation1 + 
             parity + csex + socstatus5, data = Data1)
summary(s1m1)
summary(s1m3) 

#CI
confint(s1m1)
confint(s1m3)

#N
length(s1m1$model$sdqcont)
length(s1m3$model$sdqcont)

#Make permutation sets to loop within for multiverse analysis
p1 <- c("", "+ punishmentcont")
p2 <- c("", "+ inftemperdummy", "+ inftemper")
p3 <- c("", "+ agecat", "+ agecont")
p4 <- c("", "+ csex")
p5 <- c("", "+ mdepress")
p6 <- c("", "+ distress", "+ distressdummy")
p7 <- c("", "+ esteem", "+ esteemdummy")
p8 <- c("", "+ parity")
p9 <- c("", "+ meducation1", "+ meducation4")
p10 <- c("", "+ socstatus5", "+ socstatus7", "+ socstatus13")

coeff1a <- data.frame(NULL)
coeff1b <- data.frame(NULL)
confint1a <- data.frame(NULL)
confint1b <- data.frame(NULL)
n1 <- data.frame(NULL)

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
                    model1 <- lm(paste("sdqcont ~ active + withdrawal", a, b, c, d, e, f, g, h, i, j),
                                 data = Data1)
                    coeff1a <- rbind(coeff1a, summary(model1)$coefficients[2, 1])
                    coeff1b <- rbind(coeff1b, summary(model1)$coefficients[3, 1])
                    confint1a <- rbind(confint1a, confint(model1)[2, 1:2])
                    confint1b <- rbind(confint1b, confint(model1)[3, 1:2])
                    n1 <- rbind(n1, length(model1$model$sdqcont))
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

colnames(coeff1a)[1] <- c("Active")
colnames(coeff1b)[1] <- c("Withdrawal")
colnames(confint1a) <- c("Active.LB", "Active.UB")
colnames(confint1b) <- c("Withdrawal.LB", "Withdrawal.UB")
colnames(n1)[1] <- c("N")

table1a <- cbind(coeff1a, confint1a)
table1a <- cbind(table1a, n1)
table1a <- table1a %>% arrange(Active) #this line sorts the table
num <- rep(1:nrow(table1a))
table1a <- cbind(table1a, num)

table1b <- cbind(coeff1b, confint1b)
table1b <- cbind(table1b, n1)
table1b <- table1b %>% arrange(Withdrawal)
num <- rep(1:nrow(table1b))
table1b <- cbind(table1b, num)

plot(table1a$num, table1a$Active, ylim = range(c(table1a$`Active.LB`, table1a$`Active.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Active)", xlab = "Models")
for (i in c(1:nrow(table1a))) {
  segments(table1a$num[i], table1a$`Active.LB`[i], table1a$num[i], table1a$`Active.UB`[i], col = "light blue", lwd = 0.01)
}
points(table1a$num, table1a$Active, pch = ".", col = "blue")
abline(h = 0)
abline(h = 1.11, col = "red") #replace with study's value

plot(table1b$num, table1b$Withdrawal, ylim = range(c(table1b$`Withdrawal.LB`, table1b$`Withdrawal.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Withdrawal)", xlab = "Models")
for (i in c(1:nrow(table1b))) {
  segments(table1b$num[i], table1b$`Withdrawal.LB`[i], table1b$num[i], table1b$`Withdrawal.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table1a$num, table1b$Withdrawal, pch = ".", col = "blue")
abline(h = 0)
abline(h = 0.84, col = "red") #replace with study's value

plot(table1a$N)

########Study 2:####
#PAPER 2 Zilanwala et al. 2015 Social Science and Medicine

#MCS4 Data
MCS4_derived <- read.table("mcs4_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_derived)[1] <- "mcsid"
MCS4_parent <- read.table("mcs4_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS4_geographic <- read.table("mcs4_geographically_linked_data.tab", header = T, sep = "\t", fill = TRUE)
MCS4_hhgrid <- read.table("mcs4_hhgrid.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_hhgrid)[1] <- "mcsid"

#MCS3 Data
MCS3_parent <- read.table("mcs3_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS2 Data (for date of migration to UK)
MCS2_parent <- read.table("mcs2_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS1 Data
MCS1_derived <- read.table("mcs1_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_derived)[1] <- "mcsid"
MCS1_parent <- read.table("mcs1_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#Subsetting for singleborns
#use the distinct() function to ensure there are no repeat mcsid's
table(MCS4_hhgrid$DHCNUM00)
MCS4_hhgrid2 <- subset(MCS4_hhgrid, DHCNUM00 != -1)
table(MCS4_hhgrid2$DHCNUM00)
MCS4_hhgrid_distinct <- distinct(MCS4_hhgrid2, mcsid, .keep_all = TRUE)
table(MCS4_hhgrid_distinct$DHCNUM00)

#Start the dataframe Data2
Data2 <- MCS4_hhgrid_distinct[, "mcsid", drop = F]

###Key Var: SDQ at MCS4 (Age 7)
df <- MCS4_derived[, c("mcsid", "DDEBDTA0")]
table(df$DDEBDTA0)
df <- subset(df, DDEBDTA0 >= 0)

df$sdqcont <- df$DDEBDTA0
table(df$sdqcont)

df$sdqdummy[df$DDEBDTA0 >= 17] <- 1
df$sdqdummy[df$DDEBDTA0 < 17] <- 0
table(df$sdqdummy)

Data2 <- full_join(Data2, df[c("mcsid", "sdqcont", "sdqdummy")], by = "mcsid")

###Key Var: Child Ethnicity at MCS4 (Note: study used largest group (White children) as reference group)
df <- MCS4_derived[, c("mcsid", "DDC06EA0", "DDC08EA0", "DDC11EA0")]
table(df$DDC06EA0)
table(df$DDC08EA0)
table(df$DDC11EA0)
df <- subset(df, DDC08EA0 > 0)

for (i in c(1:8)) {
  df$cethnic8a[df$DDC08EA0 == i] <- i - 1
}
table(df$cethnic8a)
df$cethnic8b <- df$cethnic8a
df$cethnic8b[df$cethnic8b == 1] <- 7 #2 = Mixed race to be put into Other group (grp 7)
table(df$cethnic8b) #8b does not put Mixed into Other, 8a does

for (i in c(1:6)) {
  df$cethnic6a[df$DDC06EA0 == i] <- i - 1
}
table(df$cethnic6a)
df$cethnic6b <- df$cethnic6a
df$cethnic6b[df$cethnic6b == 1] <- 5
table(df$cethnic6b) 

for (i in c(1:11)) {
  df$cethnic11a[df$DDC11EA0 == i] <- i - 1
}
table(df$cethnic11a)
df$cethnic11b <- df$cethnic11a
df$cethnic11b[df$cethnic11b == 1] <- 10 
table(df$cethnic11b) 

Data2 <- full_join(Data2, df[c("mcsid", "cethnic8a", "cethnic8b", "cethnic6a", "cethnic6b",
                               "cethnic11a", "cethnic11b")], by = "mcsid")

###Var to subset: ADHD and Asperger's
df <- MCS4_parent[, c("mcsid", "dmadhda0", "dmautsa0")]
table(df$dmadhda0)
table(df$dmautsa0)
df <- subset(df, dmadhda0 == 2)
df <- subset(df, dmautsa0 == 2)
df$adhd[df$dmadhda0 == 2] <- 0
df$aspergers[df$dmautsa0 == 2] <- 0
any(is.na(df) == TRUE)
Data2 <- full_join(Data2, df[c("mcsid", "adhd", "aspergers")], by = "mcsid")

###Var: Child gender DHCSEX00
df <- MCS4_hhgrid_distinct[, c("mcsid", "DHCSEX00")]
table(df$DHCSEX00)
df$csex[df$DHCSEX00 == 1] <- 1 #M
df$csex[df$DHCSEX00 == 2] <- 0 #F
Data2 <- full_join(Data2, df[c("mcsid", "csex")], by = "mcsid")

###Var: Child age (study says mean age = 7.23 years) DHCAGE00 -- in days
df <- MCS4_hhgrid_distinct[, c("mcsid", "DHCAGE00", "DHCDBY00")]
table(df$DHCDBY00)
any(df$DHCAGE00 <= 0)
df$cagedays <- df$DHCAGE00
df$cageyears[df$DHCDBY00 == 2000] <- 0
df$cageyears[df$DHCDBY00 == 2001] <- 1
df$cageyears[df$DHCDBY00 == 2002] <- 2
Data2 <- full_join(Data2, df[c("mcsid", "cagedays", "cageyears")], by = "mcsid") #drop 0 N

###Var: Maternal age at MCS4 (new var not in original study) DMDAGI00 DMDGAI00
df <- MCS4_derived[, c("mcsid", "DMDAGI00", "DMDGAI00")]
table(df$DMDAGI00)
table(df$DMDGAI00)
df$magecont <- df$DMDAGI00
for (i in c(2:4)) {
  df$magecat[df$DMDGAI00 == i] <- i - 2
}
table(df$magecat)
Data2 <- full_join(Data2, df[c("mcsid", "magecont", "magecat")], by = "mcsid")

###Var: Is English primary language? DDHLAN00
df <- MCS4_derived[, c("mcsid", "DDHLAN00")]
table(df$DDHLAN00)
df$"englishdummy" <- NA
for (i in c(1:5)) {
  if (i <= 2) {
    df$"englishdummy"[df$DDHLAN00 == i] <- 1
  } else {
    df$"englishdummy"[df$DDHLAN00 == i] <- 0
  }
}
table(df$englishdummy)
any(is.na(df) == TRUE)
Data2 <- full_join(Data2, df[c("mcsid", "englishdummy")], by = "mcsid")

###Var: Equivalised continuous household income (note: take log) DOEDE000
df <- MCS4_derived[, c("mcsid", "DOEDE000")]
df <- subset(df, DOEDE000 >= 0)
df$logincome <- log(df$DOEDE000)
Data2 <- full_join(Data2, df[c("mcsid", "logincome")], by = "mcsid")

###Var: Highest parental educational qualification (NVQ equivalence scale) DMDNVQ00
df <- MCS4_derived[, c("mcsid", "DMDNVQ00")]
table(df$DMDNVQ00)
df <- subset(df, DMDNVQ00 >= 0)
df$meducation[df$DMDNVQ00 == 1] <- 0
df$meducation[df$DMDNVQ00 == 2] <- 1
df$meducation[df$DMDNVQ00 == 3] <- 2
df$meducation[df$DMDNVQ00 == 4] <- 3
df$meducation[df$DMDNVQ00 == 5] <- 4
df$meducation[df$DMDNVQ00 == 95] <- 5
df$meducation[df$DMDNVQ00 == 96] <- 6
table(df$meducation)
Data2 <- full_join(Data2, df[c("mcsid", "meducation")], by = "mcsid")

###Var: Index of Multiple Deprivation (IMD) in quintiles DIMDSCOE DIMDSCON DIMDSCOW DIMDSCOS #not in dataset???
df <- MCS4_geographic[, c("mcsid", "dactry00", "DIMDSCOE", "DIMDSCON")]
table(df$dactry00) #1 = Eng, 2 = Wal, 3 = Scot, 4 = NI
table(df$DIMDSCOE)
table(df$DIMDSCON)

sum(is.na(df$DIMDSCOE) == T)
sum(is.na(df$DIWIMDSC) == T)
sum(is.na(df$DISIMDSC) == T)
sum(is.na(df$DIMDSCON) == T)

any(is.na(df) == TRUE)
df$deprivequint[df$DIMDSCOE == 1] <- 0
df$deprivequint[df$DIMDSCOE == 2] <- 0
df$deprivequint[df$DIMDSCOE == 3] <- 1
df$deprivequint[df$DIMDSCOE == 4] <- 1
df$deprivequint[df$DIMDSCOE == 5] <- 2
df$deprivequint[df$DIMDSCOE == 6] <- 2
df$deprivequint[df$DIMDSCOE == 7] <- 3
df$deprivequint[df$DIMDSCOE == 8] <- 3
df$deprivequint[df$DIMDSCOE == 9] <- 4
df$deprivequint[df$DIMDSCOE == 10] <- 4
table(df$deprivequint)
any(is.na(df) == T)

Data2 <- full_join(Data2, df[c("mcsid", "deprivequint")], by = "mcsid") #this var to be dropped

###Var: Mother's employment status MCS4 (working full-time, working parttime, and not working)
df <- MCS4_derived[, c("mcsid", "DMDWRK00")] #1 = in work, 2 = not in work
table(df$DMDWRK00)
df$mwork[df$DMDWRK00 == 1] <- 0
df$mwork[df$DMDWRK00 == 2] <- 1
Data2 <- full_join(Data2, df[c("mcsid", "mwork")], by = "mcsid")

###Var: Frequency of racist insults/attacks MCS3 (Age 5) -- binary: not at all/not very common vs. fairly/very common
df <- MCS3_parent[, c("mcsid", "cmarrc00")]
table(df$cmarrc00)

df$raceattack[df$cmarrc00 == 1] <- 1
df$raceattack[df$cmarrc00 == 2] <- 1
df$raceattack[df$cmarrc00 == 3] <- 0
df$raceattack[df$cmarrc00 == 4] <- 0
df$raceattack[df$cmarrc00 < 0] <- NA
table(df$raceattack)

#Data2 <- full_join(Data2, df, by = "mcsid") #try left_join?
Data2 <- left_join(Data2, df[, c("mcsid", "raceattack")], by = "mcsid")

###Var: Maternal psychological distress (K6) at MCS4
df <- MCS4_derived[, c("mcsid", "DMKESS00")]
table(df$DMKESS00)
df <- subset(df, DMKESS00 >= 0)
#df$"DMKESS00"[df$DMKESS00 == -1] <- NA
df$distress <- df$DMKESS00
table(df$distress)
Data2 <- full_join(Data2, df[c("mcsid", "distress")], by = "mcsid")

###Var: Parental basic skills difficulties at MCS1 amread00 amform00 ammath00
df <- MCS1_parent[, c("mcsid", "amread00", "amform00", "ammath00")]
table(df$amread00)
df$amread00[df$amread00 < 0] <- NA
for (i in c(1:3)) {
  df$amread00[df$amread00 == i] <- i - 1
}
df$amform00[df$amform00 < 0] <- NA
for (i in c(1:3)) {
  df$amform00[df$amform00 == i] <- i - 1
}
df$ammath00[df$ammath00 < 0] <- NA
for (i in c(1:3)) {
  df$ammath00[df$ammath00 == i] <- i - 1
}
df$basicskills <- df$amread00 + df$amform00 + df$ammath00
table(df$basicskills)

Data2 <- left_join(Data2, df[c("mcsid", "basicskills")], by = "mcsid")
#Data2 <- full_join(Data2, df, by = "mcsid") #try left_join

###Var: Parental discipline strategies at MCS4 (7 items)
df <- MCS4_parent[, c("mcsid", "dmdiiga0", "dmdisma0", "dmdisha0", "dmdibna0", "dmditra0", "dmditea0", "dmdibra0")]
table(df$dmdiiga0)

df$dmdiiga0[df$dmdiiga0 < 0] <- NA
df$dmdisma0[df$dmdisma0 < 0] <- NA
df$dmdisha0[df$dmdisha0 < 0] <- NA
df$dmdibna0[df$dmdibna0 < 0] <- NA
df$dmditra0[df$dmditra0 < 0] <- NA
df$dmditea0[df$dmditea0 < 0] <- NA
df$dmdibra0[df$dmdibra0 < 0] <- NA

df$discipline <- df$dmdiiga0 + df$dmdisma0 + df$dmdisha0 + df$dmdibna0 + df$dmditra0 + df$dmditea0 + df$dmdibra0
table(df$discipline)
#df <- subset(df, discipline > 0)
#df$discipline[df$discipline < 0] <- NA
Data2 <- left_join(Data2, df[, c("mcsid", "discipline")], by = "mcsid")

###Var: Frequency of learning activities (reading to the child and helping with reading, writing, and maths)
#df <- MCS4_parent[, c("mcsid", "dmreofa0", "dmalwha0", "dmhlwxa0", "dmhlnca0")] #this is wrong
df <- MCS4_parent[, c("mcsid", "dmreofa0", "dmalrda0", "dmhlwra0", "dmhlcoa0")]
table(df$dmreofa0)
table(df$dmalrda0) #1 = yes help, 2 = no help
table(df$dmhlwra0)
table(df$dmhlcoa0)

df$readtocm[df$dmreofa0 < 0] <- NA
df$readtocm[df$dmreofa0 == 6] <- 0
df$readtocm[df$dmreofa0 == 5] <- 1
df$readtocm[df$dmreofa0 == 4] <- 2
df$readtocm[df$dmreofa0 == 3] <- 3
df$readtocm[df$dmreofa0 == 2] <- 4
df$readtocm[df$dmreofa0 == 1] <- 5

df$readtodummy[df$readtocm <= 2] <- 0
df$readtodummy[df$readtocm >= 3] <- 1

df$helpwrite[df$dmalrda0 < 0] <- NA
df$helpwrite[df$dmalrda0 == 1] <- 1
df$helpwrite[df$dmalrda0 == 2] <- 0

df$helpmaths[df$dmhlwra0 < 0] <- NA
df$helpmaths[df$dmhlwra0 == 1] <- 1
df$helpmaths[df$dmhlwra0 == 2] <- 0

df$helpread[df$dmhlcoa0 < 0] <- NA
df$helpread[df$dmhlcoa0 == 1] <- 1
df$helpread[df$dmhlcoa0 == 2] <- 0

table(df$readtodummy)
table(df$helpread)

df$learn <- df$readtodummy + df$helpread + df$helpwrite + df$helpmaths
table(df$learn)

Data2 <- full_join(Data2, df[, c("mcsid", "learn")], by = "mcsid")

###Var: Regular bedtime on weekdays -- binary (always or usually vs. sometimes or never) dmberea0
df <- MCS4_parent[, c("mcsid", "dmberea0")]
table(df$dmberea0)
df$dmberea0[df$dmberea0 < 0] <- NA
df$regbedtime[df$dmberea0 == 1] <- 0
df$regbedtime[df$dmberea0 == 2] <- 0
df$regbedtime[df$dmberea0 == 3] <- 1
df$regbedtime[df$dmberea0 == 4] <- 1
table(df$regbedtime)
Data2 <- full_join(Data2, df[c("mcsid", "regbedtime")], by = "mcsid")

###Var: maternal immigration status
df <- MCS2_parent[, c("mcsid", "bmrebo00", "bmrewn00")]
table(df$bmrebo00)
table(df$bmrewn00)

df$mmigration[df$bmrebo00 == 1] <- 0
df$mmigration[df$bmrewn00 >= 1960 & df$bmrewn00 < 1970] <- 1
df$mmigration[df$bmrewn00 >= 1970 & df$bmrewn00 < 1980] <- 2
df$mmigration[df$bmrewn00 >= 1980 & df$bmrewn00 < 1990] <- 3
df$mmigration[df$bmrewn00 >= 1990 & df$bmrewn00 < 2000] <- 4
df$mmigration[df$bmrewn00 >= 2000] <- 5
table(df$mmigration)

sum(is.na(df$mmigration) == T)

Data2 <- full_join(Data2, df[c("mcsid", "mmigration")], by = "mcsid")

Data2$cethnic6a <- as.factor(Data2$cethnic6a)
Data2$cethnic6b <- as.factor(Data2$cethnic6b)
Data2$cethnic8a <- as.factor(Data2$cethnic8a)
Data2$cethnic8b <- as.factor(Data2$cethnic8b)
Data2$cethnic11a <- as.factor(Data2$cethnic11a)
Data2$cethnic11b <- as.factor(Data2$cethnic11b)
Data2$cageyears <- as.factor(Data2$cageyears)
Data2$magecat <- as.factor(Data2$magecat)
Data2$meducation <- as.factor(Data2$meducation)
Data2$deprivequint <- as.factor(Data2$deprivequint)
Data2$distress <- as.factor(Data2$distress)
Data2$basicskills <- as.factor(Data2$basicskills)
Data2$learn <- as.factor(Data2$learn)
Data2$mmigration <- as.factor(Data2$mmigration)

#Code to subset 'deprivequint' into Data2drop
str(Data2)
Data2drop <- Data2[, -c(28)] #28 is deprivequint
str(Data2drop)
Data2drop_sub <- Data2drop
Data2drop_sub <- Data2drop_sub[complete.cases(Data2drop_sub), ]

mice_plot <- aggr(Data2, col = c("navyblue", "yellow"),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(Data2), cex.axis = 0.7,
                  gap = 3, ylab = c("Missing Data", "Pattern"))
mice_plot2 <- aggr(Data2drop, col = c("navyblue", "yellow"),
                   numbers = TRUE, sortVars = TRUE,
                   labels = names(Data2drop), cex.axis = 0.7,
                   gap = 3, ylab = c("Missing Data", "Pattern")) #use this to check if missing

Data2drop_impute <- aregImpute(~ sdqcont + factor(csex) + cagedays + factor(englishdummy) + logincome + factor(meducation) + 
                                 factor(mwork) + factor(raceattack) + distress + factor(basicskills) + discipline + factor(regbedtime) + 
                                 learn + factor(mmigration),
                               data = Data2drop, n.impute = 5)
Data2drop_impute

Data2drop_imp <- Data2drop

Data2drop_imp$mmigration <- ifelse(is.na(Data2drop_imp$mmigration), Data2drop_impute$imputed$mmigration, Data2drop_imp$mmigration)
Data2drop_imp$englishdummy <- ifelse(is.na(Data2drop_imp$englishdummy), Data2drop_impute$imputed$englishdummy, Data2drop_imp$englishdummy)
Data2drop_imp$logincome <- ifelse(is.na(Data2drop_imp$logincome), Data2drop_impute$imputed$logincome, Data2drop_imp$logincome)
Data2drop_imp$meducation <- ifelse(is.na(Data2drop_imp$meducation), Data2drop_impute$imputed$meducation, Data2drop_imp$meducation)
Data2drop_imp$mwork <- ifelse(is.na(Data2drop_imp$mwork), Data2drop_impute$imputed$mwork, Data2drop_imp$mwork)
Data2drop_imp$raceattack <- ifelse(is.na(Data2drop_imp$raceattack), Data2drop_impute$imputed$raceattack, Data2drop_imp$raceattack)
Data2drop_imp$distress <- ifelse(is.na(Data2drop_imp$distress), Data2drop_impute$imputed$distress, Data2drop_imp$distress)
Data2drop_imp$basicskills <- ifelse(is.na(Data2drop_imp$basicskills), Data2drop_impute$imputed$basicskills, Data2drop_imp$basicskills)
Data2drop_imp$learn <- ifelse(is.na(Data2drop_imp$learn), Data2drop_impute$imputed$learn, Data2drop_imp$learn)
Data2drop_imp$discipline <- ifelse(is.na(Data2drop_imp$discipline), Data2drop_impute$imputed$discipline, Data2drop_imp$discipline)

#initial replication
s2m1 <- lm(sdqcont ~ factor(cethnic8b) + cagedays + factor(csex), data = Data2drop_imp)
s2m2 <- lm(sdqcont ~ factor(cethnic8b) + cagedays + factor(csex) + factor(mmigration) + factor(meducation) +
             factor(englishdummy) + factor(mwork) + factor(raceattack) + factor(regbedtime) + factor(learn) + 
             distress + logincome + factor(basicskills) + discipline,
           data = Data2drop_imp)
summary(s2m1)
summary(s2m2)

confint(s2m1)
confint(s2m2)

length(s2m1$model$sdqcont)
length(s2m2$model$sdqcont)

p1 <- c("+ cagedays", "+ cageyears")
p2 <- c("+ csex")
p3 <- c("", "+ mmigration")
p4 <- c("", "+ meducation")
p5 <- c("", "+ englishdummy")
p6 <- c("", "+ mwork")
p7 <- c("", "+ raceattack")
p8 <- c("", "+ regbedtime")
p9 <- c("", "+ learn")
p10 <- c("", "+ distress")
p11 <- c("", "+ logincome", "+ meanincomea", "+ meanincomeb")
p12 <- c("", "+ basicskills")
p13 <- c("", "+ discipline")

p14 <- c("Data2drop_imp", "Data2drop_sub")

coeff2a <- data.frame(NULL)
coeff2b <- data.frame(NULL)
coeff2c <- data.frame(NULL)
coeff2d <- data.frame(NULL)
coeff2e <- data.frame(NULL)
coeff2f <- data.frame(NULL)
confint2a <- data.frame(NULL)
confint2b <- data.frame(NULL)
confint2c <- data.frame(NULL)
confint2d <- data.frame(NULL)
confint2e <- data.frame(NULL)
confint2f <- data.frame(NULL)
n2 <- data.frame(NULL)

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
                        for (m in p13) {
                          model2 <- lm(paste("sdqcont ~ cethnic8b", a, b, c, d, e, f, g, h, i, j, k, l, m),
                                       data = Data2drop_imp)
                          coeff2a <- rbind(coeff2a, summary(model2)$coefficients[2, 1])
                          coeff2b <- rbind(coeff2b, summary(model2)$coefficients[3, 1])
                          coeff2c <- rbind(coeff2c, summary(model2)$coefficients[4, 1])
                          coeff2d <- rbind(coeff2d, summary(model2)$coefficients[5, 1])
                          coeff2e <- rbind(coeff2e, summary(model2)$coefficients[6, 1])
                          coeff2f <- rbind(coeff2f, summary(model2)$coefficients[7, 1])
                          confint2a <- rbind(confint2a, confint(model2)[2, 1:2])
                          confint2b <- rbind(confint2b, confint(model2)[3, 1:2])
                          confint2c <- rbind(confint2c, confint(model2)[4, 1:2])
                          confint2d <- rbind(confint2d, confint(model2)[5, 1:2])
                          confint2e <- rbind(confint2e, confint(model2)[6, 1:2])
                          confint2f <- rbind(confint2f, confint(model2)[7, 1:2])
                          n2 <- rbind(n2, length(model2$model$sdqcont))
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
}

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
                        for (m in p13) {
                          model2 <- lm(paste("sdqcont ~ cethnic8b", a, b, c, d, e, f, g, h, i, j, k, l, m),
                                       data = Data2drop_sub)
                          coeff2a <- rbind(coeff2a, summary(model2)$coefficients[2, 1])
                          coeff2b <- rbind(coeff2b, summary(model2)$coefficients[3, 1])
                          coeff2c <- rbind(coeff2c, summary(model2)$coefficients[4, 1])
                          coeff2d <- rbind(coeff2d, summary(model2)$coefficients[5, 1])
                          coeff2e <- rbind(coeff2e, summary(model2)$coefficients[6, 1])
                          coeff2f <- rbind(coeff2f, summary(model2)$coefficients[7, 1])
                          confint2a <- rbind(confint2a, confint(model2)[2, 1:2])
                          confint2b <- rbind(confint2b, confint(model2)[3, 1:2])
                          confint2c <- rbind(confint2c, confint(model2)[4, 1:2])
                          confint2d <- rbind(confint2d, confint(model2)[5, 1:2])
                          confint2e <- rbind(confint2e, confint(model2)[6, 1:2])
                          confint2f <- rbind(confint2f, confint(model2)[7, 1:2])
                          n2 <- rbind(n2, length(model2$model$sdqcont))
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
}

colnames(coeff2a)[1] <- c("Indian")
colnames(coeff2b)[1] <- c("Pakistani")
colnames(coeff2c)[1] <- c("Bangladeshi")
colnames(coeff2d)[1] <- c("Black Carribbean")
colnames(coeff2e)[1] <- c("Black African")
colnames(coeff2f)[1] <- c("Other")
colnames(confint2a) <- c("Indian LB", "Indian UB")
colnames(confint2b) <- c("Pakistani LB", "Pakistani UB")
colnames(confint2c) <- c("Bangladeshi LB", "Bangladeshi UB")
colnames(confint2d) <- c("Black Carribbean LB", "Black Carribbean UB")
colnames(confint2e) <- c("Black African LB", "Black African UB")
colnames(confint2f) <- c("Other LB", "Other UB")
colnames(n2)[1] <- c("N")

#Indian
table2a <- cbind(coeff2a, confint2a)
table2a <- cbind(table2a, n2)
table2a <- table2a %>% arrange(Indian) 
num <- rep(1:nrow(table2a))
table2a <- cbind(table2a, num)

plot(table2a$N)

plot(table2a$num, table2a$Indian, ylim = range(c(table2a$`Indian.LB`, table2a$`Indian.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Indian)", xlab = "Models")
for (i in c(1:nrow(table2a))) {
  segments(table2a$num[i], table2a$`Indian.LB`[i], table2a$num[i], table2a$`Indian.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2a$num, table2a$Indian, pch = ".", col = "blue")
abline(h = 0)
points(0, -0.037, pch = 19, col = "red") #red is unadjusted
points(1350, 0.21, pch = 19, col = "brown") #brown is adjusted

#Pakistani
table2b <- cbind(coeff2b, confint2b)
table2b <- cbind(table2b, n2)
table2b <- table2b %>% arrange(Pakistani)
num <- rep(1:nrow(table2b))
table2b <- cbind(table2b, num)

plot(table2b$num, table2b$Pakistani, ylim = range(c(table2b$`Pakistani.LB`, table2b$`Pakistani.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Pakistani)", xlab = "Models")
for (i in c(1:nrow(table2b))) {
  segments(table2b$num[i], table2b$`Pakistani.LB`[i], table2b$num[i], table2b$`Pakistani.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2b$num, table2b$Pakistani, pch = ".", col = "blue")
abline(h = 0)
points(16350, 2.43, pch = 19, col = "red") #red is unadjusted
points(12100, 0.69, pch = 19, col = "brown") #brown is adjusted

#Bangladeshi
table2c <- cbind(coeff2c, confint2c)
table2c <- cbind(table2c, n2)
table2c <- table2c %>% arrange(Bangladeshi)
num <- rep(1:nrow(table2c))
table2c <- cbind(table2c, num)

plot(table2c$num, table2c$Bangladeshi, ylim = range(c(table2c$`Bangladeshi.LB`, table2c$`Bangladeshi.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Bangladeshi)", xlab = "Models")
for (i in c(1:nrow(table2c))) {
  segments(table2c$num[i], table2c$`Bangladeshi.LB`[i], table2c$num[i], table2c$`Bangladeshi.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2c$num, table2c$Bangladeshi, pch = ".", col = "blue")
abline(h = 0)
points(15850, 1.60, pch = 19, col = "red") #red is unadjusted
points(5000, -0.023, pch = 19, col = "brown") #brown is adjusted

#Black Carribbean
table2d <- cbind(coeff2d, confint2d)
table2d <- cbind(table2d, n2)
table2d <- table2d %>% arrange(`Black Carribbean`)
table2d <- cbind(table2d, num)

plot(table2d$num, table2d$`Black.Carribbean`, 
     ylim = range(c(table2d$`Black.Carribbean.LB`, table2d$`Black.Carribbean.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Black Caribbean)", xlab = "Models")
for (i in c(1:nrow(table2d))) {
  segments(table2d$num[i], table2d$`Black.Carribbean.LB`[i], table2d$num[i], table2d$`Black.Carribbean.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2d$num, table2d$`Black.Carribbean`, pch = ".", col = "blue")
abline(h = 0)
points(16350, 1.71, pch = 19, col = "red") #red is unadjusted
points(12600, 0.61, pch = 19, col = "brown") #brown is adjusted

#Black African
table2e <- cbind(coeff2e, confint2e)
table2e <- cbind(table2e, n2)
table2e <- table2e %>% arrange(`Black African`)
num <- rep(1:nrow(table2e))
table2e <- cbind(table2e, num)

plot(table2e$num, table2e$`Black.African`, 
     ylim = range(c(table2e$`Black.African.LB`, table2e$`Black.African.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Black African)", xlab = "Models")
for (i in c(1:nrow(table2e))) {
  segments(table2e$num[i], table2e$`Black.African.LB`[i], table2e$num[i], table2e$`Black.African.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2e$num, table2e$`Black.African`, pch = ".", col = "blue")
abline(h = 0)
points(16350, -0.23, pch = 19, col = "red") #red is unadjusted
points(14700, -1.29, pch = 19, col = "brown") #brown is adjusted

#Other
table2f <- cbind(coeff2f, confint2f)
table2f <- cbind(table2f, n2)
table2f <- table2f %>% arrange(Other)
num <- rep(1:nrow(table2f))
table2f <- cbind(table2f, num)

plot(table2f$num, table2f$Other, ylim = range(c(table2f$`Other.LB`, table2f$`Other.UB`)), pch =".", col = "blue",
     ylab = "Coefficient Size (Other)", xlab = "Models")
for (i in c(1:nrow(table2f))) {
  segments(table2f$num[i], table2f$`Other.LB`[i], table2f$num[i], table2f$`Other.UB`[i], 
           col = "light blue", lwd = 0.01)
}
points(table2f$num, table2f$Other, pch = ".", col = "blue")
abline(h = 0)
points(5000, -0.11, pch = 19, col = "red") #red is unadjusted
points(0, -0.70, pch = 19, col = "brown") #brown is adjusted

########Study 3:####
#PAPER 3 Noonan et al. 2018 Social Science and Medicine - Population Health

#MCS1 Data
MCS1_derived <- read.table("mcs1_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_derived)[1] <- "mcsid"
MCS1_parent <- read.table("mcs1_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS2 Data
MCS2_derived <- read.table("mcs2_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS2_derived)[1] <- "mcsid"
MCS2_parent <- read.table("mcs2_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS3 Data
MCS3_derived <- read.table("mcs3_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS3_derived)[1] <- "mcsid"
MCS3_parent <- read.table("mcs3_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS4 Data
MCS4_derived <- read.table("mcs4_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS4_derived)[1] <- "mcsid"
MCS4_parent <- read.table("mcs4_parent_interview.tab", header = T, sep = "\t", fill = TRUE)

#MCS5 Data
MCS5_derived <- read.table("mcs5_family_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_derived)[1] <- "mcsid"
MCS5_cm_derived <- read.table("mcs5_cm_derived.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_cm_derived)[1] <- "mcsid"
MCS5_parent <- read.table("mcs5_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS5_parent)[1] <- "mcsid"

#Start the dataset
Data3 <- MCS1_derived[, "mcsid", drop = F]

###Key dependent var: SDQ TDS
df <- MCS5_cm_derived[, c("mcsid", "EDEBDTAA")]
df <- subset(df, EDEBDTAA >= 0)

df$sdqcont <- df$EDEBDTAA
table(df$sdqcont)

df$sdqdummy[df$EDEBDTAA >= 17] <- 1
df$sdqdummy[df$EDEBDTAA < 17] <- 0
table(df$sdqdummy)

Data3 <- full_join(Data3, df[c("mcsid", "sdqcont", "sdqdummy")], by = "mcsid")

### -- Model 1 baseline variables --
###Var: Family income
#Permanent family income: averageing equivalised household income over first five surveys expressed in logarithmic form
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

dfe <- MCS5_derived[, c("mcsid", "EOEDE000")]
dfe <- subset(dfe, EOEDE000 > 0)
dfe$"logincomee" <- log(dfe$EOEDE000)
df <- full_join(df, dfe, by = "mcsid")

df$"meanincomea" <- (df$logincomea + df$logincomeb + df$logincomec + df$logincomed + df$logincomee)/5

Data3 <- full_join(Data3, df[, c("mcsid", "meanincomea")], by = "mcsid") #N = 10094

#Mtd 2 of calculating income
dfa <- MCS1_derived[, c("mcsid", "ADOEDE00")]
dfa <- subset(dfa, ADOEDE00 > 0)
dfb <- MCS2_derived[, c("mcsid", "BDOEDE00")]
dfb <- subset(dfb, BDOEDE00 > 0)
dfc <- MCS3_derived[, c("mcsid", "CDOEDE00")]
dfc <- subset(dfc, CDOEDE00 > 0)
dfd <- MCS4_derived[, c("mcsid", "DOEDE000")]
dfd <- subset(dfd, DOEDE000 > 0)
dfe <- MCS5_derived[, c("mcsid", "EOEDE000")]
dfe <- subset(dfe, EOEDE000 > 0)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df <- full_join(df, dfd, by = "mcsid")
df <- full_join(df, dfe, by = "mcsid")

df$meanincomeb <- log((df$ADOEDE00 + df$BDOEDE00 + df$CDOEDE00 + df$DOEDE000 + df$EOEDE000)/5)

Data3 <- full_join(Data3, df[, c("mcsid", "meanincomeb")], by = "mcsid")

###Var: Poverty persistence
#Frequency of poverty across the first five surveys: never, one survey, two/three surveys, four/five surveys.
dfa <- MCS1_derived[, c("mcsid", "ADOEDP00")]
table(dfa$ADOEDP00)
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

dfe <- MCS5_derived[, c("mcsid", "EOEDP000")]
dfe <- subset(dfe, EOEDP000 != -1)
df <- full_join(df, dfe, by = "mcsid")

df$"poverty" <- NA
df$"poverty" <- df$ADOEDP00 + df$BDOEDP00 + df$CDOEDP00 + df$DOEDP000 + df$EOEDP000
table(df$poverty)
df$"poverty"[df$poverty == 3] <- 2
df$"poverty"[df$poverty == 4] <- 3
df$"poverty"[df$poverty == 5] <- 3

Data3 <- full_join(Data3, df[, c("mcsid", "poverty")], by = "mcsid")

### -- Model 2 additional variables --
###Var: Child age at MCS5
#10 (N = 2800), 11 (5666), or 12 (33) years old
df <- MCS5_cm_derived[, c("mcsid", "ECNUM00", "ECCAGE00")]
table(df$ECNUM00)

#Only singleborns
length(unique(df$mcsid))
length(unique(df$mcsid)) == nrow(df)
n_occur <- data.frame(table(df$mcsid))
#n_occur[n_occur$Freq > 1, ]
#str(n_occur)
dupe <- n_occur[n_occur$Freq > 1, ]
colnames(dupe)[1] <- "mcsid"
#str(dupe)
df <- anti_join(df, dupe, by = "mcsid")

df$cage <- df$ECCAGE00

Data3 <- full_join(Data3, df[, c("mcsid", "cage")], by = "mcsid")

###Var: Child sex
#Male (N = 4260) or female (4239)
df <- MCS5_cm_derived[, c("mcsid", "ECNUM00", "ECCSEX00")] #1 = M, 2 = F
table(df$ECCSEX00)

length(unique(df$mcsid))
length(unique(df$mcsid)) == nrow(df)
n_occur <- data.frame(table(df$mcsid))
dupe <- n_occur[n_occur$Freq > 1, ]
colnames(dupe)[1] <- "mcsid"
df <- anti_join(df, dupe, by = "mcsid")
table(df$ECNUM00)

df$csex[df$ECCSEX00 == 1] <- 1
df$csex[df$ECCSEX00 == 2] <- 0

Data3 <- full_join(Data3, df[, c("mcsid", "csex")], by = "mcsid")

###Var: Child ethnicity
#White (N = 7650) or non-white (849)
df <- MCS5_cm_derived[, c("mcsid", "EDC06E00", "EDC08E00", "EDC11E00")] #6, 8, and 11 category variable has same N of white 

n_occur <- data.frame(table(df$mcsid))
dupe <- n_occur[n_occur$Freq > 1, ]
colnames(dupe)[1] <- "mcsid"
df <- anti_join(df, dupe, by = "mcsid")

df <- subset(df, EDC06E00 > 0)
df$"cethnicdummy" <- NA
df$"cethnicdummy"[df$EDC06E00 == 1] <- 0 #white
df$"cethnicdummy"[df$EDC06E00 > 1] <- 1 #non-white

table(df$EDC06E00)
for (i in c(1:6)) {
  df$cethnic6[df$EDC06E00 == i] <- i - 1
}
table(df$cethnic6)
for (i in c(1:8)) {
  df$cethnic8[df$EDC08E00 == i] <- i - 1
}
table(df$cethnic8)
for (i in c(1:11)) {
  df$cethnic11[df$EDC11E00 == i] <- i - 1
}
table(df$cethnic11)

Data3 <- full_join(Data3, df[, c("mcsid", "cethnicdummy", "cethnic6", "cethnic8", "cethnic11")], by = "mcsid")

###Var: Number of siblings (MCS4) DDOTHS00
#0 (N = 968), 1 (4041), 2 (2381), more than or equal 3 (1109)
df <- MCS4_derived[, c("mcsid", "DDOTHS00")]
table(df$DDOTHS00)
df$sibling <- df$DDOTHS00
df$sibling[df$DDOTHS00 >= 3] <- 3
table(df$sibling)

Data3 <- full_join(Data3, df[, c("mcsid", "sibling")], by = "mcsid")

###Var: Maternal academic/educational qualification (MCS1)
#Tertiary degree (N = 2599), A-levels (926), GCSE (3994), None of these (980)
df <- MCS1_parent[, c("mcsid", "amacqu00")]
table(df$amacqu00)
df <- subset(df, amacqu00 > 0)

df$"education" <- NA
df$"education"[df$amacqu00 == 96 | df$amacqu00 == 95] <- 0
df$"education"[df$amacqu00 == 6 | df$amacqu00 == 5] <- 1
df$"education"[df$amacqu00 == 4] <- 2
df$"education"[df$amacqu00 == 3 | df$amacqu00 == 2 | df$amacqu00 == 1] <- 3
table(df$education)

Data3 <- full_join(Data3, df[, c("mcsid", "education")], by = "mcsid")

###Var: Maternal age at child's birth (not AMDGAB00)
#Under 20 (N = 507), 20-24 (1260), 25-29 (2410), 30-34 (2797), 35 or more (1516)
df <- MCS1_derived[, c("mcsid", "AMDAGB00", "AMDGAB00")]
table(df$AMDAGB00)
table(df$AMDGAB00)
df <- subset(df, AMDAGB00 > 0)

df$magecata <- NA
df$magecata[df$AMDAGB00 < 20] <- 0
df$magecata[df$AMDAGB00 >= 20 & df$AMDAGB00 < 25] <- 1
df$magecata[df$AMDAGB00 >= 25 & df$AMDAGB00 < 30] <- 2
df$magecata[df$AMDAGB00 >= 30 & df$AMDAGB00 < 35] <- 3
df$magecata[df$AMDAGB00 >= 35] <- 4
table(df$magecata)

for (i in c(1:4)) {
  df$magecatb[df$AMDGAB00 == i] <- i - 1
}
table(df$magecatb)

df$magecont <- df$AMDAGB00
table(df$magecont)

Data3 <- full_join(Data3, df[, c("mcsid", "magecata", "magecatb", "magecont")], by = "mcsid")

###Var: Housing tenancy status (MCS4)
#Own/mortage (N = 6177), council (938), rent/other (1384) -- check definition?
df <- MCS4_derived[c("mcsid", "DDROOW00")]
table(df$DDROOW00)
df <- subset(df, DDROOW00 > 0)

df$house <- NA
df$house[df$DDROOW00 <= 3] <- 0
df$house[df$DDROOW00 == 4 | df$DDROOW00 == 5] <- 1
df$house[df$DDROOW00 >= 6] <- 2
any(is.na(df) == T)

Data3 <- full_join(Data3, df[, c("mcsid", "house")], by = "mcsid")

###Child health endowment variables:
###Var: Child birthweight
#More than 2.5 kg (N = 8009), less than or equal 2.5kg (490)
df <- MCS1_derived[c("mcsid", "ADBWGTA0")]
df <- subset(df, ADBWGTA0 > 0)
df$birthweight[df$ADBWGTA0 <= 2.5] <- 1
df$birthweight[df$ADBWGTA0 > 2.5] <- 0

Data3 <- full_join(Data3, df[, c("mcsid", "birthweight")], by = "mcsid")

###Var: Child gestational age 
#More than or equal 37 weeks (N = 7886), less than 37 weeks (613) ADGESTA0
df <- MCS1_derived[c("mcsid", "ADGESTA0")]
df <- subset(df, ADGESTA0 > 0)
df$gestation[df$ADGESTA0 < 259] <- 1
df$gestation[df$ADGESTA0 >= 259] <- 0

Data3 <- full_join(Data3, df[, c("mcsid", "gestation")], by = "mcsid")

###Var: Child longstanding illness (MCS4) dmclsia0
#No (N = 6928) or yes (1571)
df <- MCS4_parent[c("mcsid", "dmclsia0")]
table(df$dmclsia0)
df <- subset(df, dmclsia0 > 0)
df$ill[df$dmclsia0 == 2] <- 0
df$ill[df$dmclsia0 == 1] <- 1
table(df$ill)

Data3 <- full_join(Data3, df[, c("mcsid", "ill")], by = "mcsid")

###Var: Mother drinking during pregnancy
#Never (N = 5715), light (2164), moderate (449), heavy/binge (171)
df <- MCS1_parent[c("mcsid", "amdrof00")]
table(df$amdrof00)
df <- subset(df, amdrof00 > 0)
df$alcohol[df$amdrof00 == 7] <- 0 #never
df$alcohol[df$amdrof00 == 6] <- 1 #light
df$alcohol[df$amdrof00 == 5] <- 1
df$alcohol[df$amdrof00 == 4] <- 2 #moderate
df$alcohol[df$amdrof00 == 3] <- 3 #heavy
df$alcohol[df$amdrof00 == 2] <- 3
df$alcohol[df$amdrof00 == 1] <- 3
table(df$alcohol)

Data3 <- full_join(Data3, df[, c("mcsid", "alcohol")], by = "mcsid")

###Var: Mother's smoking during pregnancy
#Never smoked (N = 5690), stopped during pregnancy (1076), smoked throughout pregnancy (1733)
df <- MCS1_parent[c("mcsid", "amsmus0a", "amsmty00", "amsmev00", "amsmch00", "amcich00")]
df <- subset(df, amsmus0a > 0) #1 = no, all others = yes

table(df$amsmus0a)
table(df$amsmty00)
table(df$amsmev00)
table(df$amcich00)
df$smoke <- NA
df$smoke[df$amsmus0a > 1] <- 2
df$smoke[df$amsmus0a == 1] <- 0
df$smoke[df$amcich00 == 0] <- 1
table(df$smoke)
any(is.na(df) == T)
#df <- subset(df, is.na(smoke) == F)

Data3 <- full_join(Data3, df[, c("mcsid", "smoke")], by = "mcsid")

###Var: Maternal breastfeeding
#No breastfeeding (N = 2482), <7 days (979), 1 week to 3 months inclusive (2127), 3 to 6 months inclusive (1225), >6 months (1686)
df <- MCS1_parent[c("mcsid", "ambfeva0", "ambfeaa0", "ambfeda0", "ambfewa0", "ambfema0")]
table(df$ambfeda0)

df$breastfeed <- NA
df$breastfeed[df$ambfeda0 <= 7] <- 1
df$breastfeed[df$ambfeva0 == 2] <- 0
df$breastfeed[df$ambfema0 > 6] <- 4
df$breastfeed[df$ambfema0 > 3 & df$ambfema0 <= 6] <- 3
df$breastfeed[df$ambfewa0 > 1 & df$ambfema0 <= 3] <- 2

table(df$breastfeed)
sum(is.na(df$breastfeed) == T)

Data3 <- full_join(Data3, df[, c("mcsid", "breastfeed")], by = "mcsid")

### -- Model 3 additional variable -- 
###Var: Maternal psychological distress, all waves (RMI for MCS1 and K6 for the rest) (see study p282)
#Never (N = 3185), early years only (520), middle years only (604), age 11 only (637), other pattern (1237), recurrent (1930), persistent (386) 
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
#End of MCS1 segment

dfb <- MCS2_derived[c("mcsid", "BMKESS00")]
dfb <- subset(dfb, BMKESS00 >= 0)
dfb$bkess <- NA
dfb$bkess <- dfb$BMKESS00

dfb$bdistress <- NA
dfb$bdistress[dfb$bkess >= 6] <- 1
dfb$bdistress[dfb$bkess < 6] <- 0
#End of MCS2 segment

dfc <- MCS3_derived[c("mcsid", "CMKESS00")]
dfc <- subset(dfc, CMKESS00 >= 0)
dfc$ckess <- NA
dfc$ckess <- dfc$CMKESS00

dfc$cdistress <- NA
dfc$cdistress[dfc$ckess >= 6] <- 1
dfc$cdistress[dfc$ckess < 6] <- 0
#End of MCS3 segment

dfd <- MCS4_derived[c("mcsid", "DMKESS00")]
dfd <- subset(dfd, DMKESS00 >= 0)
dfd$dkess <- NA
dfd$dkess <- dfd$DMKESS00

dfd$ddistress <- NA
dfd$ddistress[dfd$dkess >= 6] <- 1
dfd$ddistress[dfd$dkess < 6] <- 0
#End of MCS4 segment

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

dfe <- distinct(dfe, mcsid, .keep_all = TRUE) #only MCS5 has repetitions of mcsid
#End of MCS5 segment

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df <- full_join(df, dfd, by = "mcsid")
df <- full_join(df, dfe, by = "mcsid") #N = 8713

df$distressvar <- NA
df$distressvar <- df$armi1 + df$bdistress + df$cdistress + df$ddistress + df$edistress
table(df$distressvar)

#Groupings: never 0, early 1 (MCS1 and/or 2), middle 2 (MCS3 and/or 4), current 3 (MCS5), 
#recurrent 4 (distress MCS1 AND any other wave), persistent 5 (distress == 5), and all others 6

df$distress <- 6
df$distress[df$distressvar == 0] <- 0
df$distress[df$adistress == 1 | df$bdistress == 1] <- 1
df$distress[df$cdistress == 1 | df$ddistress == 1] <- 2
df$distress[df$edistress == 1] <- 3
df$distress[(df$adistress == 1 | df$bdistress == 1) & (df$cdistress == 1 | df$ddistress == 1)] <- 4
df$distress[df$distressvar == 5] <- 5
table(df$distress)

Data3 <- full_join(Data3, df[, c("mcsid", "distress")], by = "mcsid")

### -- Model 4 additional variables (taken from MCS4) --
###Var: Change in mother's relationship between MCS4 and 5
#Became single (N = 531), became partnered (358), no change (7610)
dfd <- MCS4_derived[c("mcsid", "DDHTYS00")]
dfe <- MCS5_derived[c("mcsid", "EDHTYS00")]
table(dfd$DDHTYS00) #1 = two-parent/carers, 2 = single-parent/carers
table(dfe$EDHTYS00)

df <- full_join(dfe, dfd, by = "mcsid")
df$rchange <- NA
df$rchange[df$DDHTYS00 == df$DDHTYS00] <- 0 #no change
df$rchange[df$DDHTYS00 == 2 & df$EDHTYS00 == 1] <- 1 #became partnered
df$rchange[df$DDHTYS00 == 1 & df$EDHTYS00 == 2] <- 2 #became single
table(df$rchange)

Data3 <- full_join(Data3, df[, c("mcsid", "rchange")], by = "mcsid")

###Var: Regular bedtime on weekdays (MCS4)
#Never/almost never (N = 288), sometimes (435), usually (2699), always (5077)
df <- MCS4_parent[c("mcsid", "dmberea0")]
df <- subset(df, dmberea0 > 0)
table(df$dmberea0)
df$bedtime[df$dmberea0 == 1] <- 0
df$bedtime[df$dmberea0 == 2] <- 1
df$bedtime[df$dmberea0 == 3] <- 2
df$bedtime[df$dmberea0 == 4] <- 3

Data3 <- full_join(Data3, df[, c("mcsid", "bedtime")], by = "mcsid")

###Var: Hours child spent playing computer/video games during week (MCS4)
#None (N = 866), <1h (4644), 1-3h (2668), >3h (321)
df <- MCS4_parent[c("mcsid", "dmcompa0")]
table(df$dmcompa0)
df <- subset(df, dmcompa0 > 0)
df$computer[df$dmcompa0 == 1] <- 0 #None
df$computer[df$dmcompa0 == 2] <- 1 #Less than an hour
df$computer[df$dmcompa0 == 3] <- 2 #1 to <3 hours
df$computer[df$dmcompa0 >= 4] <- 3 #3 hours and above
table(df$computer)

Data3 <- full_join(Data3, df[, c("mcsid", "computer")], by = "mcsid")

###Var: Mother's satisfaction with time spent with child (MCS4)
#Enough/more than enough (N = 2029), just enough (3682), not enough (2788)
df <- MCS4_parent[c("mcsid", "dmchti00")]
table(df$dmchti00)
df <- subset(df, dmchti00 > 0)
df <- subset(df, dmchti00 != 6)
df$mtime[df$dmchti00 <= 2] <- 0
df$mtime[df$dmchti00 == 3] <- 1
df$mtime[df$dmchti00 >= 4] <- 2
table(df$mtime)

Data3 <- full_join(Data3, df[, c("mcsid", "mtime")], by = "mcsid")

###Var: Tells off when naughty (MCS4)
#Never (N = 48), rarely (1019), sometimes (3003), often (4429)
df <- MCS4_parent[c("mcsid", "dmditea0")]
table(df$dmditea0)
df <- subset(df, dmditea0 > 0)
df <- subset(df, dmditea0 != 6)
df$naughtytells[df$dmditea0 == 1] <- 0
df$naughtytells[df$dmditea0 == 2] <- 1
df$naughtytells[df$dmditea0 == 3] <- 2
df$naughtytells[df$dmditea0 >= 4] <- 3
table(df$naughtytells)

Data3 <- full_join(Data3, df[, c("mcsid", "naughtytells")], by = "mcsid")

###Var: Sends to room when naughty (MCS4)
#Never (N = 1037), rarely (2498), sometimes (3406), often (1558)
df <- MCS4_parent[c("mcsid", "dmdibna0")]
table(df$dmdibna0)
df <- subset(df, dmdibna0 > 0)
df <- subset(df, dmdibna0 != 6)
df$naughtyroom[df$dmdibna0 == 1] <- 0
df$naughtyroom[df$dmdibna0 == 2] <- 1
df$naughtyroom[df$dmdibna0 == 3] <- 2
df$naughtyroom[df$dmdibna0 >= 4] <- 3
table(df$naughtyroom)

Data3 <- full_join(Data3, df[, c("mcsid", "naughtyroom")], by = "mcsid")

###Var: Takes things away when naughty (MCS4)
#Never (N = 687), rarely (2414), sometimes (3931), often (1467)
df <- MCS4_parent[c("mcsid", "dmditra0")]
table(df$dmditra0)
df <- subset(df, dmditra0 > 0)
df <- subset(df, dmditra0 != 6)
df$naughtytakes[df$dmditra0 == 1] <- 0
df$naughtytakes[df$dmditra0 == 2] <- 1
df$naughtytakes[df$dmditra0 == 3] <- 2
df$naughtytakes[df$dmditra0 >= 4] <- 3
table(df$naughtytakes)

Data3 <- full_join(Data3, df[, c("mcsid", "naughtytakes")], by = "mcsid")

###Var: Combine naughty variables
table(Data3$naughtyroom)
table(Data3$naughtytakes)
table(Data3$naughtytells)

Data3$naughtycombine <- Data3$naughtyroom + Data3$naughtytakes + Data3$naughtytells
table(Data3$naughtycombine) #cont

###Var: Amount of time reads to child (MCS4)
#Not at all (N = 176), approx monthly (615), weekly or more (7708)
df <- MCS4_parent[c("mcsid", "dmreofa0")]
table(df$dmreofa0)
df <- subset(df, dmreofa0 > 0)
df$readtocm[df$dmreofa0 == 6] <- 0
df$readtocm[df$dmreofa0 == 5 | df$dmreofa0 == 4] <- 1
df$readtocm[df$dmreofa0 == 3 | df$dmreofa0 == 2 | df$dmreofa0 == 1] <- 2
table(df$readtocm)

Data3 <- full_join(Data3, df[, c("mcsid", "readtocm")], by = "mcsid")

###Var: Time spent playing games with child (MCS4)
#Not at all (N = 385), approx monthly (2226), weekly or more (5888)
df <- MCS4_parent[c("mcsid", "dmgamea0")]
table(df$dmgamea0)
df <- subset(df, dmgamea0 > 0)
df$gamewithcm[df$dmgamea0 == 6] <- 0
df$gamewithcm[df$dmgamea0 == 5 | df$dmgamea0 == 4] <- 1
df$gamewithcm[df$dmgamea0 == 3 | df$dmgamea0 == 2 | df$dmgamea0 == 1] <- 2
table(df$gamewithcm)

Data3 <- full_join(Data3, df[, c("mcsid", "gamewithcm")], by = "mcsid")

###Var: Mother longstanding health condition (MCS4)
#No (N = 6382), yes (2117)
df <- MCS4_parent[c("mcsid", "dmloil00")]
table(df$dmloil00)
df <- subset(df, dmloil00 > 0)
df$longhealth[df$dmloil00 == 2] <- 0
df$longhealth[df$dmloil00 == 1] <- 1
table(df$longhealth)

Data3 <- full_join(Data3, df[, c("mcsid", "longhealth")], by = "mcsid")

###Var: Mother smoking (MCS4)
#Non-smoker (N = 6383), smoker (2116)
df <- MCS4_parent[c("mcsid", "dmsmus0a")]
table(df$dmsmus0a)
df <- subset(df, dmsmus0a > 0)
df$smokenow[df$dmsmus0a == 1] <- 0
df$smokenow[df$dmsmus0a > 1] <- 1
table(df$smokenow)

Data3 <- full_join(Data3, df[, c("mcsid", "smokenow")], by = "mcsid")

###Var: Mother alcohol intake (MCS4)
#5+ weekly (N = 576), 3-4 times weekly (1069), 1-2 times weekly (2445), monthly (3067), never/almost never (1342)
df <- MCS4_parent[c("mcsid", "dmaldr00")]
table(df$dmaldr00)
df <- subset(df, dmaldr00 > 0)
df$alcoholnow[df$dmaldr00 == 7 | df$dmaldr00 == 6] <- 0
df$alcoholnow[df$dmaldr00 == 5] <- 1
df$alcoholnow[df$dmaldr00 == 4] <- 2
df$alcoholnow[df$dmaldr00 == 3] <- 3
df$alcoholnow[df$dmaldr00 == 2 | df$dmaldr00 == 1] <- 4
table(df$alcoholnow)

Data3 <- full_join(Data3, df[, c("mcsid", "alcoholnow")], by = "mcsid")

###Var: Time child spends with friends outside school (MCS4)
#Not at all (N = 440), approx monthly (1532), regularly 1-3 times a week (4667), most days (1860)
df <- MCS4_parent[c("mcsid", "dmvifra0")]
table(df$dmvifra0)
df <- subset(df, dmvifra0 > 0)
df$cmfriend[df$dmvifra0 == 6] <- 0
df$cmfriend[df$dmvifra0 == 5 | df$dmvifra0 == 4] <- 1
df$cmfriend[df$dmvifra0 == 3 | df$dmvifra0 == 2] <- 2
df$cmfriend[df$dmvifra0 == 1] <- 3
table(df$cmfriend)

Data3 <- full_join(Data3, df[, c("mcsid", "cmfriend")], by = "mcsid")

###Var: Child time playing sport
#Never/rarely (N = 2334), once a week (2309), 2-3 times a week (3154), more than or equal to 4 times a week (702)
df <- MCS4_parent[c("mcsid", "dmsehoa0")]
table(df$dmsehoa0)
df <- subset(df, dmsehoa0 > 0)
df$cmsport[df$dmsehoa0 == 7 | df$dmsehoa0 == 6] <- 0
df$cmsport[df$dmsehoa0 == 5] <- 1
df$cmsport[df$dmsehoa0 == 4 | df$dmsehoa0 == 3] <- 2
df$cmsport[df$dmsehoa0 == 2 | df$dmsehoa0 == 1] <- 3
table(df$cmsport)

Data3 <- full_join(Data3, df[, c("mcsid", "cmsport")], by = "mcsid")

Data3$poverty <- as.factor(Data3$poverty) 
Data3$csex <- as.factor(Data3$csex)
Data3$cethnic6 <- as.factor(Data3$cethnic6)
Data3$cethnic8 <- as.factor(Data3$cethnic8)
Data3$cethnic11 <- as.factor(Data3$cethnic11)
Data3$sibling <- as.factor(Data3$sibling)
Data3$education <- as.factor(Data3$education)
Data3$magecata <- as.factor(Data3$magecata)
Data3$magecatb <- as.factor(Data3$magecatb)
Data3$house <- as.factor(Data3$house)
Data3$birthweight <- as.factor(Data3$birthweight)
Data3$gestation <- as.factor(Data3$gestation)
Data3$breastfeed <- as.factor(Data3$breastfeed)
Data3$smoke <- as.factor(Data3$smoke)
Data3$alcohol <- as.factor(Data3$alcohol)
Data3$ill <- as.factor(Data3$ill)
Data3$distress <- as.factor(Data3$distress)
Data3$rchange <- as.factor(Data3$rchange)
Data3$bedtime <- as.factor(Data3$bedtime)
Data3$computer <- as.factor(Data3$computer)
Data3$mtime <- as.factor(Data3$mtime)
Data3$naughtycombine <- as.factor(Data3$naughtycombine)
Data3$readtocm <- as.factor(Data3$readtocm)
Data3$gamewithcm <- as.factor(Data3$gamewithcm)
Data3$longhealth <- as.factor(Data3$longhealth)
Data3$smokenow <- as.factor(Data3$smokenow)
Data3$alcoholnow <- as.factor(Data3$alcoholnow)
Data3$cmfriend <- as.factor(Data3$cmfriend)
Data3$cmsport <- as.factor(Data3$cmsport)
Data3$naughtyroom <- as.factor(Data3$naughtyroom)
Data3$naughtytakes <- as.factor(Data3$naughtytakes)
Data3$naughtytells <- as.factor(Data3$naughtytells)

mfx <- function(x,sims=1000){
  set.seed(1984)
  pdf <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  pdfsd <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                  sd(dnorm(predict(x, type = "link"))),
                  sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  sim <- matrix(rep(NA,sims*length(coef(x))), nrow=sims)
  for(i in 1:length(coef(x))){
    sim[,i] <- rnorm(sims,coef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(sims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1])=="(Intercept)",
         return(res[2:nrow(res),]),return(res))
}

s3m1 <- glm(sdqdummy ~ meanincomeb, 
            family = binomial, data = Data3)
s3m2 <- glm(sdqdummy ~ meanincomeb +  
              cage + factor(csex) + factor(cethnicdummy) + factor(sibling) + factor(education) + factor(magecata) + factor(house) +
              factor(birthweight) + factor(gestation) + factor(breastfeed) + factor(smoke) + factor(alcohol) + factor(ill),
            family = binomial, data = Data3)
s3m3 <- glm(sdqdummy ~ meanincomeb + 
              cage + factor(csex) + factor(cethnicdummy) + factor(sibling) + factor(education) + factor(magecata) + factor(house) +
              factor(birthweight) + factor(gestation) + factor(breastfeed) + factor(smoke) + factor(alcohol) + factor(ill) +
              factor(distress),
            family = binomial, data = Data3)
s3m4 <- glm(sdqdummy ~ meanincomeb + 
              cage + factor(csex) + factor(cethnicdummy) + factor(sibling) + factor(education) + factor(magecata) + factor(house) +
              factor(birthweight) + factor(gestation) + factor(breastfeed) + factor(smoke) + factor(alcohol) + factor(ill) +
              factor(distress) +
              factor(rchange) + factor(bedtime) + factor(computer) + factor(mtime) + factor(naughtycombine) +
              factor(readtocm) + factor(gamewithcm) + factor(longhealth) + factor(smokenow) + factor(alcoholnow) + 
              factor(cmfriend) + factor(cmsport),
            family = binomial, data = Data3)

summary(s3m1)
summary(s3m2)
summary(s3m3)
summary(s3m4)

p1 <- c("meanincomea", "meanincomeb")
p2 <- c("", "+ poverty")
p3 <- c("", "+ cage + csex + sibling + education + house")
p4 <- c("", "+ cethnicdummy", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p5 <- c("", "+ magecata", "+ magecatb", "+ magecont")
p6 <- c("", "+ breastfeed + ill + distress")
p7 <- c("", "+ smoke + alcohol")
p8 <- c("", "+ birthweight + gestation")
p9 <- c("", "+ rchange + bedtime + computer + mtime")
p10 <- c("", "+ naughtycombine", "+ naughtytells + naughtyroom + naughtytakes")
p11 <- c("", "+ readtocm + gamewithcm")
p12 <- c("", "+ longhealth + smokenow + alcoholnow")
p13 <- c("", "+ cmfriend + cmsport")

coeff3 <- data.frame(NULL)
se3 <- data.frame(NULL)
n3 <- data.frame(NULL)

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
                        for (m in p13) {
                          model3 <- glm(paste("sdqdummy ~", a, b, c, d, e, f, g, h, i, j, k, l, m),
                                        data = Data3)
                          coeff3 <- rbind(coeff3, summary(model3)$coefficients[2, 1])
                          se3 <- rbind(se3, summary(model3)$coefficients[2, 2])
                          n3 <- rbind(n3, length(model3$model$sdqdummy))
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
}

colnames(coeff3)[1] <- c("Income")
colnames(n3)[1] <- c("N")
colnames(se3)[1] <- c("SE")

table3 <- cbind(coeff3, se3)
table3 <- cbind(table3, n3)

table3$UB <- table3$Income + 1.96*(table3$SE)
table3$LB <- table3$Income - 1.96*(table3$SE)

table3 <- table3 %>% arrange(Income) 
num <- rep(1:nrow(table3))
table3 <- cbind(table3, num)

plot(table3$num, table3$Income, ylim = range(c(table3$LB, table3$UB)), pch =".", col = "blue",
     ylab = "Marginal Effect Size (Family Income)", xlab = "Models")
for (i in c(1:nrow(table3))) {
  segments(table3$num[i], table3$LB[i], table3$num[i], table3$UB[i], 
           col = "light blue", lwd = 0.01)
}
points(table3$num, table3$Income, pch = ".", col = "blue")
abline(h = 0)
points(20900, -0.044, pch = 19, col = "brown") #brown is adjusted

plot(table3$N)

########Study 4:####
#PAPER 4 McMunn et al. 2011 BMJ

#MCS1 Data
MCS1_parent <- read.table("mcs1_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS1_derived <- read.table("mcs1_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS1_derived)[1] <- "mcsid"

#MCS2 Data
MCS2_parent <- read.table("mcs2_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS2_derived <- read.table("mcs2_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS2_derived)[1] <- "mcsid"

#MCS3 Data
MCS3_parent <- read.table("mcs3_parent_interview.tab", header = T, sep = "\t", fill = TRUE)
MCS3_derived <- read.table("mcs3_derived_variables.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS3_derived)[1] <- "mcsid"
MCS3_hhgrid <- read.table("mcs3_hhgrid.tab", header = T, sep = "\t", fill = TRUE)
colnames(MCS3_hhgrid)[1] <- "mcsid"

#Start the dataset
Data4 <- MCS3_derived[, "mcsid", drop = F]

###Var: SDQ TDS at Age 5 (binary; cutoff 17 or more) 
df <- MCS3_derived[, c("mcsid", "CDEBDTA0")]
table(df$CDEBDTA0)
df <- subset(df, CDEBDTA0 >= 0)

df$sdqcont <- df$CDEBDTA0
table(df$sdqcont)

df$sdqdummy[df$CDEBDTA0 >= 17] <- 1
df$sdqdummy[df$CDEBDTA0 < 17] <- 0
table(df$sdqdummy)

Data4 <- full_join(Data4, df[c("mcsid", "sdqcont", "sdqdummy")], by = "mcsid")

###Var to subset: singleton births
length(unique(Data4$mcsid))
length(unique(Data4$mcsid)) == nrow(Data4) #only singletons already, no need to run this

###Var to subset: mother present
df <- MCS3_derived[, c("mcsid", "CDNATM00", "CDMINH00")]
table(df$CDNATM00)
table(df$CDMINH00) #use this
df <- subset(df, CDMINH00 == 1)
df$motherpresent[df$CDMINH00 == 1] <- 1
Data4 <- full_join(Data4, df[c("mcsid", "motherpresent")], by = "mcsid")

###Var to subset: only white children
df <- MCS3_derived[, c("mcsid", "CMD06E00", "CMD08E00", "CMD11E00")]
table(df$CMD06E00)
table(df$CMD08E00)
table(df$CMD11E00) #no difference in numbers of whites
df <- subset(df, CMD06E00 > 0)
df$cethnicdummy[df$CMD06E00 == 1] <- 0
df$cethnicdummy[df$CMD06E00 > 1] <- 1
table(df$cethnicdummy)

table(df$CMD06E00)
for (i in c(1:6)) {
  df$cethnic6[df$CMD06E00 == i] <- i - 1
}
table(df$cethnic6)
for (i in c(1:8)) {
  df$cethnic8[df$CMD08E00 == i] <- i - 1
}
table(df$cethnic8)
for (i in c(1:11)) {
  df$cethnic11[df$CMD11E00 == i] <- i - 1
}
table(df$cethnic11)

Data4 <- full_join(Data4, df[c("mcsid", "cethnicdummy", "cethnic6", "cethnic8", "cethnic11")], by = "mcsid")

###Var to separate: child gender (male and female children run separately) CHCSEX00
df <- MCS3_hhgrid[, c("mcsid", "CHCSEX00")]
table(df$CHCSEX00)
df <- subset(df, CHCSEX00 != -1)
df$csex[df$CHCSEX00 == 1] <- 1 #male
df$csex[df$CHCSEX00 == 2] <- 0 #female
table(df$csex)
Data4 <- full_join(Data4, df[c("mcsid", "csex")], by = "mcsid") #note different join

###Var: Maternal employment ampjob00 amwkst00 bmpjob00 bmwkwk00 cmpjob00 cmwkwk00
#Count number of sweeps (MCS1, 2, 3) at which mothers were in paid work (incl. full and part time)
#Note: At sweep 1, 5% of those employed were at home on maternity leave;
#They are considered employed at sweep 1 for this analysis.
dfa <- MCS1_parent[, c("mcsid", "amwkst00")]
dfb <- MCS2_parent[, c("mcsid", "bmwkwk00")]
dfc <- MCS3_parent[, c("mcsid", "cmwkwk00")]
table(dfa$amwkst00)
dfa <- subset(dfa, amwkst00 != -1)
dfa$mpaidworka[dfa$amwkst00 == 1] <- 1
dfa$mpaidworka[dfa$amwkst00 == 2] <- 1
dfa$mpaidworka[dfa$amwkst00 == 3] <- 0
dfa$mpaidworka[dfa$amwkst00 == 4] <- 0
table(dfb$bmwkwk00)
dfb <- subset(dfb, bmwkwk00 != -1)
dfb$mpaidworkb[dfb$bmwkwk00 == 1] <- 1
dfb$mpaidworkb[dfb$bmwkwk00 == 2] <- 0
table(dfc$cmwkwk00)
dfc <- subset(dfc, cmwkwk00 > 0)
dfc$mpaidworkc[dfc$cmwkwk00 == 1] <- 1
dfc$mpaidworkc[dfc$cmwkwk00 == 2] <- 0

table(dfa$mpaidworka)
table(dfb$mpaidworkb)
table(dfc$mpaidworkc)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")

df$mpaidwork[df$mpaidworka + df$mpaidworkb + df$mpaidworkc == 3] <- 0
df$mpaidwork[df$mpaidworka + df$mpaidworkb + df$mpaidworkc == 2] <- 1
df$mpaidwork[df$mpaidworka + df$mpaidworkb + df$mpaidworkc == 1] <- 2
df$mpaidwork[df$mpaidworka + df$mpaidworkb + df$mpaidworkc == 0] <- 3

Data4 <- full_join(Data4, df[c("mcsid", "mpaidwork")], by = "mcsid")
table(Data4$mpaidwork)

###Var: Partner work status
#Partner in work, not in work, or no partner.
dfa <- MCS1_parent[, c("mcsid", "apwkst00")]
dfb <- MCS2_parent[, c("mcsid", "bpwkwk00")]
dfc <- MCS3_parent[, c("mcsid", "cpwkwk00")]
table(dfa$apwkst00)
dfa$ppaidworka[dfa$apwkst00 == 1] <- 1
dfa$ppaidworka[dfa$apwkst00 == 2] <- 1
dfa$ppaidworka[dfa$apwkst00 == 3] <- 0
dfa$ppaidworka[dfa$apwkst00 == 4] <- 0
dfa$ppaidworka[dfa$apwkst00 == -1] <- 999
table(dfb$bpwkwk00)
dfb$ppaidworkb[dfb$bpwkwk00 == 1] <- 1
dfb$ppaidworkb[dfb$bpwkwk00 == 2] <- 0
dfb$ppaidworkb[dfb$bpwkwk00 == -1] <- 999
table(dfc$cpwkwk00)
dfc$ppaidworkc[dfc$cpwkwk00 == 1] <- 1
dfc$ppaidworkc[dfc$cpwkwk00 == 2] <- 0
dfc$ppaidworkc[dfc$cpwkwk00 == -1] <- 999

table(dfa$ppaidworka)
table(dfb$ppaidworkb)
table(dfc$ppaidworkc)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")

df$ppaidwork[df$ppaidworka + df$ppaidworkb + df$ppaidworkc == 3] <- 0
df$ppaidwork[df$ppaidworka + df$ppaidworkb + df$ppaidworkc == 2] <- 1
df$ppaidwork[df$ppaidworka + df$ppaidworkb + df$ppaidworkc == 1] <- 2
df$ppaidwork[df$ppaidworka + df$ppaidworkb + df$ppaidworkc == 0] <- 3
table(df$ppaidwork)
df$ppaidwork[df$ppaidworka > 4 | df$ppaidworkb > 4 | df$ppaidworkc > 4] <- 4
any(is.na(df$ppaidwork) == T)

Data4 <- full_join(Data4, df[c("mcsid", "ppaidwork")], by = "mcsid")

any(is.na(Data4) == T)
#Data4 <- Data4[complete.cases(Data4), ]

###Var: Maternal education (NVQ scale)
df <- MCS3_derived[, c("mcsid", "CMDNVQ00")] #highest educational attainment in MCS1, 2, and 3
table(df$CMDNVQ00)
df <- subset(df, CMDNVQ00 != -1)
df$meducation[df$CMDNVQ00 == 1] <- 0
df$meducation[df$CMDNVQ00 == 2] <- 1
df$meducation[df$CMDNVQ00 == 3] <- 2
df$meducation[df$CMDNVQ00 == 4] <- 3
df$meducation[df$CMDNVQ00 == 5] <- 4
df$meducation[df$CMDNVQ00 == 95] <- 5
df$meducation[df$CMDNVQ00 == 96] <- 6

Data4 <- full_join(Data4, df[c("mcsid", "meducation")], by = "mcsid")

###Var: Mother's age at childbirth AMDGAB00
df <- MCS1_derived[, c("mcsid", "AMDGAB00", "AMDAGB00")]
table(df$AMDAGB00) #use this
table(df$AMDGAB00)
df <- subset(df, AMDAGB00 > 0)

for (i in c(1:4)) {
  df$magecat[df$AMDGAB00 == i] <- i - 1
}
table(df$magecat)

df$magecont <- df$AMDAGB00
table(df$magecont)

Data4 <- full_join(Data4, df[c("mcsid", "magecont", "magecat")], by = "mcsid")

###Var: Child age (new var)
df <- MCS3_hhgrid[, c("mcsid", "CHCSEX00", "CHCDBY00", "CHCAGE00")]
table(df$CHCSEX00)
df <- subset(df, CHCSEX00 != -1)
table(df$CHCDBY00)
any(df$CHCAGE00 < 0)

df <- subset(df, CHCAGE00 > 0)
df$cagedayscont <- df$CHCAGE00

for (i in c(2000:2002)) {
  df$cageyearscat[df$CHCDBY00 == i] <- i - 2000
}
table(df$cageyearscat)

Data4 <- full_join(Data4, df[c("mcsid", "cagedayscont", "cageyearscat")], by = "mcsid")

###Var: Household income (in brackets) at MCS1, 2, 3 
dfa <- MCS1_derived[c("mcsid", "ADHINC00", "ADOEDE00")]
table(dfa$ADHINC00)
dfa <- subset(dfa, ADHINC00 > 0)
dfa <- subset(dfa, ADHINC00 <= 6)
for (i in c(1:6)) {
  dfa$incbanda[dfa$ADHINC00 == i] <- i - 1
}
table(dfa$incbanda)

dfb <- MCS2_derived[c("mcsid", "BDHINC00", "BDOEDE00")]
table(dfb$BDHINC00)
dfb <- subset(dfb, BDHINC00 > 0)
for (i in c(1:6)) {
  dfb$incbandb[dfb$BDHINC00 == i] <- i - 1
}
table(dfb$incbandb)

dfc <- MCS3_derived[c("mcsid", "CDINCC00", "CDINCS00", "CDOEDE00")]
table(dfc$CDINCC00)
table(dfc$CDINCS00)
for (i in c(1:20)) {
  dfc$incbandc[dfc$CDINCC00 == i] <- i - 1
}
for (i in c(1:20)) {
  dfc$incbandc[dfc$CDINCS00 == i] <- i - 1
}
table(dfc$incbandc)

for (i in c(1:3)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 0
}
for (i in c(4:6)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 1
}
for (i in c(7:9)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 2
}
for (i in c(10:12)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 3
}
for (i in c(13:15)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 4
}
for (i in c(16:19)) {
  dfc$incbandc_new[dfc$incbandc == i] <- 5
}
table(dfc$incbandc_new)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df$incbandave <- df$incbanda + df$incbandb + df$incbandc_new
table(df$incbandave)

Data4 <- full_join(Data4, df[c("mcsid", "incbandave")], by = "mcsid")

###Var: Study 3's method of calculating average income
#Permanent family income: averageing equivalised household income over first five surveys expressed in logarithmic form
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

df$"meanincomea" <- (df$logincomea + df$logincomeb + df$logincomec)/3

Data4 <- full_join(Data4, df[, c("mcsid", "meanincomea")], by = "mcsid")

#Mtd 2 of calculating income
dfa <- MCS1_derived[, c("mcsid", "ADOEDE00")]
dfa <- subset(dfa, ADOEDE00 > 0)
dfb <- MCS2_derived[, c("mcsid", "BDOEDE00")]
dfb <- subset(dfb, BDOEDE00 > 0)
dfc <- MCS3_derived[, c("mcsid", "CDOEDE00")]
dfc <- subset(dfc, CDOEDE00 > 0)

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")

df$meanincomeb <- log((df$ADOEDE00 + df$BDOEDE00 + df$CDOEDE00)/3)

Data4 <- full_join(Data4, df[, c("mcsid", "meanincomeb")], by = "mcsid")

###Var: Maternal depression (K6) at MCS1, 2, 3
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

dfa$adepress <- NA
dfa$adepress[dfa$armi >= 4] <- 1
dfa$adepress[dfa$armi < 4] <- 0

table(dfa$adepress)
#End of MCS1 segment

dfb <- MCS2_derived[c("mcsid", "BMKESS00")]
dfb <- subset(dfb, BMKESS00 >= 0)
dfb$bkess <- NA
dfb$bkess <- dfb$BMKESS00

dfb$bdepress <- NA
dfb$bdepress[dfb$bkess >= 6] <- 1
dfb$bdepress[dfb$bkess < 6] <- 0

table(dfb$bdepress)
#End of MCS2 segment

dfc <- MCS3_derived[c("mcsid", "CMKESS00")]
dfc <- subset(dfc, CMKESS00 >= 0)
dfc$ckess <- NA
dfc$ckess <- dfc$CMKESS00

dfc$cdepress <- NA
dfc$cdepress[dfc$ckess >= 6] <- 1
dfc$cdepress[dfc$ckess < 6] <- 0

table(dfc$cdepress)
#End of MCS3 segment

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")
df$mdepressave <- df$adepress + df$bdepress + df$cdepress
table(df$mdepressave)

Data4 <- full_join(Data4, df[c("mcsid", "mdepressave")], by = "mcsid")

###Child health endowment variables from Study 3:
###Var: Child birthweight
df <- MCS1_derived[c("mcsid", "ADBWGTA0")]
df <- subset(df, ADBWGTA0 > 0) 
df$birthweight[df$ADBWGTA0 <= 2.5] <- 1
df$birthweight[df$ADBWGTA0 > 2.5] <- 0

Data4 <- full_join(Data4, df[, c("mcsid", "birthweight")], by = "mcsid")

###Var: Child gestational age 
df <- MCS1_derived[c("mcsid", "ADGESTA0")]
df <- subset(df, ADGESTA0 > 0)
df$gestation[df$ADGESTA0 < 259] <- 1
df$gestation[df$ADGESTA0 >= 259] <- 0

Data4 <- full_join(Data4, df[, c("mcsid", "gestation")], by = "mcsid")

###Var: Mother drinking during pregnancy
df <- MCS1_parent[c("mcsid", "amdrof00")]
table(df$amdrof00)
df <- subset(df, amdrof00 > 0)
df$alcohol[df$amdrof00 == 7] <- 0 #never
df$alcohol[df$amdrof00 == 6] <- 1 #light
df$alcohol[df$amdrof00 == 5] <- 1
df$alcohol[df$amdrof00 == 4] <- 2 #moderate
df$alcohol[df$amdrof00 == 3] <- 3 #heavy
df$alcohol[df$amdrof00 == 2] <- 3
df$alcohol[df$amdrof00 == 1] <- 3
table(df$alcohol)

Data4 <- full_join(Data4, df[, c("mcsid", "alcohol")], by = "mcsid")

###Var: Mother's smoking during pregnancy
df <- MCS1_parent[c("mcsid", "amsmus0a", "amsmty00", "amsmev00", "amsmch00", "amcich00")]
df <- subset(df, amsmus0a > 0) #1 = no, all others = yes

table(df$amsmus0a)
table(df$amsmty00)
table(df$amsmev00)
table(df$amcich00)
df$smoke <- NA
df$smoke[df$amsmus0a > 1] <- 2
df$smoke[df$amsmus0a == 1] <- 0
df$smoke[df$amcich00 == 0] <- 1
table(df$smoke)
any(is.na(df) == T)

Data4 <- full_join(Data4, df[, c("mcsid", "smoke")], by = "mcsid")

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
sum(is.na(df$breastfeed) == T)

Data4 <- full_join(Data4, df[, c("mcsid", "breastfeed")], by = "mcsid")

###Var: Household parental employment
#Dual-earner, traditional household, briefly unemployed, chronic unemployed, female breadwinner, employed lone mother, non-employed lone mother
#See study page 2 of 6, Table 1 for full details on the 7 groups.
dfa <- MCS1_parent[, c("mcsid", "ampjob00", "appjob00", "amwkst00", "ampsex00", "appsex00")]
dfb <- MCS2_parent[, c("mcsid", "bmpjob00", "bppjob00", "bmwkwk00", "bmpsex00", "bppsex00")]
dfc <- MCS3_parent[, c("mcsid", "cmpjob00", "cppjob00", "cmwkwk00", "cmpsex00", "cppsex00")]

df <- full_join(dfa, dfb, by = "mcsid")
df <- full_join(df, dfc, by = "mcsid")

table(df$ampsex00) #1 = M, 2 = F
table(df$appsex00)
any(is.na(df$appsex00) == T)
df$lonemothera[df$ampsex00 == 2 & (is.na(df$appsex00) == T)] <- 1
df$lonemothera[is.na(df$lonemothera) == T] <- 0
table(df$lonemothera)
any(is.na(df$lonemothera) == T)

table(df$bmpsex00)
table(df$bppsex00)
any(is.na(df$bppsex00) == T)
df$lonemotherb[df$bmpsex00 == 2 & (is.na(df$bppsex00) == T)] <- 1
df$lonemotherb[is.na(df$lonemotherb) == T] <- 0
table(df$lonemotherb)
any(is.na(df$lonemotherb) == T)

table(df$cmpsex00)
table(df$cppsex00)
any(is.na(df$cppsex00) == T)
df$lonemotherc[df$cmpsex00 == 2 & (is.na(df$cppsex00) == T)] <- 1
df$lonemotherc[is.na(df$lonemotherc) == T] <- 0
table(df$lonemotherc)
any(is.na(df$lonemotherc) == T)

df$lonemother[df$lonemothera == 1 | df$lonemotherb == 1 | df$lonemotherc == 1] <- 1
df$lonemother[is.na(df$lonemother) == T] <- 0
table(df$lonemother)

table(dfa$ampjob00) #1 = yes in work
df$workingmum[df$cmpsex00 == 2 & (df$ampjob00 == 1 | df$bmpjob00 == 1 | df$cmpjob00 == 1)] <- 1
df$workingmum[is.na(df$workingmum) == T] <- 0
table(df$workingmum)

table(df$ampsex00)
table(df$appsex00)
any(is.na(df$ampsex00) == T)
any(is.na(df$appsex00) == T)
any(is.na(df$bmpsex00) == T)
any(is.na(df$bppsex00) == T)
any(is.na(df$cmpsex00) == T)
any(is.na(df$cppsex00) == T)

df$twoparent[(df$ampsex00 == 2 | df$ampsex00 == 1) & (df$appsex00 == 1 | df$appsex00 == 2) &
               (df$bmpsex00 == 2 | df$bmpsex00 == 1) & (df$bppsex00 == 1 | df$bppsex00 == 2) &
               (df$cmpsex00 == 2 | df$cmpsex00 == 1) & (df$cppsex00 == 1 | df$cppsex00 == 2)] <- 1
df$twoparent[is.na(df$twoparent) == T] <- 0
table(df$twoparent)
any(is.na(df$twoparent) == T)
any(df$twoparent == 1 & df$lonemother == 1)
any(df$twoparent == 0 & df$lonemother == 0) #i.e. there are single working fathers

#Grp 1 Dual Earner
table(df$appjob00) #1 = yes
df$dualearner[df$twoparent == 1 & ((df$appjob00 == 1 & df$ampjob00 == 1) & (df$bppjob00 == 1 & df$bmpjob00 == 1) &
                                     (df$cppjob00 == 1 & df$cmpjob00 == 1))] <- 1
df$dualearner[df$twoparent == 1 & (((df$appjob00 == 1 & df$ampjob00 == 1) & (df$bppjob00 == 1 & df$bmpjob00 == 1)) | 
                                     ((df$appjob00 == 1 & df$ampjob00 == 1) & (df$cppjob00 == 1 & df$cmpjob00 == 1)) | 
                                     ((df$bppjob00 == 1 & df$bmpjob00 == 1) & (df$cppjob00 == 1 & df$cmpjob00 == 1)))] <- 1
df$dualearner[is.na(df$dualearner) == T] <- 0
table(df$dualearner)

#Grp 2 Traditional
table(df$ampjob00)
df$traditional[df$twoparent == 1 & ((df$appjob00 == 1 & df$ampjob00 == 2) & (df$bppjob00 == 1 & df$bmpjob00 == 2) &
                                      (df$cppjob00 == 1 & df$cmpjob00 == 2))] <- 1
df$traditional[df$twoparent == 1 & (((df$appjob00 == 1 & df$ampjob00 == 2) & (df$bppjob00 == 1 & df$bmpjob00 == 2)) | 
                                      ((df$appjob00 == 1 & df$ampjob00 == 2) & (df$cppjob00 == 1 & df$cmpjob00 == 2)) | 
                                      ((df$bppjob00 == 1 & df$bmpjob00 == 2) & (df$cppjob00 == 1 & df$cmpjob00 == 2)))] <- 1
df$traditional[is.na(df$traditional) == T] <- 0
table(df$traditional)

#Grp 3 Briefly Unemployed
df$briefunemployed[df$twoparent == 1 & (df$appjob00 == 2 & df$ampjob00 == 2)] <- 1
df$briefunemployed[df$twoparent == 1 & (df$bppjob00 == 2 & df$bmpjob00 == 2)] <- 1
df$briefunemployed[df$twoparent == 1 & (df$cppjob00 == 2 & df$cmpjob00 == 2)] <- 1
df$briefunemployed[is.na(df$briefunemployed) == T] <- 0
table(df$briefunemployed)

#Grp 4 Chronic Unemployed
df$chronicunemployed[df$twoparent == 1 & ((df$appjob00 == 2 & df$ampjob00 == 2) & (df$bppjob00 == 2 & df$bmpjob00 == 2) &
                                            (df$cppjob00 == 2 & df$cmpjob00 == 2))] <- 1
df$chronicunemployed[df$twoparent == 1 & (((df$appjob00 == 2 & df$ampjob00 == 2) & (df$bppjob00 == 2 & df$bmpjob00 == 2)) | 
                                            ((df$appjob00 == 2 & df$ampjob00 == 2) & (df$cppjob00 == 2 & df$cmpjob00 == 2)) | 
                                            ((df$bppjob00 == 2 & df$bmpjob00 == 2) & (df$cppjob00 == 2 & df$cmpjob00 == 2)))] <- 1
df$chronicunemployed[is.na(df$chronicunemployed) == T] <- 0
table(df$chronicunemployed)

#Grp 5 Female Breadwinner
df$fembreadwinner[df$twoparent == 1 & ((df$appjob00 == 2 & df$ampjob00 == 1) | (df$bppjob00 == 2 & df$bmpjob00 == 1) | 
                                         (df$cppjob00 == 2 & df$cmpjob00 == 1))] <- 1
df$fembreadwinner[is.na(df$fembreadwinner) == T] <- 0
table(df$fembreadwinner)

#Grp 6 Employed Lone Mother
df$lonemotheremployed[df$lonemother == 1 & df$workingmum == 1] <- 1
df$lonemotheremployed[is.na(df$lonemotheremployed) == T] <- 0
table(df$lonemotheremployed)

#Grp 7 Unemployed Lone Mother
df$lonemotherunemployed[df$lonemother == 1 & df$workingmum == 0] <- 1
df$lonemotherunemployed[is.na(df$lonemotherunemployed) == T] <- 0
table(df$lonemotherunemployed)

#Combine into single var
df$hhemploymentgrp[df$dualearner == 1] <- 0
df$hhemploymentgrp[df$traditional == 1] <- 1
df$hhemploymentgrp[df$briefunemployed == 1] <- 2
df$hhemploymentgrp[df$chronicunemployed == 1] <- 3
df$hhemploymentgrp[df$fembreadwinner == 1] <- 4
df$hhemploymentgrp[df$lonemotheremployed == 1] <- 5
df$hhemploymentgrp[df$lonemotherunemployed == 1] <- 6
table(df$hhemploymentgrp)
sum(is.na(df$hhemploymentgrp) == T)

Data4 <- full_join(Data4, df[c("mcsid", "hhemploymentgrp")], by = "mcsid")

Data4$mpaidwork <- as.factor(Data4$mpaidwork)
Data4$ppaidwork <- as.factor(Data4$ppaidwork)
Data4$meducation <- as.factor(Data4$meducation)
Data4$incbandavefactor <- as.factor(Data4$incbandave)
Data4$hhemploymentgrp <- as.factor(Data4$hhemploymentgrp)

Data4boys <- subset(Data4, csex == 1)
Data4girls <- subset(Data4, csex == 0)

s4m1m <- glm(sdqdummy ~ factor(mpaidwork), family = binomial, data = Data4boys)
s4m1f <- glm(sdqdummy ~ factor(mpaidwork), family = binomial, data = Data4girls)

s4m2m <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork), 
             family = binomial, data = Data4boys)
s4m2f <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork), 
             family = binomial, data = Data4girls)

s4m3m <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork) + 
               factor(meducation) + incbandave, 
             family = binomial, data = Data4boys)
s4m3f <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork) + 
               factor(meducation) + incbandave, 
             family = binomial, data = Data4girls)

s4m4m <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork) + 
               factor(meducation) + incbandave + mdepressave, 
             family = binomial, data = Data4boys)
s4m4f <- glm(sdqdummy ~ factor(mpaidwork) + magecont + factor(ppaidwork) + 
               factor(meducation) + incbandave + mdepressave, 
             family = binomial, data = Data4girls)

summary(s4m1m)
summary(s4m1f)
summary(s4m2m)
summary(s4m2f)
summary(s4m3m)
summary(s4m3f)
summary(s4m4m)
summary(s4m4f)

#Factor 1
#Unadjusted
exp(summary(s4m1m)$coefficients[2, 1])
exp(confint(s4m1m)[2, 1])
exp(confint(s4m1m)[2, 2])

exp(summary(s4m1f)$coefficients[2, 1])
exp(confint(s4m1f)[2, 1])
exp(confint(s4m1f)[2, 2])

#Adjusted
exp(summary(s4m4m)$coefficients[2, 1])
exp(confint(s4m4m)[2, 1])
exp(confint(s4m4m)[2, 2])

exp(summary(s4m4f)$coefficients[2, 1])
exp(confint(s4m4f)[2, 1])
exp(confint(s4m4f)[2, 2])

#Factor 2
#Unadjusted
exp(summary(s4m1m)$coefficients[3, 1])
exp(confint(s4m1m)[3, 1])
exp(confint(s4m1m)[3, 2])

exp(summary(s4m1f)$coefficients[3, 1])
exp(confint(s4m1f)[3, 1])
exp(confint(s4m1f)[3, 2])

#Adjusted
exp(summary(s4m4m)$coefficients[3, 1])
exp(confint(s4m4m)[3, 1])
exp(confint(s4m4m)[3, 2])

exp(summary(s4m4f)$coefficients[3, 1])
exp(confint(s4m4f)[3, 1])
exp(confint(s4m4f)[3, 2])

#Factor 3
#Unadjusted
exp(summary(s4m1m)$coefficients[4, 1])
exp(confint(s4m1m)[4, 1])
exp(confint(s4m1m)[4, 2])

exp(summary(s4m1f)$coefficients[4, 1])
exp(confint(s4m1f)[4, 1])
exp(confint(s4m1f)[4, 2])

#Adjusted
exp(summary(s4m4m)$coefficients[4, 1])
exp(confint(s4m4m)[4, 1])
exp(confint(s4m4m)[4, 2])

exp(summary(s4m4f)$coefficients[4, 1])
exp(confint(s4m4f)[4, 1])
exp(confint(s4m4f)[4, 2])

s4m5m <- glm(sdqdummy ~ factor(hhemploymentgrp), family = binomial, data = Data4boys)
s4m5f <- glm(sdqdummy ~ factor(hhemploymentgrp), family = binomial, data = Data4girls)

s4m6m <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont, 
             family = binomial, data = Data4boys)
s4m6f <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont, 
             family = binomial, data = Data4girls)

s4m7m <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont + 
               factor(meducation) + incbandave, 
             family = binomial, data = Data4boys)
s4m7f <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont + 
               factor(meducation) + incbandave, 
             family = binomial, data = Data4girls)

s4m8m <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont + 
               factor(meducation) + incbandave + mdepressave, 
             family = binomial, data = Data4boys)
s4m8f <- glm(sdqdummy ~ factor(hhemploymentgrp) + magecont + 
               factor(meducation) + incbandave + mdepressave, 
             family = binomial, data = Data4girls)

summary(s4m5m)
summary(s4m5f)
summary(s4m6m)
summary(s4m6f)
summary(s4m7m)
summary(s4m7f)
summary(s4m8m)
summary(s4m8f)

Data4$mpaidwork <- as.factor(Data4$mpaidwork)
Data4$ppaidwork <- as.factor(Data4$ppaidwork)
Data4$meducation <- as.factor(Data4$meducation)
Data4$incbandavefactor <- as.factor(Data4$incbandave)
Data4$hhemploymentgrp <- as.factor(Data4$hhemploymentgrp)

Data4$magecat <- as.factor(Data4$magecat)
Data4$cethnic6 <- as.factor(Data4$cethnic6)
Data4$cethnic8 <- as.factor(Data4$cethnic8)
Data4$cethnic11 <- as.factor(Data4$cethnic11)
Data4$cageyearscat <- as.factor(Data4$cageyearscat)
Data4$birthweight <- as.factor(Data4$birthweight)
Data4$gestation <- as.factor(Data4$gestation)
Data4$breastfeed <- as.factor(Data4$breastfeed)
Data4$smoke <- as.factor(Data4$smoke)
Data4$alcohol <- as.factor(Data4$alcohol)

p1 <- c("", "+ magecont", "+ magecat")
p2 <- c("", "+ ppaidwork")
p3 <- c("", "+ meducation")
p4 <- c("", "+ incbandave", "+ incbandavefactor", "+ meanincomea", "+ meanincomeb")
p5 <- c("", "+ mdepressave")

p6 <- c("", "+ cethnicdummy", "+ cethnic6", "+ cethnic8", "+ cethnic11")
p7 <- c("", "+ cagedayscont", "+ cageyearscat")
p8 <- c("", "+ birthweight + gestation + breastfeed")
p9 <- c("", "+ smoke + alcohol")

coeff4m1 <- data.frame(NULL)
coeff4m2 <- data.frame(NULL)
coeff4m3 <- data.frame(NULL)
se4m1 <- data.frame(NULL)
se4m2 <- data.frame(NULL)
se4m3 <- data.frame(NULL)
n4m <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  model4m <- glm(paste("sdqdummy ~ mpaidwork", a, b, c, d, e, f, g, h, i), family = binomial, data = Data4boys)
                  coeff4m1 <- rbind(coeff4m1, summary(model4m)$coefficients[2, 1])
                  coeff4m2 <- rbind(coeff4m2, summary(model4m)$coefficients[3, 1])
                  coeff4m3 <- rbind(coeff4m3, summary(model4m)$coefficients[4, 1])
                  se4m1 <- rbind(se4m1, summary(model4m)$coefficients[2, 2])
                  se4m2 <- rbind(se4m2, summary(model4m)$coefficients[3, 2])
                  se4m3 <- rbind(se4m3, summary(model4m)$coefficients[4, 2])
                  n4m <- rbind(n4m, length(model4m$model$sdqdummy))
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeff4m1)[1] <- c("Employ")
colnames(coeff4m2)[1] <- c("Employ")
colnames(coeff4m3)[1] <- c("Employ")
colnames(se4m1)[1] <- c("SE")
colnames(se4m2)[1] <- c("SE")
colnames(se4m3)[1] <- c("SE")
colnames(n4m)[1] <- c("N")

table4m1 <- cbind(coeff4m1, se4m1)
table4m1 <- cbind(table4m1, n4m)

table4m2 <- cbind(coeff4m2, se4m2)
table4m2 <- cbind(table4m2, n4m)

table4m3 <- cbind(coeff4m3, se4m3)
table4m3 <- cbind(table4m3, n4m)

table4m1$UB <- table4m1$Employ + 1.96*(table4m1$SE)
table4m1$LB <- table4m1$Employ - 1.96*(table4m1$SE)

table4m2$UB <- table4m2$Employ + 1.96*(table4m2$SE)
table4m2$LB <- table4m2$Employ - 1.96*(table4m2$SE)

table4m3$UB <- table4m3$Employ + 1.96*(table4m3$SE)
table4m3$LB <- table4m3$Employ - 1.96*(table4m3$SE)

table4m1 <- table4m1 %>% arrange(Employ) 
num <- rep(1:nrow(table4m1))
table4m1 <- cbind(table4m1, num)

table4m2 <- table4m2 %>% arrange(Employ) 
num <- rep(1:nrow(table4m2))
table4m2 <- cbind(table4m2, num)

table4m3 <- table4m3 %>% arrange(Employ) 
num <- rep(1:nrow(table4m3))
table4m3 <- cbind(table4m3, num)

coeff4f1 <- data.frame(NULL)
coeff4f2 <- data.frame(NULL)
coeff4f3 <- data.frame(NULL)
se4f1 <- data.frame(NULL)
se4f2 <- data.frame(NULL)
se4f3 <- data.frame(NULL)
n4f <- data.frame(NULL)

for (a in p1) {
  for (b in p2) {
    for (c in p3) {
      for (d in p4) {
        for (e in p5) {
          for (f in p6) {
            for (g in p7) {
              for (h in p8) {
                for (i in p9) {
                  model4f <- glm(paste("sdqdummy ~ mpaidwork", a, b, c, d, e, f, g, h, i), family = binomial, data = Data4girls)
                  coeff4f1 <- rbind(coeff4f1, summary(model4f)$coefficients[2, 1])
                  coeff4f2 <- rbind(coeff4f2, summary(model4f)$coefficients[3, 1])
                  coeff4f3 <- rbind(coeff4f3, summary(model4f)$coefficients[4, 1])
                  se4f1 <- rbind(se4f1, summary(model4f)$coefficients[2, 2])
                  se4f2 <- rbind(se4f2, summary(model4f)$coefficients[3, 2])
                  se4f3 <- rbind(se4f3, summary(model4f)$coefficients[4, 2])
                  n4f <- rbind(n4f, length(model4f$model$sdqdummy))
                }
              }
            }
          }
        }
      }
    }
  }
}

colnames(coeff4f1)[1] <- c("Employ")
colnames(coeff4f2)[1] <- c("Employ")
colnames(coeff4f3)[1] <- c("Employ")
colnames(se4f1)[1] <- c("SE")
colnames(se4f2)[1] <- c("SE")
colnames(se4f3)[1] <- c("SE")
colnames(n4f)[1] <- c("N")

table4f1 <- cbind(coeff4f1, se4f1)
table4f1 <- cbind(table4f1, n4f)

table4f2 <- cbind(coeff4f2, se4f2)
table4f2 <- cbind(table4f2, n4f)

table4f3 <- cbind(coeff4f3, se4f3)
table4f3 <- cbind(table4f3, n4f)

table4f1$UB <- table4f1$Employ + 1.96*(table4f1$SE)
table4f1$LB <- table4f1$Employ - 1.96*(table4f1$SE)

table4f2$UB <- table4f2$Employ + 1.96*(table4f2$SE)
table4f2$LB <- table4f2$Employ - 1.96*(table4f2$SE)

table4f3$UB <- table4f3$Employ + 1.96*(table4f3$SE)
table4f3$LB <- table4f3$Employ - 1.96*(table4f3$SE)

table4f1 <- table4f1 %>% arrange(Employ) 
num <- rep(1:nrow(table4f1))
table4f1 <- cbind(table4f1, num)

table4f2 <- table4f2 %>% arrange(Employ) 
num <- rep(1:nrow(table4f2))
table4f2 <- cbind(table4f2, num)

table4f3 <- table4f3 %>% arrange(Employ) 
num <- rep(1:nrow(table4f3))
table4f3 <- cbind(table4f3, num)

#This function converts logits (coefficients) to probability.
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

table4m1$OR <- exp(table4m1$Employ)
table4m1$ORUB <- exp(table4m1$UB)
table4m1$ORLB <- exp(table4m1$LB)
table4m2$OR <- exp(table4m2$Employ)
table4m2$ORUB <- exp(table4m2$UB)
table4m2$ORLB <- exp(table4m2$LB)
table4m3$OR <- exp(table4m3$Employ)
table4m3$ORUB <- exp(table4m3$UB)
table4m3$ORLB <- exp(table4m3$LB)
table4f1$OR <- exp(table4f1$Employ)
table4f1$ORUB <- exp(table4f1$UB)
table4f1$ORLB <- exp(table4f1$LB)
table4f2$OR <- exp(table4f2$Employ)
table4f2$ORUB <- exp(table4f2$UB)
table4f2$ORLB <- exp(table4f2$LB)
table4f3$OR <- exp(table4f3$Employ)
table4f3$ORUB <- exp(table4f3$UB)
table4f3$ORLB <- exp(table4f3$LB)

plot(table4m1$num, table4m1$OR, ylim = range(c(table4m1$ORLB, table4m1$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Boys; Two Waves)", xlab = "Models")
for (i in c(1:nrow(table4m1))) {
  segments(table4m1$num[i], table4m1$ORLB[i], table4m1$num[i], table4m1$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4m1$num, table4m1$OR, pch = ".", col = "blue")
abline(h = 1)
points(6550, 1.15, pch = 19, col = "red") #red is unadjusted
points(0, 0.93, pch = 19, col = "brown") #brown is adjusted

plot(table4m1$N)

plot(table4m2$num, table4m2$OR, ylim = range(c(table4m2$ORLB, table4m2$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Boys; One Wave)", xlab = "Models")
for (i in c(1:nrow(table4m2))) {
  segments(table4m2$num[i], table4m2$ORLB[i], table4m2$num[i], table4m2$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4m2$num, table4m2$OR, pch = ".", col = "blue")
abline(h = 1)
points(6850, 1.58, pch = 19, col = "red") #red is unadjusted
points(0, 0.94, pch = 19, col = "brown") #brown is adjusted

plot(table4m3$num, table4m3$OR, ylim = range(c(table4m3$ORLB, table4m3$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Boys; No Waves)", xlab = "Models")
for (i in c(1:nrow(table4m3))) {
  segments(table4m3$num[i], table4m3$ORLB[i], table4m3$num[i], table4m3$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4m3$num, table4m3$OR, pch = ".", col = "blue")
abline(h = 1)
points(6875, 2.29, pch = 19, col = "red") #red is unadjusted
points(0, 1.00, pch = 19, col = "brown") #brown is adjusted

plot(table4f1$num, table4f1$OR, ylim = range(c(table4f1$ORLB, table4f1$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Girls; Two Waves)", xlab = "Models")
for (i in c(1:nrow(table4f1))) {
  segments(table4f1$num[i], table4f1$ORLB[i], table4f1$num[i], table4f1$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4f1$num, table4f1$OR, pch = ".", col = "blue")
abline(h = 1)
points(6575, 2.27, pch = 19, col = "red") #red is unadjusted
points(1425, 1.49, pch = 19, col = "brown") #brown is adjusted

plot(table4f1$N)

plot(table4f2$num, table4f2$OR, ylim = range(c(table4f2$ORLB, table4f2$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Girls; One Wave)", xlab = "Models")
for (i in c(1:nrow(table4f2))) {
  segments(table4f2$num[i], table4f2$ORLB[i], table4f2$num[i], table4f2$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4f2$num, table4f2$OR, pch = ".", col = "blue")
abline(h = 1)
points(7050, 2.84, pch = 19, col = "red") #red is unadjusted
points(1425, 1.39, pch = 19, col = "brown") #brown is adjusted

plot(table4f3$num, table4f3$OR, ylim = range(c(table4f3$ORLB, table4f3$ORUB)), pch =".", col = "blue",
     ylab = "Odds Ratio (Girls; No Waves)", xlab = "Models")
for (i in c(1:nrow(table4f3))) {
  segments(table4f3$num[i], table4f3$ORLB[i], table4f3$num[i], table4f3$ORUB[i], 
           col = "light blue", lwd = 0.01)
}
points(table4f3$num, table4f3$OR, pch = ".", col = "blue")
abline(h = 1)
points(6900, 6.00, pch = 19, col = "red") #red is unadjusted
points(1600, 2.01, pch = 19, col = "brown") #brown is adjusted
