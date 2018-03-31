

### Objective: monitoring production parameter overtime to early detection of disease 
# introduction into the herds.

# Keep in mind that we are going to use the same process for other 4 parameters.


### Variable of interesting ("response"): Diff_Totalborn_Alive
# Farm: 14 farms

### Method: EWMA
#  Parameters:
#       1. lambda (smoothing parameter) = 0.4
#       2. sigmas (number of sigmas to use for computing control limits) = 3 


####-----------------------------------------------------------------------------####

### Packages:
p <- c("psych", "qcc", "ggplot2", "plyr")
lapply(p, require, character.only = TRUE)


####-----------------------------------------------------------------------------####

### Import data:
SPC <- read.csv("SPC_allherds.csv", header=TRUE,sep=",")
str(SPC)
dim(SPC)

# Date -> as.date:
SPC$Period.Start <- as.Date(SPC$Period.Start, format= "%m/%d/%Y")

## Descriptive:
str(SPC)
summary(SPC$Farm)
describe(SPC$Diff_Totalborn_Alive)

# Split dataset to separate analysis:
BR <- subset(SPC, Farm == "BR")
CF <- subset(SPC, Farm == "CF")
HS <- subset(SPC, Farm == "HS")
KR <- subset(SPC, Farm == "KR")
PP <- subset(SPC, Farm == "PP")
PR <- subset(SPC, Farm == "PR")
PE <- subset(SPC, Farm == "PE")
SE <- subset(SPC, Farm == "SE")
S1 <- subset(SPC, Farm == "S1")
S2 <- subset(SPC, Farm == "S2")
SRE <- subset(SPC, Farm == "SRE")
TY <- subset(SPC, Farm == "TY")
TE <- subset(SPC, Farm == "TE")
WK <- subset(SPC, Farm == "WK")

####-----------------------------------------------------------------------------####

####                         Descriptive Analysis                                ####


#BR = 108 - 1 PRRS Status
describeBy(BR$Diff_Totalborn_Alive, BR$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = BR)

ggplot(BR, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=25) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = BR)

describe(BR$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=BR, geom="line", ylim = c(2250, 2750))


##########################################################################################

#CF = 108  - 1 PRRS status
summary(CF$Diff_Totalborn_Alive, CF$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = CF)

ggplot(CF, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = CF)

describe(CF$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=CF, geom="line", ylim = c(4200, 5000))



##########################################################################################

#HS = 108  - 2 PRRS status
describeBy(HS$Diff_Totalborn_Alive, HS$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = HS)

ggplot(HS, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.HS <- ddply(HS, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))

ggplot(HS, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=35, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.HS, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = HS)

describe(HS$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=HS, geom="line", ylim = c(4800, 5300))


##########################################################################################

#KR = 108 - 2 PRRS status - DOUBLE-CHECK
describeBy(KR$Diff_Totalborn_Alive, KR$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = KR)

ggplot(KR, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.KR <- ddply(KR, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(KR, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=35, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.KR, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")


qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = KR)

describe(KR$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=KR, geom="line", ylim = c(4900, 5250))


##########################################################################################

#PP = 108 - 3 PRRS status
describeBy(PP$Diff_Totalborn_Alive, PP$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = PP)

ggplot(PP, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.PP <- ddply(PP, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(PP, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=35, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.PP, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = PP)

describe(PP$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=PP, geom="line", ylim = c(4900, 5400))


##########################################################################################

#PR = 108  - 1 PRRS status
describeBy(PR$Diff_Totalborn_Alive, PR$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = PR)

ggplot(PR, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = PR)

describe(PR$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=PR, geom="line", ylim = c(2250, 2600))


##########################################################################################

#PE = 108 - 3 PRRS status
describeBy(PE$Diff_Totalborn_Alive, PE$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = PE)

ggplot(PE, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.pe <- ddply(PE, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(PE, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=30, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.pe, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = PE)

describe(PE$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=PE, geom="line", ylim = c(4700, 5200))


##########################################################################################

#SE = 108 - 2 PRRS status
describeBy(SRE$Diff_Totalborn_Alive, SRE$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = SRE)

ggplot(SRE, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.sandy <- ddply(SRE, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(SRE, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=20, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.sandy, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = SRE)

describe(SRE$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=SRE, geom="line", ylim = c(3000, 3600))


##########################################################################################

#S1 = 105 - 4 PRRS status
describeBy(S1$Diff_Totalborn_Alive, S1$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = S1)

ggplot(S1, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.s1 <- ddply(S1, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(S1, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=20, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m1, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = S1)

describe(S1$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=S1, geom="line", ylim = c(1050, 1500))


##########################################################################################

#S2 = 108 -- Batch farrowing - 4 PRRS status
describeBy(S2$Diff_Totalborn_Alive, S2$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = S2)

ggplot(S2, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.s2 <- ddply(SE, "PRRS.status", summarise, rating.mean=mean(SE$Diff_Totalborn_Alive))
ggplot(S2, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=15, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.s2, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = S2)

describe(S2$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=S2, geom="line", ylim = c(5100, 5800))


##########################################################################################

#SRE = 108 - 3 PRRS status
describeBy(SRE$Diff_Totalborn_Alive, SRE$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = SRE)

ggplot(SRE, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.se <- ddply(SRE, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(SRE, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=15, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.se, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = SRE)

describe(SRE$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=SRE, geom="line", ylim = c(2500, 3000))


##########################################################################################

#TY = 108 - 1 PRRS status
describeBy(TY$Diff_Totalborn_Alive, TY$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = TY)

ggplot(TY, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

ggplot(TY, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=20) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = TY)

describe(TY$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=TY, geom="line", ylim = c(2300, 2500))


##########################################################################################

#TE = 108 - 3 PRRS status
describeBy(TE$Diff_Totalborn_Alive, TE$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = TE)

ggplot(TE, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=100) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.tr <- ddply(TE, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(TE, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=30, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.tr, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = TE)

describe(TE$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=TE, geom="line", ylim = c(5000, 5550))


##########################################################################################

#WK = 108 - 3 PRRS status
describeBy(WK$Diff_Totalborn_Alive, WK$PRRS.status)

qplot(PRRS.status, Diff_Totalborn_Alive ,geom = "boxplot", data = WK)

ggplot(WK, aes(x=Diff_Totalborn_Alive)) +
  geom_histogram(binwidth=30) +
  ggtitle("Diff. Totalborn and Born Alive by Status")

m.ww <- ddply(WK, "PRRS.status", summarise, rating.mean=mean(Diff_Totalborn_Alive))
ggplot(WK, aes(x=Diff_Totalborn_Alive)) + 
  geom_histogram(binwidth=30, colour="black", fill="white") + 
  facet_grid(PRRS.status ~ .)+
  geom_vline(data=m.ww, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  ggtitle("Diff. Totalborn and Born Alive by Status")

qplot(x="SowInventory", AvSowInventory ,geom = "boxplot", data = WK)

describe(WK$AvSowInventory)
qplot(Period.Start, AvSowInventory, data=WK, geom="line", ylim = c(4700, 5350))


####---------------------------------------------------------------------------------#####
####                                                                                 #####
####                                    EWMA                                         #####

## Package: qcc
??qcc

#### BR = 108 - 1 PRRS Status

# Baseline - 21 weeks: USED THIS!!!!
BR.Baseline <-  BR$Diff_Totalborn_Alive[1:21]
BR.b <- ewma(BR.Baseline, lambda=0.4, nsigmas=3)
summary(BR.b)

# Compared with Baseline:
BR <- BR$Diff_Totalborn_Alive[22:108]
BR.ewma <- ewma(BR.Baseline, lambda=0.4, nsigmas=3, newdata= BR, plot = T)
summary(BR.ewma)

#Points out of control:
BR.ewma$violations


## EWMA Without Baseline
BR<- BR$Diff_Totalborn_Alive
describe(BR)

b.ewma <- ewma(BR, lambda=0.4, nsigmas=3)
summary(b.ewma)

#Points out of control:
b.ewma$violations


#########################################################################################

#### CF = 108  - 1 PRRS status
CF<- CF$Diff_Totalborn_Alive
describe(CF)

# 2nd - Baseline - 21 weeks: USED THIS!!!!
CF.Baseline <-  CF$Diff_Totalborn_Alive[1:21]
CF.b <- ewma(CF.Baseline, lambda=0.4, nsigmas=3)
summary(CF.b)

# Compared with Baseline:
CF <- CF$Diff_Totalborn_Alive[22:108]
CF.ewma <- ewma(CF.Baseline, lambda=0.4, nsigmas=3, newdata= CF, plot = T)
summary(CF.ewma)

#Points out of control:
CF.ewma$violations


## EWMA Without Baseline
c.ewma <- ewma(CF, lambda=0.4, nsigmas=3)
summary(c.ewma)

#Points out of control:
c.ewma$violations


#########################################################################################

#### HS = 108  - 2 PRRS status

# 1st or 2nd model detect 2 weeks before, 3rd model detect 1 week before.

# 1st - Baseline - 21 weeks:  USED THIS!!!!
HS.Baseline <-  HS$Diff_Totalborn_Alive[1:21]
HS.b <- ewma(HS.Baseline, lambda=0.4, nsigmas=3)
summary(HS.b)

# Compared with Baseline:
HS <- HS$Diff_Totalborn_Alive[22:108]
HS.ewma <- ewma(HS.Baseline, lambda=0.4, nsigmas=3, newdata= HS, plot = T)
summary(HS.ewma)

#Points out of control:
HS.ewma$violations


#########################################################################################

#### KR = 108 - 2 PRRS status 

# 1st - Baseline - PRRSV free and PEDV status 1/2: USED THIS ONE
KR.baseline <-  KR$Diff_Totalborn_Alive[1:21]
KR.b <- ewma(KR.baseline, lambda=0.4, nsigmas=3)
summary(KR.b)

# Compared with Baseline:
KR <- KR$Diff_Totalborn_Alive[22:108]
KR.ewma <- ewma(KR.baseline, lambda=0.4, nsigmas=3, newdata= KR, plot = T)
summary(KR.ewma)

#Points out of control:
KR.ewma$violations


#########################################################################################

#### PP = 108 - 3 PRRS status

# 2nd - Baseline = 35 weeks:
PP.baseline <-  PP$Diff_Totalborn_Alive[74:108]

PP.b <- ewma(PP.baseline, lambda=0.4, nsigmas=3)
summary(PP.b)

# Compared with Baseline:
PP <- PP$Diff_Totalborn_Alive[1:73]
PP.ewma <- ewma(PP.baseline, lambda=0.4, nsigmas=3, newdata= PP, plot = T)
summary(PP.ewma)

#Points out of control:
PP.ewma$violations


#########################################################################################

#### PR = 108  - 1 PRRS status

# EWMA without baseline: USED this one, much noise using baseline.
PR.1<- PR$Diff_Totalborn_Alive
describe(PR.1)

p.ewma <- ewma(PR.1, lambda=0.4, nsigmas=3)
summary(p.ewma)

#Points out of control:
p.ewma$violations


#########################################################################################

#### SE = 108 - 3 PRRS status

# 1st Baseline - 21 weeks: THIS ONE!!!
SE$Diff_Totalborn_Alive
SE.baseline <-  SE$Diff_Totalborn_Alive[1:21]
SE.b <- ewma(SE.baseline, lambda=0.4, nsigmas=3)
summary(SE.b)

# Compared with Baseline:
SE.N <- SE$Diff_Totalborn_Alive[22:108]

SE.ewma <- ewma(SE.baseline, lambda=0.4, nsigmas=3, newdata= SE.N, plot = T)
summary(SE.ewma)

#Points out of control:
SE.ewma$violations


#########################################################################################

#### TY = 108 - 1 PRRS status

# Baseline - 21 weeks:
TY.baseline <-  TY$Diff_Totalborn_Alive[1:21] # 23 weeks baseline
TY.b <- ewma(TY.baseline, lambda=0.4, nsigmas=3)
summary(TY.b)

# Compared with Baseline:
TEY <- TY$Diff_Totalborn_Alive[22:108]
TY.ewma <- ewma(TY.baseline, lambda=0.4, nsigmas=3, newdata= TEY, plot = T)
summary(TY.ewma)

#Points out of control:
TY.ewma$violations



#########################################################################################

#### PE = 108 - 3 PRRS status

# Baseline - 21 weeks: with PED - USED THIS ONE
PR.baseline <-  PE$Diff_Totalborn_Alive[1:21] # 16 weeks baseline
PR.b <- ewma(PR.baseline, lambda=0.4, nsigmas=3)
summary(PR.b)

# Compared with Baseline:
PT <- PE$Diff_Totalborn_Alive[17:108]

PR.ewma <- ewma(PR.baseline, lambda=0.4, nsigmas=3, newdata= PT, plot = T)
summary(PR.ewma)

#Points out of control:
PR.ewma$violations


#########################################################################################

#### SRE = 108 - 2 PRRS status (free)

# 1st Baseline - 21 weeks: USED THIS !!!
SRE.baseline <-  SRE$Diff_Totalborn_Alive[1:21] # 21 weeks baseline
SRE.b <- ewma(SRE.baseline, lambda=0.4, nsigmas=3)
summary(SRE.b)

# Compared with Baseline:
SY <- SRE$Diff_Totalborn_Alive[22:108]
SRE.ewma <- ewma(SRE.baseline, lambda=0.4, nsigmas=3, newdata= SY, plot = T)
summary(SRE.ewma)

#Points out of control:
SRE.ewma$violations



#########################################################################################

#### S1 = 105 - 4 PRRS status -  Batch farrowing - NOT USED (12-20-2016)

S1.values <- c (74,87,57,80,74.5,84.75,74.5,75,87.25,80.75,85.5,87.75,79,73.75,73.25,72.25,
                  71.75,77.5,82.5,84.5,72.25,62,67.5,67.25,70.75,80,70.75,68,74.5,73.5,73.75,
                  76.5,72.75,72.5,65.25,62.25,61.25,65.5,68.75,72.25,76,76.25,88.75,85.75,82.5,
                  67,62,59.25,65,72.25,73,82,84.75,82.25,83,76.75,67.25,69.25,69.5,68.5,77.25,
                  76.75,77.75,89.25,66,50.25,54.75,65.25,64.25,64.25,64.5,60.25,60.25,60.25,
                  76.25,68,68,68,51.5,57.5,57.5,57.5,58,45.5,45.5,45.5,234.75,317.25,317.25,322,
                  253.25,214,214,209.25,170.25,148.25,148.25,148.25,83.25,57.75,57.75,57.75,63.25,
                  77.5,77.5)
sum(S1.values)


# 1st Baseline - 21 weeks: week beggining 45
S1.baseline <-  S1.values[45:65] # 21 weeks baseline

S1.b <- ewma(S1.baseline, lambda=0.4, nsigmas=3)
summary(S1.b)

# Compared with Baseline:
SITE1 <- S1.values[66:105]
SITE1.ewma <- ewma(S1.baseline, lambda=0.4, nsigmas=3, newdata= SITE1, plot = T)
summary(SITE1.ewma)

#Points out of control:
SITE1.ewma$violations


#########################################################################################

#### S2 = 108 - 4 PRRS status - Don't have enough data to set a baseline production 

#                       NOT USED (12-20-2016)

# 1st Baseline - 10 weeks
S2.baseline <-  S2$Diff_Totalborn_Alive[17:26] # 10 weeks baseline

S2.b <- ewma(S2.baseline, lambda=0.4, nsigmas=3)
summary(S2.b)

# Compared with Baseline:
SITE2 <- S2$Diff_Totalborn_Alive[27:108] #26:81
SITE2.ewma <- ewma(S2.baseline, lambda=0.4, nsigmas=3, newdata= SITE2, plot = T)
summary(SITE2.ewma)

#Points out of control:
SITE2.ewma$violations


#########################################################################################

#### TE = 108 - 3 PRRS status

# 1st Baseline -  21 weeks - USED THIS!!!!
TE.baseline <-  TE$Diff_Totalborn_Alive[13:33] # 21 weeks baseline

TE.b <- ewma(TE.baseline, lambda=0.4, nsigmas=3)
summary(TE.b)

# Compared with Baseline:
TE <- TE$Diff_Totalborn_Alive[34:108]

TE.ewma <- ewma(TE.baseline, lambda=0.4, nsigmas=3, 
                         newdata= TE, plot = T)
summary(TE.ewma)

#Points out of control:
TE.ewma$violations


#########################################################################################

#### WK = 108 - 3 PRRS status

# Baseline -  21 weeks
WK.baseline <-  WK$Diff_Totalborn_Alive[17:37] # 21 weeks baseline

WK.b <- ewma(WK.baseline, lambda=0.4, nsigmas=3)
summary(WK.b)

# Compared with Baseline:
Wk <- WK$Diff_Totalborn_Alive[38:108]

WK.ewma <- ewma(WK.baseline, lambda=0.4, nsigmas=3, 
                         newdata= Wk, plot = T)
summary(WK.ewma)

#Points out of control:
WK.ewma$violations


#########################################################################################

### Main script:
library(qcc)

## Parameters:
# Sigma = 3
# Lambda = 0.4

# Baseline:
baseline <- 
  q <- ewma(baseline, lambda=0.4, nsigmas=3)
summary(q)

# Compared with Baseline:
new.data <- 
  q <- ewma(baseline, lambda=0.4, nsigmas=3, newdata= new.data, plot = FALSE)
summary(q)
plot(q)


#########################################################################################
#########################################################################################
