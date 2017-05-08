
# Loading packages

library(dplyr)
library(TOSTER)

# Setting directory

dir <- "C:/MANGO/"

# Importing data

wide.data <- read.csv(paste0(dir, "MANGO 040517 DSMB.csv"))[1:240,]
long.data <- read.csv(paste0(dir, "MANGO 040517 long.csv"))
codes     <- read.csv(paste0(dir, "MANGO codes DSMB.csv"))

# Merging in dosage (trial arm) data

long.data <- merge(x=long.data, y=codes, by="id")
wide.data <- merge(x=wide.data, y=codes, by="id")

# Change dates to date format

wide.data$date_adm <- as.Date(wide.data$date_adm, format="%m/%d/%Y")
wide.data$date_exit <- as.Date(wide.data$date_exit, format="%m/%d/%Y")
wide.data$dint0 <- as.Date(wide.data$dint0, format="%m/%d/%Y")
wide.data$dint2 <- as.Date(wide.data$dint2, format="%m/%d/%Y")

# Adding change in whz, waz, haz, weight gain by in week 0-2 vs. week 3+

wide.data <- wide.data %>% 
              mutate(avg.whz.change  = whz_exit - whz0,
                     avg.muac.change = muac_exit - muac0,
                     #avg.haz.change = haz_exit - haz0,
                     weight.gain.02 = (weight2 - weight0) / as.numeric(dint2 - dint0),
                     weight.gain.3 = (weight_exit - weight2) / as.numeric(date_exit - dint2))

# Grouping data by dosage

long.data <- group_by(long.data, dosage)
wide.data <- group_by(wide.data, dosage)

# Making sure the samples are balanced

  # Gender
    table(wide.data$dosage, wide.data$sex)    # Dose 1: 50.4% female, Dose 2: 55.5% female
    prop.table(table(wide.data$dosage, wide.data$sex,exclude=""),1)
    
  # Criteria of admission
    table(wide.data$dosage, wide.data$criteria_adm)  
    prop.table(table(wide.data$dosage, wide.data$criteria_adm, exclude=""),1)
    
  # Age
    wide.data %>%                               # Average age of those treated:
      group_by(dosage) %>%                      #   Dose 1: 13.8 years old
        summarise(mean.age=mean(age, na.rm=T),
                  sd.age=sd(age, na.rm=T))  #   Dose 2: 12.5 years old

  # MUAC at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.muac=mean(muac0, na.rm=T),
                sd.muac=sd(muac0, na.rm=T))             
                               
  # weight at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.entry=mean(weight0, na.rm=T),
                sd.weight.entry=sd(weight0, na.rm=T)) 
    
  # weight for height z-score at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
                
    
  # height for age z-score at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.haz.entry=mean(haz0, na.rm=T),
                sd.haz.entry=sd(haz0, na.rm=T)) 
    
    
  # Length of stay
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T)) 
    
  # Rate of weight gain from admission to exit (g/kg/d)
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d, na.rm=T)) 
    
  
# Looking at differences in outcomes
    
    # change in MUAC
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.muac.change=mean(avg.muac.change, na.rm=T),
                sd.muac.change=sd(avg.muac.change, na.rm=T))
                
    # change in weight
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.change=mean(avg.weight.change, na.rm=T),
                sd.weight.change=sd(avg.weight.change, na.rm=T))
    
    # change in weight for height z-score
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.whz.change=mean(avg.whz.change, na.rm=T),
                sd.whz.change=sd(avg.whz.change, na.rm=T))
    
    # change in height for age z-score
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.haz.change=mean(avg.haz.change, na.rm=T),
                sd.haz.change=sd(avg.haz.change, na.rm=T))
    
    # Rate of weight gain from admission to exit (g/kg/d) *Weeks 0-2
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.02, na.rm=T),
                sd.weight.gain=sd(weight.gain.02, na.rm=T))

    # Rate of weight gain from admission to exit (g/kg/d) *Weeks 3+
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.3, na.rm=T)) 
    
    
# 2x2 table of reason for exit
    
referral.table <- table(wide.data$dosage, wide.data$exit, exclude="")

referral.prop.table <- prop.table(referral.table, 1)

#

table(wide.data$dosage, wide.data$exit, exclude="")



# Rate of weight gain

dat.compare <- wide.data %>%
                group_by(dosage) %>%
                  summarise(mean.weight.gain=mean(wgain_d, na.rm=T),
                            se.weight.gain=sd(wgain_d, na.rm=T)/sqrt(120)) %>%    #fix n later
                     mutate(upper.CI = mean.weight.gain + 1.96 * sd.weight.gain,
                            lower.CI = mean.weight.gain - 1.96 * sd.weight.gain)

dat.compare <- data.frame(dat.compare)
n <- c(121, 119)
dat.compare <- cbind(dat.compare, n)

# Independent samples t-test

dat.compare <- wide.data %>%
  group_by(dosage) %>%
  summarise(mean.weight.gain=mean(wgain_d, na.rm=T),
            sd.weight.gain=sd(wgain_d, na.rm=T)) 

dat.compare <- data.frame(dat.compare)
n <- c(121, 119)
dat.compare <- cbind(dat.compare, n)


TOSTtwo(m1=dat.compare[1,2],
        m2=dat.compare[2,2],
        sd1=dat.compare[1,3],
        sd2=dat.compare[2,3],
        n1=dat.compare[1,4],
        n2=dat.compare[2,4],
        low_eqbound_d=-.05,
        high_eqbound_d=.05,
        alpha = 0.05,
        var.equal=FALSE)

# https://cran.rstudio.com/web/packages/TOSTER/vignettes/IntroductionToTOSTER.html

t.test(wide.data$wgain_d ~ wide.data$dosage)

# TOST: Two One-Sided Tests Procedure
# NHST: Null Hypothesis Significance Testing






# Scrap

# Week of exit
wide.data %>%
  group_by(dosage) %>%
  summarise(mean.exit.week=mean(exit_visit, na.rm=T))