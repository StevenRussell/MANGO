
#-------------------------------------------------------------------------------------------------------------------------------------#
# In this code, we performing exploratory data analysis on the MANGO dataset. 
#
# Created by Steven Russell
# Last updated: May 11, 2017
#-------------------------------------------------------------------------------------------------------------------------------------#

# Loading packages

library(dplyr)
library(stringr)
library(TOSTER)
library(MASS)
library(C50)  # may not need this
library(xtable)
library(gridExtra)

# Setting directory

dir <- "C:/MANGO/"

# Importing data

wide.data <- read.csv(paste0(dir, "MANGO 090517 DSMB.csv"))[1:240,]
long.data <- read.csv(paste0(dir, "MANGO 040517 long.csv"))
codes     <- read.csv(paste0(dir, "MANGO codes DSMB.csv"))

# Merging in dosage (trial arm) data

long.data <- merge(x=long.data, y=codes, by="id")
wide.data <- merge(x=wide.data, y=codes, by="id")

# Selecting all date variables and converting them to date format

Date_Convert <- function(var){
  as.Date(var, format="%m/%d/%Y")
}

date.vars <- c( "date_adm", "date_exit", str_subset(names(wide.data), "dint") )

wide.data[date.vars] <- lapply(wide.data[date.vars], Date_Convert)

# Adding change in whz, waz, haz, weight gain by in week 0-2 vs. week 3+

wide.data <- wide.data %>% 
              mutate(whz.change  = whz_exit - whz0,
                     muac.change = muac_exit - muac0,
                     #haz.change = haz_exit - haz0,
                     weight.gain.02 = (weight2 - weight0)*1000 / as.numeric(dint2 - dint0) / weight0,
                     weight.gain.3 = (weight_exit - weight2)*1000 / as.numeric(date_exit - dint2) / weight2,
                     recovered.status = ifelse(exit == "Recovered", 1, 0),
                     referred.status = ifelse(exit == "Referred", 1, 0),
                     default.status = ifelse(exit == "Defaulted", 1, 0),
                     treatment.failure = ifelse(exit == "Referred" | exit == "Died", 1, 0),
                     finished = ifelse(is.na(exit_visit) == F, 1, 0),
                     current.week = ifelse(is.na(exit_visit) == T & is.na(weight15) == F,15,
                                    ifelse(is.na(exit_visit) == T & is.na(weight14) == F,14,
                                    ifelse(is.na(exit_visit) == T & is.na(weight13) == F,13,
                                    ifelse(is.na(exit_visit) == T & is.na(weight12) == F,12,
                                    ifelse(is.na(exit_visit) == T & is.na(weight11) == F,11,
                                    ifelse(is.na(exit_visit) == T & is.na(weight10) == F,10,
                                    ifelse(is.na(exit_visit) == T & is.na(weight9) == F,9,
                                    ifelse(is.na(exit_visit) == T & is.na(weight8) == F,8,
                                    ifelse(is.na(exit_visit) == T & is.na(weight7) == F,7,
                                    ifelse(is.na(exit_visit) == T & is.na(weight6) == F,6,
                                    ifelse(is.na(exit_visit) == T & is.na(weight5) == F,5,
                                    ifelse(is.na(exit_visit) == T & is.na(weight4) == F,4,
                                    ifelse(is.na(exit_visit) == T & is.na(weight3) == F,3,
                                    ifelse(is.na(exit_visit) == T & is.na(weight2) == F,2,
                                    ifelse(is.na(exit_visit) == T & is.na(weight1) == F,1,0))))))))))))))),
                     
                     weight.gain.unfinished = ifelse(current.week == 1, (weight1 - weight0)*1000 / as.numeric(dint1 - dint0) / weight0,
                                              ifelse(current.week == 2, (weight2 - weight0)*1000 / as.numeric(dint2 - dint0) / weight0, 
                                              ifelse(current.week == 3, (weight3 - weight0)*1000 / as.numeric(dint3 - dint0) / weight0,      
                                              ifelse(current.week == 4, (weight4 - weight0)*1000 / as.numeric(dint4 - dint0) / weight0,
                                              ifelse(current.week == 5, (weight5 - weight0)*1000 / as.numeric(dint5 - dint0) / weight0,      
                                              ifelse(current.week == 6, (weight6 - weight0)*1000 / as.numeric(dint6 - dint0) / weight0,
                                              ifelse(current.week == 7, (weight7 - weight0)*1000 / as.numeric(dint7 - dint0) / weight0, 
                                              ifelse(current.week == 8, (weight8 - weight0)*1000 / as.numeric(dint8 - dint0) / weight0,      
                                              ifelse(current.week == 9, (weight9 - weight0)*1000 / as.numeric(dint9 - dint0) / weight0,
                                              ifelse(current.week == 10, (weight10 - weight0)*1000 / as.numeric(dint10 - dint0) / weight0,      
                                              ifelse(current.week == 11, (weight11 - weight0)*1000 / as.numeric(dint11 - dint0) / weight0,      
                                              ifelse(current.week == 12, (weight12 - weight0)*1000 / as.numeric(dint12 - dint0) / weight0, 
                                              ifelse(current.week == 13, (weight13 - weight0)*1000 / as.numeric(dint13 - dint0) / weight0,      
                                              ifelse(current.week == 14, (weight14 - weight0)*1000 / as.numeric(dint14 - dint0) / weight0,
                                              ifelse(current.week == 15, (weight15 - weight0)*1000 / as.numeric(dint15 - dint0) / weight0,      
                                              ifelse(current.week == 16, (weight16 - weight0)*1000 / as.numeric(dint16 - dint0) / weight0,NA)))))))))))))))))        
                                                   
                  
# Grouping data by dosage

long.data <- group_by(long.data, dosage)
wide.data <- group_by(wide.data, dosage)

# --------------------------------------------------------------------------------------------------------------------------------- #
#                                     Making sure the initial samples are balanced                                                  #
# --------------------------------------------------------------------------------------------------------------------------------- #

  # Gender
    table(wide.data$dosage, wide.data$sex)    # Dose 1: 50.4% female, Dose 2: 55.5% female
    prop.table(table(wide.data$dosage, wide.data$sex,exclude=""),1)
    chisq.test(table(wide.data$dosage, wide.data$sex, exclude = "") ) 
    
  # Criteria of admission
    table(wide.data$dosage, wide.data$criteria_adm)  
    prop.table(table(wide.data$dosage, wide.data$criteria_adm, exclude=""),1)
    chisq.test(table(wide.data$dosage, wide.data$criteria_adm, exclude = "") ) 
    
  # Age
    wide.data %>%                               # Average age of those treated:
        group_by(dosage) %>%                      #   Dose 1: 13.8 years old
           summarise(mean.age=mean(age, na.rm=T),
                     sd.age=sd(age, na.rm=T))  #   Dose 2: 12.5 years old

    t.test(wide.data$age ~ wide.data$dosage)
    
  # MUAC at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.muac=mean(muac0, na.rm=T),
                sd.muac=sd(muac0, na.rm=T))             
       
    t.test(wide.data$muac0 ~ wide.data$dosage)
    
  # MUAC at entry (finished)
    wide.data[wide.data$finished == T,] %>%
      group_by(dosage) %>%
      summarise(mean.muac.entry=mean(muac0, na.rm=T),
                sd.muac.entry=sd(muac0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == T,]$muac0 ~ wide.data[wide.data$finished == T,]$dosage)
    
  # MUAC at entry (among those unfinished)
    wide.data[wide.data$finished == F,] %>%
      group_by(dosage) %>%
      summarise(mean.muac.entry=mean(muac0, na.rm=T),
                sd.muac.entry=sd(muac0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == F,]$muac0 ~ wide.data[wide.data$finished == F,]$dosage)
    
  # weight at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.entry=mean(weight0, na.rm=T),
                sd.weight.entry=sd(weight0, na.rm=T)) 
    
    t.test(wide.data$weight0 ~ wide.data$dosage)
    
  # weight for height z-score at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
      
    t.test(wide.data$whz0 ~ wide.data$dosage)
    
  # weight for height z-score at entry (finished)
    wide.data[wide.data$finished == T,] %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == T,]$whz0 ~ wide.data[wide.data$finished == T,]$dosage)
    
  # weight for height z-score at entry (among those unfinished)
    wide.data[wide.data$finished == F,] %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == F,]$whz0 ~ wide.data[wide.data$finished == F,]$dosage)
    
  # height for age z-score at entry
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.haz.entry=mean(haz0, na.rm=T),
                sd.haz.entry=sd(haz0, na.rm=T)) 
    
    t.test(wide.data$haz0 ~ wide.data$dosage)

# --------------------------------------------------------------------------------------------------------------------------------- #
#                                             Looking at differences in outcomes                                                    #
# --------------------------------------------------------------------------------------------------------------------------------- #
    
  # Length of stay
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T),
                sd.length.of.stay=sd(stay, na.rm=T))
                
    t.test(wide.data$stay ~ wide.data$dosage)
    
  # Rate of weight gain from admission to exit (g/kg/d) # (weight_exit - weight_adm)/stay/weight_adm
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d, na.rm=T),
                sd.weight.gain=sd(wgain_d, na.rm=T)) 
    
    t.test(wide.data$wgain_d ~ wide.data$dosage)
    
  # change in MUAC
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.muac.change=mean(muac.change, na.rm=T),
                sd.muac.change=sd(muac.change, na.rm=T))
      
    t.test(wide.data$muac.change ~ wide.data$dosage)
    
  # change in weight
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.change=mean(weight.change, na.rm=T),
                sd.weight.change=sd(weight.change, na.rm=T))
    
  # change in weight for height z-score
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.whz.change=mean(whz.change, na.rm=T),
                sd.whz.change=sd(whz.change, na.rm=T))
    
    t.test(wide.data$whz.change ~ wide.data$dosage)
    
  # change in height for age z-score
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.haz.change=mean(haz.change, na.rm=T),
                sd.haz.change=sd(haz.change, na.rm=T))
    
  # Rate of weight gain from admission to exit (g/kg/d) *Weeks 0-2
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.02, na.rm=T),
                sd.weight.gain=sd(weight.gain.02, na.rm=T))

  # Rate of weight gain from admission to exit (g/kg/d) *Weeks 3+
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.3, na.rm=T),
                sd.weight.gain=sd(weight.gain.3, na.rm=T))
                
  # Rate of weight gain (among those who finished)
    wide.data[wide.data$finished == T,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d, na.rm=T),
                sd.weight.gain=sd(wgain_d, na.rm=T))
    
  # Rate of weight gain (among those who did not finished)
    wide.data[wide.data$finished == F,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.unfinished, na.rm=T),
                sd.weight.gain=sd(weight.gain.unfinished, na.rm=T))
    
    t.test(wide.data$weight.gain.unfinished ~ wide.data$dosage)
    
    # Rate of weight gain (among those who finished, 0-2)
    wide.data[wide.data$finished == T,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.02, na.rm=T),
                sd.weight.gain=sd(weight.gain.02, na.rm=T))
    
    # Rate of weight gain (among those who did not finished, 0-2)
    wide.data[wide.data$finished == F,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.02, na.rm=T),
                sd.weight.gain=sd(weight.gain.02, na.rm=T))
    
    
  # Rate of weight gain (among those who finished)
    wide.data[wide.data$finished == T,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d, na.rm=T),
                sd.weight.gain=sd(wgain_d, na.rm=T))
    
  # Rate of weight gain (among those who did not finished)
    wide.data[wide.data$finished == F,] %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(weight.gain.unfinished, na.rm=T),
                sd.weight.gain=sd(weight.gain.unfinished, na.rm=T))
    
# Looking at reason for referral
    
    t <- cbind(table(wide.data$dosage, wide.data$referral, exclude = "")[1,],
               table(wide.data$dosage, wide.data$referral, exclude = "")[2,])
    
    t <- cbind(prop.table(table(wide.data$dosage, wide.data$referral, exclude = "")[1,]),
               prop.table(table(wide.data$dosage, wide.data$referral, exclude = "")[2,]))
    t2 <- t * 100
    
    t <- data.frame(cbind(prop.table(table(wide.data$dosage, wide.data$referral, exclude = "")[1,]),
                          prop.table(table(wide.data$dosage, wide.data$referral, exclude = "")[2,]),
                          table(wide.data$dosage, wide.data$referral, exclude = "")[1,],
                          table(wide.data$dosage, wide.data$referral, exclude = "")[2,]
                          ))
    
    t2 <- mutate(t, dose1 = paste0( X3, " (", round(X1 * 100,2), "%)"),
                    dose2 = paste0( X4, " (", round(X2 * 100,2), "%)"))
    
    t3 <- t2[,5:6]
    
    select(t2, X1)
    
    %>%
                    select( dose1, dose2)
                
           
    t2 <- t * 100
    
    
# 2x2 table of reason for exit
    
referral.table <- table(wide.data$dosage, wide.data$exit, exclude="")

referral.prop.table <- prop.table(referral.table, 1)

# 2x2 table of recovery by gender

referral.table2 <- table(wide.data$sex, wide.data$recovered.status, exclude="")

referral.prop.table2 <- prop.table(referral.table2, 1)


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

# --------------------------------------------------------------------------------------------------------------------------------- #
#                                                     Non-inferiority testing                                                       #
# --------------------------------------------------------------------------------------------------------------------------------- #

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

# --------------------------------------------------------------------------------------------------------------------------------- #
#                                                  Exploration of Recovery                                                          #
# --------------------------------------------------------------------------------------------------------------------------------- #

# A child will be considered recovered when he/she reaches the two following criteria:
#    1) WHZ ≥ -2 at two consecutive visits 
#    2) MUAC ≥ 125mm at two consecutive visits. 

pass <- data.frame(cbind(
wide.data$id,
pass1 <- ifelse(wide.data$whz1 >= -2 & wide.data$muac1 >= 125, 1, 0),
pass2 <- ifelse(wide.data$whz2 >= -2 & wide.data$muac2 >= 125, 1, 0),
pass3 <- ifelse(wide.data$whz3 >= -2 & wide.data$muac3 >= 125, 1, 0),
pass4 <- ifelse(wide.data$whz4 >= -2 & wide.data$muac4 >= 125, 1, 0),
pass5 <- ifelse(wide.data$whz5 >= -2 & wide.data$muac5 >= 125, 1, 0),
pass6 <- ifelse(wide.data$whz6 >= -2 & wide.data$muac6 >= 125, 1, 0),
pass7 <- ifelse(wide.data$whz7 >= -2 & wide.data$muac7 >= 125, 1, 0),
pass8 <- ifelse(wide.data$whz8 >= -2 & wide.data$muac8 >= 125, 1, 0),
pass9 <- ifelse(wide.data$whz9 >= -2 & wide.data$muac9 >= 125, 1, 0),
pass10 <- ifelse(wide.data$whz10 >= -2 & wide.data$muac10 >= 125, 1, 0),
pass11 <- ifelse(wide.data$whz11 >= -2 & wide.data$muac11 >= 125, 1, 0),
pass12 <- ifelse(wide.data$whz12 >= -2 & wide.data$muac12 >= 125, 1, 0),
pass13 <- ifelse(wide.data$whz13 >= -2 & wide.data$muac13 >= 125, 1, 0),
pass14 <- ifelse(wide.data$whz14 >= -2 & wide.data$muac14 >= 125, 1, 0),
pass15 <- ifelse(wide.data$whz15 >= -2 & wide.data$muac15 >= 125, 1, 0),
pass16 <- ifelse(wide.data$whz16 >= -2 & wide.data$muac16 >= 125, 1, 0),
as.character(wide.data$exit),
wide.data$finished,
wide.data$dosage
))

names(pass) <- c("id", "pass1", "pass2", "pass3", "pass4", "pass5", "pass6", "pass7", "pass8", "pass9",
                "pass10", "pass11", "pass12", "pass13", "pass14", "pass15", "pass16", "exit", "finished", "dosage")

pass <- pass %>%
  mutate(mw_pass = ifelse( (pass1 == 1 & pass2 == 1) | (pass2 == 1 & pass3 == 1) |
                            (pass3 == 1 & pass4 == 1) | (pass4 == 1 & pass5 == 1) | 
                            (pass5 == 1 & pass6 == 1) | (pass6 == 1 & pass7 == 1) | (pass7 == 1 & pass8 == 1) | (pass8 == 1 & pass9 == 1) |
                            (pass9 == 1 & pass10 == 1) | (pass11 == 1 & pass12 == 1) | 
                            (pass12 == 1 & pass13 == 1) | (pass13 == 1 & pass14 == 1) |
                            (pass14 == 1 & pass15 == 1) | (pass15 == 1 & pass16 == 1), 1, 0 ))

pass <- pass %>%
          mutate(flag = ifelse(   (mw_pass == 0 | is.na(mw_pass) == T) & exit == "Recovered", 1, 0))

# Dose by passed recovery criteria

table(pass$dosage, pass$mw_pass)  # Dose 1: 41 kids met criteria Dose 2: 38 kids met criteria








table(muac_pass2$m_pass)
table(muac_pass2$X17)

strange <- pass %>%
  filter( flag == 1)

strange$id

s <- wide.data[wide.data$id %in% strange$id,]

s2 <- s[s$criteria_adm == "adm_muac_whz",]
  
  
table(s$csps)
table(s$criteria_adm)





.finished <- pass[pass$X18 == 1,]

muac_pass <- data.frame(cbind(
  wide.data$id,
  muac_pass1 <- ifelse(wide.data$whz1 >= -2 & wide.data$muac1 >= 125, 1, 0),
  muac_pass2 <- ifelse(wide.data$whz2 >= -2 & wide.data$muac2 >= 125, 1, 0),
  muac_pass3 <- ifelse(wide.data$whz3 >= -2 & wide.data$muac3 >= 125, 1, 0),
  muac_pass4 <- ifelse(wide.data$whz4 >= -2 & wide.data$muac4 >= 125, 1, 0),
  muac_pass5 <- ifelse(wide.data$whz5 >= -2 & wide.data$muac5 >= 125, 1, 0),
  muac_pass6 <- ifelse(wide.data$whz6 >= -2 & wide.data$muac6 >= 125, 1, 0),
  muac_pass7 <- ifelse(wide.data$whz7 >= -2 & wide.data$muac7 >= 125, 1, 0),
  muac_pass8 <- ifelse(wide.data$whz8 >= -2 & wide.data$muac8 >= 125, 1, 0),
  muac_pass9 <- ifelse(wide.data$whz9 >= -2 & wide.data$muac9 >= 125, 1, 0),
  muac_pass10 <- ifelse(wide.data$whz10 >= -2 & wide.data$muac10 >= 125, 1, 0),
  muac_pass11 <- ifelse(wide.data$whz11 >= -2 & wide.data$muac11 >= 125, 1, 0),
  muac_pass12 <- ifelse(wide.data$whz12 >= -2 & wide.data$muac12 >= 125, 1, 0),
  muac_pass13 <- ifelse(wide.data$whz13 >= -2 & wide.data$muac13 >= 125, 1, 0),
  muac_pass14 <- ifelse(wide.data$whz14 >= -2 & wide.data$muac14 >= 125, 1, 0),
  muac_pass15 <- ifelse(wide.data$whz15 >= -2 & wide.data$muac15 >= 125, 1, 0),
  muac_pass16 <- ifelse(wide.data$whz16 >= -2 & wide.data$muac16 >= 125, 1, 0),
  as.character(wide.data$exit),
  wide.data$finished,
  wide.data$dosage
))

muac_pass2 <- muac_pass %>%
                 mutate(m_pass = ifelse( (X1 == 1 & X2 == 1) | (X2 == 1 & X3 == 1) |
                                         (X3 == 1 & X4 == 1) | (X4 == 1 & X5 == 1) | 
                                         (X5 == 1 & X6 == 1) | (X6 == 1 & X7 == 1) | (X7 == 1 & X8 == 1) | (X8 == 1 & X9 == 1) |
                                         (X9 == 1 & X10 == 1) | (X11 == 1 & X12 == 1) | 
                                         (X12 == 1 & X13 == 1) | (X13 == 1 & X14 == 1) |
                                         (X14 == 1 & X15 == 1) | (X15 == 1 & X16 == 1), 1, 0 ))

# Dose by passed recovery criteria

table(muac_pass2$X19, muac_pass2$m_pass)   
table(muac_pass2$m_pass)
table(muac_pass2$X17)
                                         
strange <- muac_pass2 %>%
            filter( (m_pass == 0 | is.na(m_pass) == T) & X17 == "Recovered")



[muac_pass2$m_pass != 1 & muac_pass2$X17 == "Recovered",]





whz_pass <- data.frame(cbind(
  whz_pass1 <- ifelse(wide.data$whz1 >= -2 & wide.data$muac1 >= 125, 1, 0),
  whz_pass2 <- ifelse(wide.data$whz2 >= -2 & wide.data$muac2 >= 125, 1, 0),
  whz_pass3 <- ifelse(wide.data$whz3 >= -2 & wide.data$muac3 >= 125, 1, 0),
  whz_pass4 <- ifelse(wide.data$whz4 >= -2 & wide.data$muac4 >= 125, 1, 0),
  whz_pass5 <- ifelse(wide.data$whz5 >= -2 & wide.data$muac5 >= 125, 1, 0),
  whz_pass6 <- ifelse(wide.data$whz6 >= -2 & wide.data$muac6 >= 125, 1, 0),
  whz_pass7 <- ifelse(wide.data$whz7 >= -2 & wide.data$muac7 >= 125, 1, 0),
  whz_pass8 <- ifelse(wide.data$whz8 >= -2 & wide.data$muac8 >= 125, 1, 0),
  whz_pass9 <- ifelse(wide.data$whz9 >= -2 & wide.data$muac9 >= 125, 1, 0),
  whz_pass10 <- ifelse(wide.data$whz10 >= -2 & wide.data$muac10 >= 125, 1, 0),
  whz_pass11 <- ifelse(wide.data$whz11 >= -2 & wide.data$muac11 >= 125, 1, 0),
  whz_pass12 <- ifelse(wide.data$whz12 >= -2 & wide.data$muac12 >= 125, 1, 0),
  whz_pass13 <- ifelse(wide.data$whz13 >= -2 & wide.data$muac13 >= 125, 1, 0),
  whz_pass14 <- ifelse(wide.data$whz14 >= -2 & wide.data$muac14 >= 125, 1, 0),
  whz_pass15 <- ifelse(wide.data$whz15 >= -2 & wide.data$muac15 >= 125, 1, 0),
  whz_pass16 <- ifelse(wide.data$whz16 >= -2 & wide.data$muac16 >= 125, 1, 0),
  as.character(wide.data$exit),
  wide.data$finished,
  wide.data$dosage
))                                        
                                        




  






# --------------------------------------------------------------------------------------------------------------------------------- #
#                                                     Binomial Odds Ratios                                                          #
# --------------------------------------------------------------------------------------------------------------------------------- #

OR <- function(predictor){
  
  x <- glm(recovered.status ~ predictor, family=binomial(link="logit"), data=wide.data)
  n = round(exp(cbind(coef(x), confint(x))), 2)[2,]
  paste0(n[1], " (", n[2], ",", n[3], ")" )   
  
}

predicter.labels <- c("Age", "Sex", "Weight for Height Z-score", "Height for Age Z-score",
                      "MUAC at admission", "Weight at admission", "Dosage")

predicter.data <- matrix(cbind(wide.data$age, wide.data$sex, wide.data$whz_adm, wide.data$haz_adm,
                               wide.data$muac_adm, wide.data$weight_adm, wide.data$dosage), nrow=240, ncol=7)

table6 <- cbind(predicter.labels, apply(predicter.data, 2, OR))

# Saving table

pdf( paste0(dir, "table6.pdf"), height=10, width=5) 

grid.table(table6)

dev.off()


# Referral

OR <- function(predictor){
  
  x <- glm(referred.status ~ predictor, family=binomial(link="logit"), data=wide.data)
  n = round(exp(cbind(coef(x), confint(x))), 2)[2,]
  paste0(n[1], " (", n[2], ",", n[3], ")" )   
  
}

predicter.labels <- c("Age", "Sex", "Weight for Height Z-score", "Height for Age Z-score",
                      "MUAC at admission", "Weight at admission", "Dosage")

predicter.data <- matrix(cbind(wide.data$age, wide.data$sex, wide.data$whz_adm, wide.data$haz_adm,
                               wide.data$muac_adm, wide.data$weight_adm, wide.data$dosage), nrow=240, ncol=7)

table7 <- cbind(predicter.labels, apply(predicter.data, 2, OR))

# Saving table

pdf( paste0(dir, "table7.pdf"), height=10, width=5) 

grid.table(table7)

dev.off()

# Default

OR <- function(predictor){
  
  x <- glm(default.status ~ predictor, family=binomial(link="logit"), data=wide.data)
  n = round(exp(cbind(coef(x), confint(x))), 2)[2,]
  paste0(n[1], " (", n[2], ",", n[3], ")" )   
  
}

predicter.labels <- c("Age", "Sex", "Weight for Height Z-score", "Height for Age Z-score",
                      "MUAC at admission", "Weight at admission", "Dosage")

predicter.data <- matrix(cbind(wide.data$age, wide.data$sex, wide.data$whz_adm, wide.data$haz_adm,
                               wide.data$muac_adm, wide.data$weight_adm, wide.data$dosage), nrow=240, ncol=7)

table8 <- cbind(predicter.labels, apply(predicter.data, 2, OR))

# Saving table

pdf( paste0(dir, "table8.pdf"), height=10, width=5) 

grid.table(table8)

dev.off()

# Treatment failure

OR <- function(predictor){
  
  x <- glm(treatment.failure ~ predictor, family=binomial(link="logit"), data=wide.data)
  n = round(exp(cbind(coef(x), confint(x))), 2)[2,]
  paste0(n[1], " (", n[2], ",", n[3], ")" )   
  
}

predicter.labels <- c("Age", "Sex", "Weight for Height Z-score", "Height for Age Z-score",
                      "MUAC at admission", "Weight at admission", "Dosage")

predicter.data <- matrix(cbind(wide.data$age, wide.data$sex, wide.data$whz_adm, wide.data$haz_adm,
                               wide.data$muac_adm, wide.data$weight_adm, wide.data$dosage), nrow=240, ncol=7)

table9 <- cbind(predicter.labels, apply(predicter.data, 2, OR))

# Saving table

pdf( paste0(dir, "table9.pdf"), height=10, width=5) 

grid.table(table9)

dev.off()


# --------------------------------------------------------------------------------------------------------------------------------- #
#                                                           Regression                                                              #
# --------------------------------------------------------------------------------------------------------------------------------- #

fit <- lm(wgain_d ~ sex + age + whz_adm + haz_adm + muac_adm + weight_adm + dosage,data=wide.data)
summary(fit)
AIC(fit)

fit <- lm(wgain_d ~ sex + age + whz_adm + haz_adm + muac_adm + weight_adm + dosage,data=wide.data)
summary(fit)
AIC(fit)

model <- glm(recovered.status ~ age + sex + whz_adm + haz_adm + muac_adm + weight_adm + dosage,
             family=binomial(link='logit'), data=wide.data)
summary(model)

model <- glm(recovered.status ~ whz_adm + haz_adm + muac_adm + weight_adm + csps,
             family=binomial(link='logit'), data=wide.data)
summary(model)



# Missing values

install.packages("Amelia")
library(Amelia)

png(paste0(dir, 'missmap.png'), 12, 8, units='in', res=300)

par(mfrow=c(1, 1), oma=c(5, 5, 5, 10))

missmap(long.data, main = "", x.cex=1.5, y.labels = c(rep("", 4080)), legend=T)

mtext("Observaton number", 2, 3, cex = 2)
#mtext("Variable", 1, 3, cex = 2)

mtext("Missing values vs. observed", 3, 3, cex = 2)
axis(2, at=c(0, 1000, 2000, 3000, 4000),  line=0, #labels=c("", "20%", "40%", "60%", "80%", "100%"),
     cex.axis=1.5, cex.lab=1.5)

dev.off()

legend(x=19, y=5000, 
       legend = c("Missing"), #in order from top to bottom
       fill = sequential[3:2], # 6:1 reorders so legend order matches graph
       title = "",
       cex = 1.5,
       bty="n")

legend(x=25, y=5000, 
       legend = c("Observed"), #in order from top to bottom
       fill = sequential[3:2], # 6:1 reorders so legend order matches graph
       title = "",
       cex = 1.5,
       bty="n")

# Missing values (wide data)

png(paste0(dir, 'missmap_wide.png'), 12, 8, units='in', res=300)

par(mfrow=c(1, 1), oma=c(5, 5, 5, 10))

missmap(wide.data[,c("age", "sex", "whz_adm", "haz_adm", "muac_adm", "weight_adm", "dosage")],
                  main = "", x.cex=1.5, y.labels = c(rep("", 240)), legend=T)

mtext("Observaton number", 2, 3, cex = 2)
#mtext("Variable", 1, 3, cex = 2)

mtext("Missing values vs. observed", 3, 3, cex = 2)
axis(2, at=c(0, 120, 240),  line=0, #labels=c("", "20%", "40%", "60%", "80%", "100%"),
     cex.axis=1.5, cex.lab=1.5)

dev.off()


# Forest



# scramble the order of the credit data to avoid bias

tree.dat <- wide.data[,c("age", "sex", "whz_adm", "haz_adm", "muac_adm", "weight_adm", "dosage",
                         "criteria_adm", "csps", "recovered.status")]

set.seed(2)
data_rand <- tree.dat[order(runif(240)), ]

# Create  a 90% training set and a 10% test set
train <- data_rand[1:192, ]
test <- data_rand[193:240, ]

# use table to confirm train and test match up  # May 9 note, this depends on seed (ok?)
prop.table( table( train$recovered.status))
prop.table( table( test$recovered.status))

# Train the decision tree model
model <- C5.0(data.frame(train[c(-10)]), as.factor(train$recovered.status))

#What does the tree look like?
model
summary(model)

# predict the class of the test data
pred <- predict(model, data.frame(test))

#Use Crosstable to check performance
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c(' actual default', 'predicted default'))



            

















