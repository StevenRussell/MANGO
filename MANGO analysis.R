
#-------------------------------------------------------------------------------------------------------------------------------------#
# In this code, we performing exploratory data analysis on the MANGO dataset. 
#
# Created by Steven Russell
# Last updated: Sep 1, 2017
#-------------------------------------------------------------------------------------------------------------------------------------#

# Loading packages

library(tidyverse)
library(readxl)
library(janitor)
#library(stringr)
library(TOSTER)
library(xtable)
library(gridExtra)

# Setting directory

dir <- "C:/MANGO/MANGO (Sep 1 2017)/"

# Importing data

wide.data <- read_excel(paste0(dir, "MANGO_wide_010917.xlsx"))
long.data <- read_excel(paste0(dir, "MANGO_long_010917.xlsx"))
codes     <- read_excel(paste0(dir, "MANGO codes DSMB.xlsx"))

# Merging in dosage (trial arm) data

long.data <- merge(x=long.data, y=codes, by="id")
wide.data <- merge(x=wide.data, y=codes, by="id")

# Function to convert variables to date format

Date_Convert <- function(var){
  as.Date(var, format="%m/%d/%Y")
}

# Selecting all date variables

date.vars <- c( "date_adm", "date_exit", str_subset(names(wide.data), "dint") )

# Converting all dates variables to date format

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
                                              ifelse(current.week == 16, (weight16 - weight0)*1000 / as.numeric(dint16 - dint0) / weight0,NA)))))))))))))))),
                     
                     stay.unfinished = ifelse(current.week == 1, (dint1 - dint0),
                                       ifelse(current.week == 2, (dint2 - dint0),
                                       ifelse(current.week == 3, (dint3 - dint0),
                                       ifelse(current.week == 4, (dint4 - dint0),
                                       ifelse(current.week == 5, (dint5 - dint0),
                                       ifelse(current.week == 6, (dint6 - dint0),
                                       ifelse(current.week == 7, (dint7 - dint0),
                                       ifelse(current.week == 8, (dint8 - dint0),
                                       ifelse(current.week == 9, (dint9 - dint0),
                                       ifelse(current.week == 10, (dint10 - dint0),
                                       ifelse(current.week == 11, (dint11 - dint0),
                                       ifelse(current.week == 12, (dint12 - dint0),
                                       ifelse(current.week == 13, (dint13 - dint0),
                                       ifelse(current.week == 14, (dint14 - dint0),
                                       ifelse(current.week == 15, (dint15 - dint0),
                                       ifelse(current.week == 16, (dint16 - dint0),NA))))))))))))))))
                     
                     )        
                                                   
                  
# Grouping data by dosage

long.data <- group_by(long.data, dosage)
wide.data <- group_by(wide.data, dosage)

# --------------------------------------------------------------------------------------------------------------------------------- #
#                                     Making sure the initial samples are balanced                                                  #
# --------------------------------------------------------------------------------------------------------------------------------- #

  # Gender

    wide.data %>%
      crosstab(dosage, sex) %>%
      adorn_crosstab("row", show_n = T, digits = 1)
  
    chisq.test(table(wide.data$dosage, wide.data$sex, exclude = ""))

  # Criteria of admission
    
    wide.data %>%
      crosstab(dosage, criteria_adm) %>%
      adorn_crosstab("row", show_n = T, digits = 1)
      
    chisq.test(table(wide.data$dosage, wide.data$criteria_adm, exclude = ""))
    
  # Age
    
    wide.data %>%                              
        group_by(dosage) %>%                      
        summarise(mean.age=mean(age, na.rm=T),
                  sd.age=sd(age, na.rm=T)) 

    t.test(wide.data$age ~ wide.data$dosage)
    
  # MUAC at entry
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.muac=mean(muac0, na.rm=T),
                sd.muac=sd(muac0, na.rm=T))             
    
    t.test(wide.data$muac0 ~ wide.data$dosage)
    
  # weight at entry
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.entry=mean(weight0, na.rm=T),
                sd.weight.entry=sd(weight0, na.rm=T)) 
    
    t.test(wide.data$weight0 ~ wide.data$dosage)
    
  # weight for height z-score at entry
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(count=n(),
                mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
      
    t.test(wide.data$whz0 ~ wide.data$dosage)
    
  # height for age z-score at entry
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.haz.entry=mean(haz0, na.rm=T),
                sd.haz.entry=sd(haz0, na.rm=T)) 
    
    t.test(wide.data$haz0 ~ wide.data$dosage)
    
  # date of admission
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.date.entry=mean(date_adm, na.rm=T),
                sd.date.entry=sd(date_adm, na.rm=T)) 
    
    t.test(wide.data$date_adm ~ wide.data$dosage)
    
# --------------------------------------------------------------------------------------------------------------------------------- #
#                              Looking at baseline values (stratified by finished/unfinished)                                       #
# --------------------------------------------------------------------------------------------------------------------------------- #
    
    # MUAC at entry (finished)
    
    wide.data %>%
      filter(finished == T) %>%
      group_by(dosage) %>%
      summarise(mean.muac.entry=mean(muac0, na.rm=T),
                sd.muac.entry=sd(muac0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == T,]$muac0 ~ wide.data[wide.data$finished == T,]$dosage)
    
    # MUAC at entry (among those unfinished)
    
    wide.data %>%
      filter(finished == F) %>%
      group_by(dosage) %>%
      summarise(mean.muac.entry=mean(muac0, na.rm=T),
                sd.muac.entry=sd(muac0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == F,]$muac0 ~ wide.data[wide.data$finished == F,]$dosage)
    
    # weight for height z-score at entry (finished)
    
    wide.data %>%
      filter(finished == T) %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == T,]$whz0 ~ wide.data[wide.data$finished == T,]$dosage)
    
    # weight for height z-score at entry (among those unfinished)
    
    wide.data %>%
      filter(finished == F) %>%
      group_by(dosage) %>%
      summarise(mean.whz.entry=mean(whz0, na.rm=T),
                sd.whz.entry=sd(whz0, na.rm=T))
    
    t.test(wide.data[wide.data$finished == F,]$whz0 ~ wide.data[wide.data$finished == F,]$dosage)

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
                
  # Percent who are already finished
    
    wide.data %>%
      group_by(dosage) %>%
      crosstab(finished, dosage) %>%
      adorn_crosstab("col", show_n = T, digits = 1)
    
  # Recovery by dosage
    
    wide.data %>%
      group_by(dosage) %>%
      crosstab(recovered.status, dosage) %>%
      adorn_crosstab("col", show_n = T, digits = 1)
    
    # --------------------------------------------------------------------------------------------------------------------------------- #
    #                                     Looking at differences in outcomes (stratified)                                                #
    # --------------------------------------------------------------------------------------------------------------------------------- #
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d_start, na.rm=T),
                sd.weight.gain=sd(wgain_d_start, na.rm=T))
    
    t.test(wide.data$wgain_d_start  ~ wide.data$dosage)
    
    wide.data %>%
      group_by(dosage) %>%
      summarise(mean.weight.gain=mean(wgain_d_after , na.rm=T),
                sd.weight.gain=sd(wgain_d_after, na.rm=T))
    
    t.test(wide.data$wgain_d_after ~ wide.data$dosage)
    
    
    
    # Length of stay (among those who recovered)
    
    wide.data %>%
      filter(exit == "Recovered") %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T),
                sd.length.of.stay=sd(stay, na.rm=T))
    
    t.test(wide.data[wide.data$exit == "Recovered",]$stay ~ wide.data[wide.data$exit == "Recovered",]$dosage)
    
    # Length of stay (among those who were referred)
    
    wide.data %>%
      filter(exit == "Referred") %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T),
                sd.length.of.stay=sd(stay, na.rm=T))
    
    t.test(wide.data[wide.data$exit == "Referred",]$stay ~ wide.data[wide.data$exit == "Referred",]$dosage)
    
    # Length of stay (among those who defaulted)
    
    wide.data %>%
      filter(exit == "Defaulted") %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T),
                sd.length.of.stay=sd(stay, na.rm=T))
    
    t.test(wide.data[wide.data$exit == "Defaulted",]$stay ~ wide.data[wide.data$exit == "Defaulted",]$dosage)
    
    # Length of stay (among those who are finished)
    
    wide.data %>%
      filter(finished == 1) %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay, na.rm=T),
                sd.length.of.stay=sd(stay, na.rm=T))
    
    t.test(wide.data[wide.data$finished == 1,]$stay ~ wide.data[wide.data$finished == 1,]$dosage)
    
    # Length of stay (among those who are unfinished)
    
    wide.data %>%
      filter(finished == 0) %>%
      group_by(dosage) %>%
      summarise(mean.length.of.stay=mean(stay.unfinished, na.rm=T),
                sd.length.of.stay=sd(stay.unfinished, na.rm=T))
    
    t.test(wide.data[wide.data$finished == 0,]$stay.unfinished ~ wide.data[wide.data$finished == 0,]$dosage)
    
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
    
    
    
    # --------------------------------------------------------------------------------------------------------------------------------- #
    #                                                 Binomial Odds Ratios (finishing as outcome)                                                        #
    # --------------------------------------------------------------------------------------------------------------------------------- #
    
    # Creating function to calculate odds ratio
    
    OddsRatio <- function(outcome, predictor){
      
      x <- glm(outcome ~ predictor, family=binomial(link="logit"), data=wide.data)
      n <- round(exp(cbind(coef(x), confint(x))), 2)[2,]
      paste0(n[1], " (", n[2], ",", n[3], ")" )   
      
    }
    
    # List of predictor variables to apply the function to
    
    predictor.list <- list(wide.data$age, wide.data$sex, wide.data$whz_adm, wide.data$haz_adm,
                           wide.data$muac_adm, wide.data$weight_adm, wide.data$dosage)
    
    # List of outcome variables to apply the function to
    
    outcome.list <- rep(list(wide.data$finished), 7)
    
    # Applying the function over the two lists of function arguments
    
    odds.ratio.vector <- mapply(OddsRatio, outcome = outcome.list, predictor.list)
    
    # Adding labels for predictor variables
    
    predictor.labels <- c("Age", "Sex", "Weight for Height Z-score", "Height for Age Z-score",
                          "MUAC at admission", "Weight at admission", "Dosage")
    
    # Creating final table
    
    cbind(predictor.labels, odds.ratio.vector)
    
    
    # --------------------------------------------------------------------------------------------------------------------------------- #
    #                                                 Binomial Odds Ratios (other outcomes)                                                         #
    # --------------------------------------------------------------------------------------------------------------------------------- #
    
    recovered.list <- rep(list(wide.data$recovered.status), 7)
    referred.list <- rep(list(wide.data$referred.status), 7)
    default.list <- rep(list(wide.data$default.status), 7)
    failure.list <- rep(list(wide.data$treatment.failure), 7) 
    
    recovered.or.vector <- mapply(OddsRatio, outcome = recovered.list, predictor.list)
    referred.or.vector <- mapply(OddsRatio, outcome = referred.list, predictor.list)
    default.or.vector <- mapply(OddsRatio, outcome = default.list, predictor.list)
    failure.or.vector <- mapply(OddsRatio, outcome = failure.list, predictor.list)
      
    # Creating final tables
    
    # Recovered
    cbind(predictor.labels, recovered.or.vector)
    
    # Referred
    cbind(predictor.labels, referred.or.vector)
    
    # Defaulted
    cbind(predictor.labels, default.or.vector)
    
    # Treatment failure
    cbind(predictor.labels,  failure.or.vector)
    
    # --------------------------------------------------------------------------------------------------------------------------------- #
    #                                             Reason for referral and reason for exit                                               #
    # --------------------------------------------------------------------------------------------------------------------------------- #
    
  # Referral
    
    # Replace 'replacment characters' from string
    
    wide.data$referral <- gsub("[^[:alnum:]///' ]", "", wide.data$referral)
    
    wide.data$referral <- recode(wide.data$referral, 
                "Fever  39C or hypothermia  35C" = "Other",
                "Severe anemia Hb  4g/dl" = "Other"
                )
    
    
    # Create crosstabulation
    
    wide.data %>%
      filter(referred.status == 1) %>%
      crosstab(referral, dosage) %>%
      adorn_crosstab("col", show_n = T, digits = 1)
    
  # Exit
    
    wide.data %>%
      filter(exit_visit > 0) %>%
      crosstab(exit, dosage) %>%
      adorn_crosstab("col", show_n = T, digits = 1)
    
    
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






