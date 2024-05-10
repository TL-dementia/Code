####################################################################################################
## Project: = Improving accuracy in the estimation of probable dementia in racially and ethnically diverse groups with penalized regression and transfer learning
## Code started: July, 2023
## Last update: February, 2024
####################################################################################################
# clear work space
rm(list=ls())
## set seed
set.seed(75007)
####################################################################################################
## load packages 
####################################################################################################
x <- c("haven", "plm", "tidyverse", "ggplot2",  "dplyr",  "dplyr", "missForest", "glmnet", "glmtrans", "zoo")
install.packages(x) 
lapply(x, library, character.only = TRUE)

#Please set your own directory.
# Set working directory #For example, I created a folder "TL_dementia".
setwd("~/Documents/TL_dementia/")

####################################################################################################
## Download the RAND HRS data & clean the variables & create final sample
####################################################################################################
RAND_HRS <- read_dta("RAND_HRS.dta")
#select sample before 2016
RAND_HRS <- RAND_HRS[RAND_HRS$wave <= 13,]
#only selecting the desired variables by removing other variables with r+number form.
pattern = "([[:alpha:]])([0-9]+)([[:alpha:]])"
RAND_HRS <- RAND_HRS[, -grep(pattern = pattern, colnames(RAND_HRS))]
####################################################################################################
###########Create data set
####################################################################################################
# Select sample with responses.
RAND_HRS <- RAND_HRS[RAND_HRS$inw ==1,]

#create ID.
RAND_HRS$HHIDPN <- as.numeric(RAND_HRS$hhid)*1000 + as.numeric(RAND_HRS$pn)

#sex/gender
RAND_HRS$female <- ifelse(RAND_HRS$ragender == 2, 1, 0)

#race/ethnicity

RAND_HRS$NH_white <- ifelse(RAND_HRS$raracem == 1 & RAND_HRS$rahispan == 0, 1, 0)
RAND_HRS$NH_black <- ifelse(RAND_HRS$raracem == 2 & RAND_HRS$rahispan == 0, 1, 0)
RAND_HRS$NH_other <- ifelse(RAND_HRS$raracem == 3 & RAND_HRS$rahispan == 0, 1, 0)
RAND_HRS$Hispanic <- ifelse(RAND_HRS$rahispan == 1, 1, 0)

#age category

RAND_HRS$hagecat70 <- ifelse(RAND_HRS$ragey_e < 75, 1, 0)
RAND_HRS$hagecat75 <- ifelse(RAND_HRS$ragey_e >= 75 &  RAND_HRS$ragey_e < 80, 1, 0)
RAND_HRS$hagecat80 <- ifelse(RAND_HRS$ragey_e >= 80 &  RAND_HRS$ragey_e < 85, 1, 0)
RAND_HRS$hagecat85 <- ifelse(RAND_HRS$ragey_e >= 85 &  RAND_HRS$ragey_e < 90, 1, 0)
RAND_HRS$hagecat90 <- ifelse(RAND_HRS$ragey_e >= 90, 1, 0)

#education
RAND_HRS$LTHS <- ifelse(RAND_HRS$raedegrm == 0, 1, 0)
RAND_HRS$HSGED <- ifelse(RAND_HRS$raedegrm > 0  & RAND_HRS$raedegrm <= 3, 1, 0)
RAND_HRS$GTHS <- ifelse(RAND_HRS$raedegrm >=4 & RAND_HRS$raedegrm <= 8, 1, 0)

####################################################################################################
#Cognitive Test - self respondents
####################################################################################################

#ADLs, IADLs
RAND_HRS$adl <- RAND_HRS$radla
RAND_HRS$iadl <- RAND_HRS$riadlza

#TICS variable
RAND_HRS$proxy <- ifelse(RAND_HRS$rproxy == 1, 1, 0)
RAND_HRS$iword <- RAND_HRS$rimrc 
RAND_HRS$dword <- RAND_HRS$rdlrc
RAND_HRS$ser7 <- RAND_HRS$rser7
RAND_HRS$bwc <- RAND_HRS$rbwc20
RAND_HRS$bwc1 <- ifelse(RAND_HRS$bwc == 2, 1, 0)
RAND_HRS$date_wrong <- ifelse(RAND_HRS$rdy == 0 & RAND_HRS$rmo == 0 & RAND_HRS$ryr == 0 & RAND_HRS$rdw == 0, 1, 0)
RAND_HRS$name_wrong <- ifelse(RAND_HRS$rcact == 0 & RAND_HRS$rpres == 0 & RAND_HRS$rvp == 0 & RAND_HRS$rscis == 0, 1, 0)
RAND_HRS$date_recall <- RAND_HRS$rdy + RAND_HRS$rmo + RAND_HRS$ryr + RAND_HRS$rdw 
RAND_HRS$name_recall <- RAND_HRS$rcact + RAND_HRS$rpres + RAND_HRS$rvp + RAND_HRS$rscis


####################################################################################################
#Cognitive Test - proxy respondents using HRS Core 2016/ 2014 data
####################################################################################################

####################################################################################################
## IQCODE 2014 
####################################################################################################

library(haven)
core2014 <- read_dta("HCAP code/core2014.dta")

## subset
#For proxy interview 2014

core_subset <- subset(core2014, 
                      select=c('HHID',
                               'PN',#sample id
                               "OD501", #0 How would you rate  respondent's memory at the present time? 1. excellent 2. very good 3. good 4. fair 5. poor 8. DK 9. RF
                               "OD506", #1 Remembering things about family and friends, such as occupations, birtODays, and addresses 1. improved 2. not chcanged 3. gotten worse 8. DK 9. RF
                               "OD509", #2 Remembering things that have happened recently?
                               "OD512", #3 Recalling conversations a few days later?
                               "OD515", #4 Remembering (her/your) address and telephone number?
                               "OD518", #5 Remembering what day and month it is?
                               "OD521", #6 Remembering where things are usually kept?
                               "OD524", #7 Remembering where to find things which have been put in a different place than usual?
                               "OD527", #8 Knowing how to work familiar machines around the house?
                               "OD530", #9 Learning to use a new gadget or machine around the house?
                               "OD533", #10 Learning new things in general?
                               "OD536", #11 Following a story in a book or on TV?
                               "OD539", #12 Making decisions on everyday matters?
                               "OD542", #13 Handling money for shopping?
                               "OD545", #14 Handling financial matters, that is, (her/your) pension or dealing with the bank?
                               "OD548", #15 Handling other everyday arithmetic problems, such as, knowing how much food to buy, knowing how long between visits from family or friends?
                               "OD551", #16 Using (her/your) intelligence to understand what's going on and to reason things through?
                               "OD507", "OD510", "OD513", "OD516", "OD519", "OD522", "OD525", "OD528", "OD531","OD534","OD537", "OD540","OD543","OD546","OD549","OD552", #1. much improved 2. a bit improved 8. DK 9. RF
                               "OD508", "OD511", "OD514", "OD517", "OD520", "OD523", "OD526" ,"OD529", "OD532","OD535","OD538", "OD541","OD544","OD547", "OD550","OD553"#4. a bit worse 5. much worse 8. DK 9. RF
                      )) 

core_subject1 <- core_subset
start = which(colnames(core_subject1) == "OD506")
end = which(colnames(core_subject1) == "OD551")
improve = which( colnames(core_subject1) == "OD507")
worse = which( colnames(core_subject1) == "OD508")
interval1 = improve - start
interval2 = worse - start

#IQCODE

for (i in start:end) {
  core_subject1[,i] <- case_when(core_subject1[,i] == 1 & core_subject1[,i+interval1] == 1 ~ 1, #much improved
                                 core_subject1[,i] == 1 & core_subject1[,i+interval1] != 1 & is.na(core_subject1[,i+interval1]) ~ 2, #a bit improved
                                 core_subject1[,i] == 2 ~ 3 , #same
                                 core_subject1[,i] == 3 &  core_subject1[,i+interval2] == 5 ~ 5, #much worse
                                 core_subject1[,i] == 3 &  core_subject1[,i+interval2] != 5 & !is.na(core_subject1[,i+interval2]) ~ 4)#a bit worse
} 

core_subject1[,start:end] <- sapply(core_subject1[,start:end], as.numeric)
core_subject1$IQCODE <- rowMeans(core_subject1[,start:end], na.rm = TRUE)
table(core_subject1$IQCODE)
core_subject1$wave <- 12


sel <- c("HHID","PN", "wave", "IQCODE")
core_2014_IQCODE <- core_subject1[, sel]

core2014 <- NULL
core_subset <- NULL
core_subject1 <- NULL

####################################################################################################
## IQCODE 2016
####################################################################################################
library(haven)
core2016 <- read_dta("HCAP code/core2016.dta")

core_subset <- subset(core2016, 
                      select=c('HHID',
                               'PN',#sample id
                               "PD501", #0 How would you rate  respondent's memory at the present time? 1. excellent 2. very goPD 3. goPD 4. fair 5. poor 8. DK 9. RF
                               "PD506", #1 Remembering things about family and friends, such as occupations, birtPDays, and addresses 1. improved 2. not chcanged 3. gotten worse 8. DK 9. RF
                               "PD509", #2 Remembering things that have happened recently?
                               "PD512", #3 Recalling conversations a few days later?
                               "PD515", #4 Remembering (her/your) address and telephone number?
                               "PD518", #5 Remembering what day and month it is?
                               "PD521", #6 Remembering where things are usually kept?
                               "PD524", #7 Remembering where to find things which have been put in a different place than usual?
                               "PD527", #8 Knowing how to work familiar machines around the house?
                               "PD530", #9 Learning to use a new gadget or machine around the house?
                               "PD533", #10 Learning new things in general?
                               "PD536", #11 Following a story in a book or on TV?
                               "PD539", #12 Making decisions on everyday matters?
                               "PD542", #13 Handling money for shopping?
                               "PD545", #14 Handling financial matters, that is, (her/your) pension or dealing with the bank?
                               "PD548", #15 Handling other everyday arithmetic problems, such as, knowing how much foPD to buy, knowing how long between visits from family or friends?
                               "PD551", #16 Using (her/your) intelligence to understand what's going on and to reason things through?
                               "PD507", "PD510", "PD513", "PD516", "PD519", "PD522", "PD525", "PD528", "PD531","PD534","PD537", "PD540","PD543","PD546","PD549","PD552", #1. much improved 2. a bit improved 8. DK 9. RF
                               "PD508", "PD511", "PD514", "PD517", "PD520", "PD523", "PD526" ,"PD529", "PD532","PD535","PD538", "PD541","PD544","PD547", "PD550","PD553"#4. a bit worse 5. much worse 8. DK 9. RF
                      )) 

core_subject1 <- core_subset
start = which(colnames(core_subject1) == "PD506")
end = which(colnames(core_subject1) == "PD551")
improve = which( colnames(core_subject1) == "PD507")
worse = which( colnames(core_subject1) == "PD508")
interval1 = improve - start
interval2 = worse - start

#IQCODE

for (i in start:end) {
  core_subject1[,i] <- case_when(core_subject1[,i] == 1 & core_subject1[,i+interval1] == 1 ~ 1, #much improved
                                 core_subject1[,i] == 1 & core_subject1[,i+interval1] != 1 & is.na(core_subject1[,i+interval1]) ~ 2, #a bit improved
                                 core_subject1[,i] == 2 ~ 3 , #same
                                 core_subject1[,i] == 3 &  core_subject1[,i+interval2] == 5 ~ 5, #much worse
                                 core_subject1[,i] == 3 &  core_subject1[,i+interval2] != 5 & !is.na(core_subject1[,i+interval2]) ~ 4)#a bit worse
} 

core_subject1[,start:end] <- sapply(core_subject1[,start:end], as.numeric)
core_subject1$IQCODE <- rowMeans(core_subject1[,start:end], na.rm = TRUE)
table(core_subject1$IQCODE)
core_subject1$wave <- 13


sel <- c("HHID", "PN", "wave", "IQCODE")
core_2016_IQCODE <- core_subject1[, sel]


core_subject1 <- NULL
core_subset <- NULL
core2016 <- NULL
core2014 <- NULL

####################################################################################################
# Merge data with Core_2014 and 2016
####################################################################################################

colnames(RAND_HRS)[which(names(RAND_HRS) == "hhid")] <- "HHID"
colnames(RAND_HRS)[which(names(RAND_HRS) == "pn")] <- "PN"


core_2016 <- left_join(RAND_HRS, core_2016_IQCODE)
core_2014 <- left_join(RAND_HRS, core_2014_IQCODE)

RAND_HRS$IQCODE <- case_when(RAND_HRS$wave == 12  ~ core_2014$IQCODE,
                             RAND_HRS$wave == 13  ~ core_2016$IQCODE)

####################################################################################################
#Create change values for self respondents. (adl and iadl apply for the proxy respondents as well.)
####################################################################################################
sel <- c("adl", "iadl", "date_recall", "bwc1", "ser7", "rscis", "rcact", "rpres", "iword", "dword")
#Current - last observed variable.
RAND_HRS <- RAND_HRS %>% 
  group_by(HHIDPN) %>%
  arrange(wave) %>%
  mutate(across(sel,  ~ .x - lag(na.locf(.x,na.rm = FALSE),1), .names = "{col}ch"))

####################################################################################################
#Create change values for proxy respondents. 
####################################################################################################

RAND_HRS <- RAND_HRS %>% 
  group_by(HHID, PN) %>%
  arrange(wave) %>%
  mutate(IQCODE_ch = IQCODE - lag(na.locf(IQCODE, na.rm = FALSE),1)) 

####################################################################################################
#Create past values (later be used for proxy respondents whose response status changed from self to proxy.)
####################################################################################################
sel <- c("proxy","date_recall", "ser7","rpres", "iword", "dword")

RAND_HRS <- RAND_HRS %>% 
  group_by(HHID, PN) %>%
  arrange(wave) %>%
  mutate(across(sel, ~ lag(na.locf(.x, na.rm = FALSE),1), .names = "{col}_lag")) 


####################################################################################################
#Merge the data with previous dementia algorithms
####################################################################################################

GP <- read_sas("hrsdementia_2021_1109/hrsdementia_2021_1109.sas7bdat")
GP$HRS_year <- as.numeric(GP$HRS_year)
GP$wave <- (GP$HRS_year - 1990)/2
GP$HRS_year <- NULL
GP$HHIDPN <- as.numeric(GP$HHID)*1000 + as.numeric(GP$PN)

Latent <- read_dta("PredictedCognitionDementiaMeasures/Dementia_HRS_2000-2016_Basic_Release1_2m.dta")
Latent$HHIDPN <- as.numeric(Latent$hhidpn)

Latent <- subset(Latent,select = c("HHIDPN", "PrDem", "wave"))
GP <- subset(GP,select = c("HHIDPN", "wave", "expert_p", "hurd_p", "lasso_p"))

RAND_HRS <- left_join(RAND_HRS, GP)
RAND_HRS <- left_join(RAND_HRS, Latent)

RAND_HRS <- RAND_HRS[RAND_HRS$wave == 13,]

####################################################################################################
# According to response status (proxy vs. self), 
# if there is a change from self to proxy, we provide the value when they were self-respondent.
# if there is no change (proxy to proxy), we set the value to be 0.
####################################################################################################

RAND_HRS$date_recall_lag <- case_when(RAND_HRS$proxy_lag == 0~ RAND_HRS$date_recall_lag,
                                      RAND_HRS$proxy_lag == 1 ~ 0)

RAND_HRS$ser7_lag <- case_when(RAND_HRS$proxy_lag == 0 ~ RAND_HRS$ser7_lag,
                               RAND_HRS$proxy_lag == 1 ~ 0)

RAND_HRS$rpres_lag <- case_when(RAND_HRS$proxy_lag == 0 ~ RAND_HRS$rpres_lag,
                                RAND_HRS$proxy_lag == 1~ 0)

RAND_HRS$iword_lag <- case_when(RAND_HRS$proxy_lag == 0 ~ RAND_HRS$iword_lag,
                                RAND_HRS$proxy_lag == 1 ~ 0)

RAND_HRS$dword_lag <- case_when(RAND_HRS$proxy_lag == 0 ~ RAND_HRS$dword_lag,
                                RAND_HRS$proxy_lag == 1 ~ 0)
#Change in IQCODE
RAND_HRS$IQCODE_ch <- case_when(RAND_HRS$proxy_lag == 0 ~ 0,
                                RAND_HRS$proxy_lag == 1 ~ RAND_HRS$IQCODE_ch) 
#exclude sample without weight.
RAND_HRS <- RAND_HRS[!is.na(RAND_HRS$rwtcrnh),]

####################################################################################################
## Merge with HCAP diagnosis
####################################################################################################

HCAP_diagnosis <- read_dta("HCAP_diagnosis.dta")
HCAP_diagnosis$HHIDPN <- as.numeric(HCAP_diagnosis$hhid)*1000 + as.numeric(HCAP_diagnosis$pn)

RAND_HCAP <- left_join(HCAP_diagnosis, RAND_HRS, by = "HHIDPN") #3496

RAND_HCAP <- RAND_HCAP[RAND_HCAP$ragey_e >= 70,] #2616 (3496-2616)/3496

RAND_HCAP  <- RAND_HCAP[is.na(RAND_HCAP$Hispanic)|
                          is.na(RAND_HCAP$NH_white)|
                          is.na(RAND_HCAP$NH_black)|
                          is.na(RAND_HCAP$NH_other)|
                          RAND_HCAP$NH_other != 1,] #2557 (2616-2557)/2616

RAND_HCAP<- RAND_HCAP[!is.na(RAND_HCAP$lasso_p) & !is.na(RAND_HCAP$PrDem) & !is.na(RAND_HCAP$expert_p) & !is.na(RAND_HCAP$hurd_p) ,] #2388 (2557-2388)/2557

####################################################################################################
## Apply same selection criteria to RAND HRS data.
####################################################################################################
#To harmonize the age range with existing algorithms.
RAND_HRS <- RAND_HRS[RAND_HRS$ragey_e >= 65,] #9994

RAND_HRS <- RAND_HRS[RAND_HRS$ragey_e>= 70,] #(9994-7357)/9994

RAND_HRS  <- RAND_HRS[is.na(RAND_HRS$Hispanic)|
                        is.na(RAND_HRS$NH_white)|
                        is.na(RAND_HRS$NH_black)|
                        is.na(RAND_HRS$NH_other)|
                        RAND_HRS$NH_other != 1,]  #(7357-7187)/7357

RAND_HRS <- RAND_HRS[!is.na(RAND_HRS$lasso_p) & !is.na(RAND_HRS$PrDem) & !is.na(RAND_HRS$expert_p) & !is.na(RAND_HRS$hurd_p) ,] # (7187-6630)/7187


####################################################################################################
## Imputations.
####################################################################################################
####################################################################################################
## Categorical values to factors
####################################################################################################

# Transform to categorical or binary for imputation, except 
cols <- c("HHIDPN","HCAP16WGTR",
          "rwtcrnh","ragey_e",
          "expert_p","hurd_p","lasso_p","PrDem", "R1HCAPDX")

RAND_HCAP_1 <- RAND_HCAP[,names(RAND_HCAP) %in% cols]
RAND_HCAP_2 <- as.data.frame(lapply(RAND_HCAP[,!names(RAND_HCAP) %in% cols], as.factor))
RAND_HCAP <- cbind(RAND_HCAP_1, RAND_HCAP_2)

####################################################################################################
## Imputation for the self-respondents HCAP
####################################################################################################

RAND_HCAP_self <- RAND_HCAP[RAND_HCAP$proxy == 0,]
RAND_HCAP_self <- subset(RAND_HCAP_self, select = c( "HHIDPN", "HCAP16WGTR", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                     "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
                                                     "adl", "iadl", "proxy", "LTHS", "HSGED", "GTHS","raedyrs",
                                                     "adlch", "iadlch", "date_recallch", "bwc1ch", "ser7ch",
                                                     "rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
                                                     "female",
                                                     "date_recall", "name_recall","ser7","iword","dword","bwc1", "rpres", "rscis", "rcact",
                                                     #"IQCODE_ch","proxy_lag","date_recall_lag", "ser7_lag","rpres_lag", "iword_lag", "dword_lag",
                                                     "expert_p", "lasso_p", "hurd_p", "PrDem","R1HCAPDX") )

self_impute <- RAND_HCAP_self[, !names(RAND_HCAP_self) %in% c( "rwtcrnh", "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                               "lasso_p", "hurd_p", "expert_p","PrDem", "R1HCAPDX")]
self_remain <- RAND_HCAP_self[, names(RAND_HCAP_self) %in% c("rwtcrnh",  "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                             "lasso_p", "hurd_p", "expert_p","PrDem", "R1HCAPDX")]
#check missingness
summary(self_impute)

#missForest
imputation_self <- missForest(as.data.frame(self_impute),verbose=TRUE,maxiter=5,ntree= 500)
imputed_self <- imputation_self$ximp
imputed_self <- data.frame(imputed_self)
imputed_self$HHIDPN <- RAND_HCAP_self$HHIDPN
imputed_self$wave <- RAND_HCAP_self$wave
## add outcome back into data
imputed_self_data <- left_join(imputed_self, self_remain)
write.csv(imputed_self_data, "imputed_HCAP_self.csv", row.names=TRUE)

####################################################################################################
## Categorical values to factors
####################################################################################################

# Transform to categorical or binary for imputation, except 
cols <- c("HHIDPN",
          "rwtcrnh","ragey_e",
          "expert_p","hurd_p","lasso_p","PrDem")

RAND_HRS_1 <- RAND_HRS[,names(RAND_HRS) %in% cols]
RAND_HRS_2 <- as.data.frame(lapply(RAND_HRS[,!names(RAND_HRS) %in% cols], as.factor))
RAND_HRS <- cbind(RAND_HRS_1, RAND_HRS_2)
#remove without sample weights
RAND_HRS <- RAND_HRS[!is.na(RAND_HRS$rwtcrnh),]

####################################################################################################
## Imputation for the self-respondents HRS
####################################################################################################

RAND_HRS <- RAND_HRS[!is.na(RAND_HRS$lasso_p) & !is.na(RAND_HRS$PrDem) & !is.na(RAND_HRS$expert_p) & !is.na(RAND_HRS$hurd_p) ,]

RAND_HRS_self <- RAND_HRS[RAND_HRS$proxy == 0,]

RAND_HRS_self <- subset(RAND_HRS_self, select = c( "HHIDPN", "rwtcrnh", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                   "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
                                                   "adl", "iadl", "proxy", "LTHS", "HSGED", "GTHS","raedyrs",
                                                   "adlch", "iadlch", "date_recallch", "bwc1ch", "ser7ch",
                                                   "rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
                                                   "female",
                                                   "date_recall", "name_recall","ser7","iword","dword","bwc1","rpres", "rscis", "rcact",
                                                   #"IQCODE_ch","proxy_lag","date_recall_lag", "ser7_lag","rpres_lag", "iword_lag", "dword_lag",
                                                   "expert_p", "lasso_p", "hurd_p", "PrDem")
)

self_impute <- RAND_HRS_self[, !names(RAND_HRS_self) %in% c( "rwtcrnh", "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                             "lasso_p", "hurd_p", "expert_p","PrDem")]
self_remain <- RAND_HRS_self[, names(RAND_HRS_self) %in% c("rwtcrnh",  "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                           "lasso_p", "hurd_p", "expert_p","PrDem")]

#check missingness
summary(self_impute)

imputation_self <- missForest(as.data.frame(self_impute),verbose=TRUE,maxiter=5,ntree= 500)
imputed_self <- imputation_self$ximp
imputed_self <- data.frame(imputed_self)
imputed_self$HHIDPN <- RAND_HRS_self$HHIDPN
## add outcome back into data
imputed_self_data <- left_join(imputed_self, self_remain)
write.csv(imputed_self_data, "imputed_HRS_self.csv", row.names=TRUE)


####################################################################################################
## Imputation for the proxy-respondents HCAP
####################################################################################################

RAND_HCAP_proxy <- RAND_HCAP[RAND_HCAP$proxy == 1,]
RAND_HCAP_proxy <- subset(RAND_HCAP_proxy, select = c( "HHIDPN", "HCAP16WGTR", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                       "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
                                                       "adl", "iadl", "proxy", "LTHS", "HSGED", "GTHS","raedyrs",
                                                       "adlch", "iadlch", 
                                                       #"date_recallch", "bwc1ch", "ser7ch",
                                                       # "rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
                                                       "female",
                                                       #"date_recall", "name_recall","ser7","iword","dword","bwc1", "rpres", "rscis", "rcact",
                                                       "IQCODE","IQCODE_ch","proxy_lag","date_recall_lag", "ser7_lag","rpres_lag", "iword_lag", "dword_lag",
                                                       "expert_p", "lasso_p", "hurd_p", "PrDem","R1HCAPDX") )

proxy_impute <- RAND_HCAP_proxy[, !names(RAND_HCAP_proxy) %in% c( "rwtcrnh", "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                                  "lasso_p", "hurd_p", "expert_p","PrDem", "R1HCAPDX")]
proxy_remain <- RAND_HCAP_proxy[, names(RAND_HCAP_proxy) %in% c("rwtcrnh",  "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                                "lasso_p", "hurd_p", "expert_p","PrDem", "R1HCAPDX")]
#check missingness
summary(proxy_impute)

#missForest
imputation_proxy <- missForest(as.data.frame(proxy_impute),verbose=TRUE,maxiter=5,ntree= 500)
imputed_proxy <- imputation_proxy$ximp
imputed_proxy <- data.frame(imputed_proxy)
imputed_proxy$HHIDPN <- RAND_HCAP_proxy$HHIDPN
imputed_proxy$wave <- RAND_HCAP_proxy$wave
## add outcome back into data
imputed_proxy_data <- left_join(imputed_proxy, proxy_remain)
write.csv(imputed_proxy_data, "imputed_HCAP_proxy.csv", row.names=TRUE)


####################################################################################################
## Imputation for the proxy-respondents HRS
####################################################################################################

RAND_HRS_proxy <- RAND_HRS[RAND_HRS$proxy == 1,]
RAND_HRS_proxy <- subset(RAND_HRS_proxy, select = c( "HHIDPN", "rwtcrnh", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                     "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
                                                     "adl", "iadl", "proxy", "LTHS", "HSGED", "GTHS","raedyrs",
                                                     "adlch", "iadlch",
                                                     #"date_recallch", "bwc1ch", "ser7ch",
                                                     # "rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
                                                     "female",
                                                     # "date_recall", "name_recall","ser7","iword","dword","bwc1","rpres", "rscis", "rcact",
                                                     "IQCODE", "IQCODE_ch","proxy_lag","date_recall_lag", "ser7_lag","rpres_lag", "iword_lag", "dword_lag",
                                                     "expert_p", "lasso_p", "hurd_p", "PrDem")
)


proxy_impute <- RAND_HRS_proxy[, !names(RAND_HRS_proxy) %in% c( "rwtcrnh", "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                                "lasso_p", "hurd_p", "expert_p","PrDem")]
proxy_remain <- RAND_HRS_proxy[, names(RAND_HRS_proxy) %in% c("rwtcrnh",  "HHIDPN", "Hispanic", "NH_white", "NH_black", "NH_other",
                                                              "lasso_p", "hurd_p", "expert_p","PrDem")]

#check missingness
summary(proxy_impute)

imputation_proxy <- missForest(as.data.frame(proxy_impute),verbose=TRUE,maxiter=5,ntree= 500)
imputed_proxy <- imputation_proxy$ximp
imputed_proxy <- data.frame(imputed_proxy)
imputed_proxy$HHIDPN <- RAND_HRS_proxy$HHIDPN
## add outcome back into data
imputed_proxy_data <- left_join(imputed_proxy, proxy_remain)
write.csv(imputed_proxy_data, "imputed_HRS_proxy.csv", row.names=TRUE)




