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

install.packages("srvyr")
install.packages("gtsummary")
install.packages("plyr")

library(srvyr)
library(gtsummary)
library(gtsummary)
library(tidyverse)
library(plyr)


proxy_gold <- read_csv("imputed_HCAP_proxy.csv")
self_gold <- read_csv("imputed_HCAP_self.csv")

# Assign dementia 
self_gold$dementia <- ifelse(self_gold$R1HCAPDX == 3, 1, 0)
proxy_gold$dementia <- ifelse(proxy_gold$R1HCAPDX == 3, 1, 0)

self_gold$edu1 <- ifelse(self_gold$raedyrs < 6, 1, 0)
self_gold$edu2 <- ifelse(self_gold$raedyrs >= 6 & self_gold$raedyrs < 9, 1, 0)
self_gold$edu3 <- ifelse(self_gold$raedyrs >= 9 & self_gold$raedyrs < 12, 1, 0)
self_gold$edu4 <- ifelse(self_gold$raedyrs == 12, 1, 0)
self_gold$edu5 <- ifelse(self_gold$raedyrs > 12, 1, 0)

proxy_gold$edu1 <- ifelse(proxy_gold$raedyrs < 6, 1, 0)
proxy_gold$edu2 <- ifelse(proxy_gold$raedyrs >= 6 & proxy_gold$raedyrs < 9, 1, 0)
proxy_gold$edu3 <- ifelse(proxy_gold$raedyrs >= 9 & proxy_gold$raedyrs < 12, 1, 0)
proxy_gold$edu4 <- ifelse(proxy_gold$raedyrs == 12, 1, 0)
proxy_gold$edu5 <- ifelse(proxy_gold$raedyrs > 12, 1, 0)

sel <- c(  "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
           "edu1", "edu2", "edu3", "edu4", "edu5", 
          "female",
          "adl", "iadl", 
          "date_recall", "bwc1","ser7", "rscis", "rcact","rpres","iword","dword",
          "dementia")

####################################################################################################
## Table 1 self-respondents
####################################################################################################


#NH Black
self_gold_b <- self_gold[self_gold$NH_black==1,]
summary_b <-
  survey::svydesign(self_gold_b$HHIDPN, data = subset(self_gold_b , select = sel), weights = NULL) %>%
#you need to change the weights =  self_gold$HCAP16WGTR for the values.
  tbl_svysummary(by = dementia, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                 all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("adl", "iadl", "date_recall", "ser7") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test")) 
#Check out.
summary_b
#Hispanic
self_gold_h <- self_gold[self_gold$Hispanic==1,]
summary_h <-
  survey::svydesign(self_gold_h$HHIDPN, data = subset(self_gold_h , select = sel), weights = NULL) %>%
  #you need to change the weights =  self_gold$HCAP16WGTR for the values.
  tbl_svysummary(by = dementia, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                 all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("adl", "iadl",  "date_recall", "ser7") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test"))  
#Check out.
summary_h

#NH white
self_gold_w <- self_gold[self_gold$NH_white==1,]
summary_w <-
  survey::svydesign(self_gold_w$HHIDPN, data = subset(self_gold_w , select = sel), weights = NULL) %>%
  #you need to change the weights =  self_gold$HCAP16WGTR for the values.
  tbl_svysummary(by = dementia, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                 all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("adl", "iadl",  "date_recall", "ser7") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test"))  
#Check out.
summary_w
#we can print out summary_b, summary_h, summary_w and check the values.

####################################################################################################
## Table 1 proxy-respondents
####################################################################################################

# Assign dementia 
proxy_gold$dementia <- ifelse(proxy_gold$R1HCAPDX == 3, 1, 0)

sel <- c(  "hagecat70","hagecat75","hagecat80","hagecat85","hagecat90",
          "edu1", "edu2", "edu3", "edu4", "edu5", 
           "female",
           "adl", "iadl", 
           "IQCODE", "dementia")


proxy_summary <-
  survey::svydesign(proxy_gold$HHIDPN, data = subset(proxy_gold , select = sel), weights = NULL) %>%
  # you need to change the weights =proxy_gold$HCAP16WGTR
  tbl_svysummary(by = dementia, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("adl", "iadl",  "IQCODE") ~ "continuous"),
                 digits = all_continuous() ~ 2) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test"))  

#Check out!
proxy_summary

####################################################################################################
## Table 2 comparing HCAP vs. HRS
####################################################################################################

proxy_source <- read_csv("imputed_HRS_proxy.csv")
self_source <- read_csv("imputed_HRS_self.csv")

sel <- c( "hagecat70", "hagecat75","hagecat80","hagecat85","hagecat90",
          "NH_black", "NH_white", "Hispanic",
          "edu1", "edu2", "edu3", "edu4", "edu5", 
          "female",
          #"HCAP16WGTR",
          #"rwtcrnh",
          "adl", "iadl", "HHIDPN")

gold <- rbind(proxy_gold[,sel],  self_gold[,sel])

sel <- c( "hagecat70", "hagecat75","hagecat80","hagecat85","hagecat90",
          "NH_black", "NH_white", "Hispanic",
          "edu1", "edu2", "edu3", "edu4", "edu5", 
          "female",
          #"HCAP16WGTR",
          #"rwtcrnh",
          "adl", "iadl", "HHIDPN")
source<- rbind(proxy_source[,sel],  self_source[,sel])

gold$group <- "gold"
source$group <- "source"
gold_source <- rbind(gold, source)

sel <- c( "hagecat70", "hagecat75","hagecat80","hagecat85","hagecat90",
          "NH_black", "NH_white", "Hispanic",
          "edu1", "edu2", "edu3", "edu4", "edu5", 
          "group",
          "female",
          #"HCAP16WGTR",
          #"rwtcrnh",
          "adl", "iadl")
  
HRS_HCAP <-
  survey::svydesign(gold_source$HHIDPN, data = subset(gold_source , select = sel), weights = NULL) %>%
  tbl_svysummary(by = "group", statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("adl", "iadl") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test")) 

#Check out!
HRS_HCAP











  