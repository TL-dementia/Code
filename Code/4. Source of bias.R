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

x <- c("haven",  "tidyverse", "ggplot2", "dplyr","dplyr", "gtools")
#install.packages(x) 
lapply(x, library, character.only = TRUE)
setwd("~/Documents/TL_dementia/")

nTrials = 1000
threshold = nTrials*0.5

library(readr)
small_data_w <- read_csv("small_data_w.csv")
small_data_h <- read_csv("small_data_h.csv")
small_data_b <- read_csv("small_data_b.csv")

colnames(small_data_w)[1] ="name"
colnames(small_data_h)[1] ="name"
colnames(small_data_b)[1] ="name"

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

small_data_w <- small_data_w %>% 
  mutate(
    average_beta = rowMeans(small_data_w[ , grepl("beta", names(small_data_w))]),
    average_bias = rowMeans(small_data_w[ , grepl("delta", names(small_data_w))]),
    sd_bias = apply(small_data_w[ , grepl("delta", names(small_data_w))], 1, sd),
    lower_bound = average_bias - 1.96*(apply(small_data_w[ , grepl("delta", names(small_data_w))], 1, sd)/sqrt(nTrials)),
    upper_bound = average_bias + 1.96*(apply(small_data_w[ , grepl("delta", names(small_data_w))], 1, sd)/sqrt(nTrials)))  


library(dplyr)
for(i in 1:nTrials) {
  col_name <- paste0("s", i)
  small_data_w[[col_name]] <- ifelse(small_data_w[[paste0("delta", i)]] > small_data_w$lower_bound & 
                                     small_data_w[[paste0("delta", i)]] < small_data_w$upper_bound, 1, 0)
}

pattern <- "s"
# Generate the vector of column names
col_names <- paste0(pattern, 1:nTrials)
# Calculate the row sums for columns with the specified names
small_data_w$count <- rowSums(small_data_w[col_names])
small_data_w <- small_data_w[ ,c("name","average_bias", "count","sd_bias","lower_bound", "upper_bound")]
small_data_w <- small_data_w[small_data_w$count >= threshold,]

########################################################################################################################################################################

rownames(small_data_w) <- small_data_w$name

rownames(small_data_w)[which(rownames(small_data_w) == "hagecat75")] <- "Age 75-79"
rownames(small_data_w)[which(rownames(small_data_w) == "hagecat80")] <- "Age 80-84"
rownames(small_data_w)[which(rownames(small_data_w) == "hagecat85")] <- "Age 85-90"
rownames(small_data_w)[which(rownames(small_data_w) == "hagecat90")] <- "Age 90-"

rownames(small_data_w)[which(rownames(small_data_w) == "female")] <- "Female"

rownames(small_data_w)[which(rownames(small_data_w) == "edu2")] <- "Edu 6-8 yrs"
rownames(small_data_w)[which(rownames(small_data_w) == "edu3")] <- "Edu 9-11 yrs"
rownames(small_data_w)[which(rownames(small_data_w) == "edu4")] <- "Edu 12 yrs"
rownames(small_data_w)[which(rownames(small_data_w) == "edu5")] <- "Edu >12 yrs"

rownames(small_data_w)[which(rownames(small_data_w) == "date_recall")] <- "Date orientation"
rownames(small_data_w)[which(rownames(small_data_w) == "iword")] <- "Immediate word recall"
rownames(small_data_w)[which(rownames(small_data_w) == "dword")] <- "Delay word recall"
rownames(small_data_w)[which(rownames(small_data_w) == "ser7")] <- "Serial 7 subtraction"
rownames(small_data_w)[which(rownames(small_data_w) == "bwc1")] <- "Backward count"
rownames(small_data_w)[which(rownames(small_data_w) == "rscis")] <- "Name (Scissors)"
rownames(small_data_w)[which(rownames(small_data_w) == "rcact")] <- "Name (Cactus)"
rownames(small_data_w)[which(rownames(small_data_w) == "rpres")] <- "Name (President)"

rownames(small_data_w)[which(rownames(small_data_w) == "adl")] <- "ADL"
rownames(small_data_w)[which(rownames(small_data_w) == "iadl")] <- "IADL"
rownames(small_data_w)[which(rownames(small_data_w) == "adlch")] <- "Change in ADL"
rownames(small_data_w)[which(rownames(small_data_w) == "iadlch")] <- "Change in IADL"

rownames(small_data_w)[which(rownames(small_data_w) == "dwordch")] <- "Change in Delay"
rownames(small_data_w)[which(rownames(small_data_w) == "iwordch")] <- "Change in Immediate"
rownames(small_data_w)[which(rownames(small_data_w) == "date_recallch")] <- "Change in Date"
rownames(small_data_w)[which(rownames(small_data_w) == "rpresch")] <- "Change in Name (president)"
rownames(small_data_w)[which(rownames(small_data_w) == "rcactch")] <- "Change in Name (cactus)"
rownames(small_data_w)[which(rownames(small_data_w) == "rscisch")] <- "Change in Name (scissors)"
rownames(small_data_w)[which(rownames(small_data_w) == "bwc1ch")] <- "Change in Backward"
rownames(small_data_w)[which(rownames(small_data_w) == "ser7ch")] <- "Change in Serial7"


small_data_w$id <- rownames(small_data_w)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

small_data_b <- small_data_b %>% 
  mutate(  #detection_count = rowSums(small_data_b[ , grepl("detection", names(small_data_b))]),
    average_beta = rowMeans(small_data_b[ , grepl("beta", names(small_data_b))]),
    average_bias = rowMeans(small_data_b[ , grepl("delta", names(small_data_b))]),
    sd_bias = apply(small_data_b[ , grepl("delta", names(small_data_b))], 1, sd),
    lower_bound = average_bias - 1.96*(apply(small_data_b[ , grepl("delta", names(small_data_b))], 1, sd)/sqrt(nTrials)),
    upper_bound = average_bias + 1.96*(apply(small_data_b[ , grepl("delta", names(small_data_b))], 1, sd)/sqrt(nTrials)))  

library(dplyr)
for(i in 1:nTrials) {
  col_name <- paste0("s", i)
  small_data_b[[col_name]] <- ifelse(small_data_b[[paste0("delta", i)]] > small_data_b$lower_bound & 
                                       small_data_b[[paste0("delta", i)]] < small_data_b$upper_bound, 1, 0)
}

pattern <- "s"
# Generate the vector of column names
col_names <- paste0(pattern, 1:nTrials)
# Calculate the row sums for columns with the specified names
small_data_b$count <- rowSums(small_data_b[col_names])
small_data_b <- small_data_b[ ,c("name","average_bias", "count","sd_bias","lower_bound", "upper_bound")]
small_data_b <- small_data_b[small_data_b$count >= threshold,]


rownames(small_data_b) <- small_data_b$name

rownames(small_data_b)[which(rownames(small_data_b) == "hagecat75")] <- "Age 75-79"
rownames(small_data_b)[which(rownames(small_data_b) == "hagecat80")] <- "Age 80-84"
rownames(small_data_b)[which(rownames(small_data_b) == "hagecat85")] <- "Age 85-90"
rownames(small_data_b)[which(rownames(small_data_b) == "hagecat90")] <- "Age 90-"

rownames(small_data_b)[which(rownames(small_data_b) == "female")] <- "Female"

rownames(small_data_b)[which(rownames(small_data_b) == "edu2")] <- "Edu 6-8 yrs"
rownames(small_data_b)[which(rownames(small_data_b) == "edu3")] <- "Edu 9-11 yrs"
rownames(small_data_b)[which(rownames(small_data_b) == "edu4")] <- "Edu 12 yrs"
rownames(small_data_b)[which(rownames(small_data_b) == "edu5")] <- "Edu >12 yrs"

rownames(small_data_b)[which(rownames(small_data_b) == "date_recall")] <- "Date orientation"
rownames(small_data_b)[which(rownames(small_data_b) == "iword")] <- "Immediate word recall"
rownames(small_data_b)[which(rownames(small_data_b) == "dword")] <- "Delay word recall"
rownames(small_data_b)[which(rownames(small_data_b) == "ser7")] <- "Serial 7 subtraction"
rownames(small_data_b)[which(rownames(small_data_b) == "bwc1")] <- "Backward count"
rownames(small_data_b)[which(rownames(small_data_b) == "rscis")] <- "Name (Scissors)"
rownames(small_data_b)[which(rownames(small_data_b) == "rcact")] <- "Name (Cactus)"
rownames(small_data_b)[which(rownames(small_data_b) == "rpres")] <- "Name (President)"

rownames(small_data_b)[which(rownames(small_data_b) == "adl")] <- "ADL"
rownames(small_data_b)[which(rownames(small_data_b) == "iadl")] <- "IADL"
rownames(small_data_b)[which(rownames(small_data_b) == "adlch")] <- "Change in ADL"
rownames(small_data_b)[which(rownames(small_data_b) == "iadlch")] <- "Change in IADL"

rownames(small_data_b)[which(rownames(small_data_b) == "dwordch")] <- "Change in Delay"
rownames(small_data_b)[which(rownames(small_data_b) == "iwordch")] <- "Change in Immediate"
rownames(small_data_b)[which(rownames(small_data_b) == "date_recallch")] <- "Change in Date"
rownames(small_data_b)[which(rownames(small_data_b) == "rpresch")] <- "Change in Name (president)"
rownames(small_data_b)[which(rownames(small_data_b) == "rcactch")] <- "Change in Name (cactus)"
rownames(small_data_b)[which(rownames(small_data_b) == "rscisch")] <- "Change in Name (scissors)"
rownames(small_data_b)[which(rownames(small_data_b) == "bwc1ch")] <- "Change in Backward"
rownames(small_data_b)[which(rownames(small_data_b) == "ser7ch")] <- "Change in Serial7"

small_data_b$id <- rownames(small_data_b)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

small_data_h <- small_data_h %>% 
  mutate(  #detection_count = rowSums(small_data_h[ , grepl("detection", names(small_data_h))]),
    average_beta = rowMeans(small_data_h[ , grepl("beta", names(small_data_h))]),
    average_bias = rowMeans(small_data_h[ , grepl("delta", names(small_data_h))]),
    sd_bias = apply(small_data_h[ , grepl("delta", names(small_data_h))], 1, sd),
    lower_bound = average_bias - 1.96*(apply(small_data_h[ , grepl("delta", names(small_data_h))], 1, sd)/sqrt(nTrials)),
    upper_bound = average_bias + 1.96*(apply(small_data_h[ , grepl("delta", names(small_data_h))], 1, sd)/sqrt(nTrials)))  

library(dplyr)
for(i in 1:nTrials) {
  col_name <- paste0("s", i)
  small_data_h[[col_name]] <- ifelse(small_data_h[[paste0("delta", i)]] > small_data_h$lower_bound & 
                                       small_data_h[[paste0("delta", i)]] < small_data_h$upper_bound, 1, 0)
}

pattern <- "s"
# Generate the vector of column names
col_names <- paste0(pattern, 1:nTrials)
# Calculate the row sums for columns with the specified names
small_data_h$count <- rowSums(small_data_h[col_names])
small_data_h <- small_data_h[ ,c("name","average_bias", "count","sd_bias","lower_bound", "upper_bound")]
small_data_h <- small_data_h[small_data_h$count >= threshold,]

rownames(small_data_h) <- small_data_h$name

rownames(small_data_h)[which(rownames(small_data_h) == "hagecat75")] <- "Age 75-79"
rownames(small_data_h)[which(rownames(small_data_h) == "hagecat80")] <- "Age 80-84"
rownames(small_data_h)[which(rownames(small_data_h) == "hagecat85")] <- "Age 85-90"
rownames(small_data_h)[which(rownames(small_data_h) == "hagecat90")] <- "Age 90-"

rownames(small_data_h)[which(rownames(small_data_h) == "female")] <- "Female"

rownames(small_data_h)[which(rownames(small_data_h) == "edu2")] <- "Edu 6-8 yrs"
rownames(small_data_h)[which(rownames(small_data_h) == "edu3")] <- "Edu 9-11 yrs"
rownames(small_data_h)[which(rownames(small_data_h) == "edu4")] <- "Edu 12 yrs"
rownames(small_data_h)[which(rownames(small_data_h) == "edu5")] <- "Edu >12 yrs"

rownames(small_data_h)[which(rownames(small_data_h) == "date_recall")] <- "Date orientation"
rownames(small_data_h)[which(rownames(small_data_h) == "iword")] <- "Immediate word recall"
rownames(small_data_h)[which(rownames(small_data_h) == "dword")] <- "Delay word recall"
rownames(small_data_h)[which(rownames(small_data_h) == "ser7")] <- "Serial 7 subtraction"
rownames(small_data_h)[which(rownames(small_data_h) == "bwc1")] <- "Backward count"
rownames(small_data_h)[which(rownames(small_data_h) == "rscis")] <- "Name (Scissors)"
rownames(small_data_h)[which(rownames(small_data_h) == "rcact")] <- "Name (Cactus)"
rownames(small_data_h)[which(rownames(small_data_h) == "rpres")] <- "Name (President)"

rownames(small_data_h)[which(rownames(small_data_h) == "adl")] <- "ADL"
rownames(small_data_h)[which(rownames(small_data_h) == "iadl")] <- "IADL"
rownames(small_data_h)[which(rownames(small_data_h) == "adlch")] <- "Change in ADL"
rownames(small_data_h)[which(rownames(small_data_h) == "iadlch")] <- "Change in IADL"

rownames(small_data_h)[which(rownames(small_data_h) == "dwordch")] <- "Change in Delay"
rownames(small_data_h)[which(rownames(small_data_h) == "iwordch")] <- "Change in Immediate"
rownames(small_data_h)[which(rownames(small_data_h) == "date_recallch")] <- "Change in Date"
rownames(small_data_h)[which(rownames(small_data_h) == "rpresch")] <- "Change in Name (president)"
rownames(small_data_h)[which(rownames(small_data_h) == "rcactch")] <- "Change in Name (cactus)"
rownames(small_data_h)[which(rownames(small_data_h) == "rscisch")] <- "Change in Name (scissors)"
rownames(small_data_h)[which(rownames(small_data_h) == "bwc1ch")] <- "Change in Backward"
rownames(small_data_h)[which(rownames(small_data_h) == "ser7ch")] <- "Change in Serial7"


small_data_h$id <- rownames(small_data_h)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

small_data_b$group <- "Black"
small_data_w$group <- "White"
small_data_h$group <- "Hispanic"
small_data <- rbind(small_data_b, small_data_h, small_data_w)
small_data$count <- round((small_data$count/nTrials)*100,0) 

small_data$id <- factor(small_data$id ,levels = c( "Age 75-79","Age 80-84","Age 85-90","Age 90-","Female",
                                                   "Edu 6-8 yrs", "Edu 9-11 yrs", "Edu 12 yrs","Edu >12 yrs",
                                                   "Date orientation","Immediate word recall","Delay word recall",
                                                   "Serial 7 subtraction","Backward count","Name (Scissors)","Name (Cactus)","Name (President)",
                                                   "Change in Date","Change in Immediate", "Change in Delay", "Change in Serial7", "Change in Backward",
                                                   "Change in Name (scissors)","Change in Name (cactus)","Change in Name (president)","ADL","IADL",
                                                   "Change in ADL", "Change in IADL"))



bias <- ggplot(data = small_data, aes(x = id, y = average_bias, fill = count)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_fill_gradient(low = "lightyellow", high = "red", name = 'Detection frequency (%)') +
  scale_color_continuous(limits = c(threshold, 1000)) +
  scale_y_continuous(breaks = seq(round(min(small_data$average_bias),2) , round( max(small_data$average_bias),2), 0.01), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15, color = "black"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.key.size = unit(0.75, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        #plot.caption = element_text(size = 12, face = "italic"),
        #plot.title = element_text(size = 16),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  facet_grid(group ~ .) +
  labs(#title = "Average predictor bias by race and ethnicity",
    x = NULL,
    y = "Average predictor bias")+
  geom_hline(yintercept = 0, linewidth = 0.15, color = "black")


