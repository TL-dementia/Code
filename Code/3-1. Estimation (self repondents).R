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
x <- c("haven",  "tidyverse", "ggplot2", "ggpubr", "dplyr", "expss", "foreign", "nnet", "reshape2", 
       "dplyr", "gtools","devtools", "xtable", "missForest", "glmnet", "glmtrans", "survey", "zoo", "pROC", "caret", "PRROC",
       "matrixStats", "mltools")
install.packages(x) 
lapply(x, library, character.only = TRUE)

#Please set your own directory.
# Set working directory #For example, I created a folder "TL_dementia".
setwd("~/Documents/TL_dementia/")

####################################################################################################
## Download the data
####################################################################################################

self_source <- read_csv("imputed_HRS_self.csv")
self_gold <- read_csv("imputed_HCAP_self.csv")

# Assign dementia 
self_gold$dementia <- ifelse(self_gold$R1HCAPDX == 3, 1, 0)

set.seed(41489)

# number of repetition.
nTrials = 1000

#These matrices will store the values.
# Brier score
res = matrix(0, nrow = nTrials, ncol = 6)
#Calibration slope
slope = matrix(0, nrow = nTrials, ncol = 6)
#Calibration slope
intercept = matrix(0, nrow = nTrials, ncol = 6)
#AUC
auc = matrix(0, nrow = nTrials, ncol = 6)
#AUPRC
pr = matrix(0, nrow = nTrials, ncol = 6)


#Store value of the estimator
datalist = list()
datalist = vector("list", length = nTrials)

options(scipen = 999)

self_source$edu1 <- ifelse(self_source$raedyrs < 6, 1, 0)
self_source$edu2 <- ifelse(self_source$raedyrs >= 6 & self_source$raedyrs < 9, 1, 0)
self_source$edu3 <- ifelse(self_source$raedyrs >= 9 & self_source$raedyrs < 12, 1, 0)
self_source$edu4 <- ifelse(self_source$raedyrs == 12, 1, 0)
self_source$edu5 <- ifelse(self_source$raedyrs > 12, 1, 0)


self_gold$edu1 <- ifelse(self_gold$raedyrs < 6, 1, 0)
self_gold$edu2 <- ifelse(self_gold$raedyrs >= 6 & self_gold$raedyrs < 9, 1, 0)
self_gold$edu3 <- ifelse(self_gold$raedyrs >= 9 & self_gold$raedyrs < 12, 1, 0)
self_gold$edu4 <- ifelse(self_gold$raedyrs == 12, 1, 0)
self_gold$edu5 <- ifelse(self_gold$raedyrs > 12, 1, 0)

########################################################################################################################################################################
####################################For Non-Hispanic Black participants######################################################################################
########################################################################################################################################################################


for(i in 1:nTrials){
  #for self-respondents
  
  gold_subset <- self_gold[self_gold$NH_black == 1,]
  source_subset_data <- self_source[self_source$NH_black == 1,]
  
  gold_subset <- data.table(gold_subset)
  
  train_size <- floor(1 * nrow(gold_subset))
  in_rows <- sample(c(1:nrow(gold_subset)), size = train_size, replace = FALSE)
  
  train <- gold_subset[sample(x = nrow(gold_subset), size = nrow(gold_subset), replace = TRUE),]
  test <- gold_subset[in_rows, ]
  
  #Hurd
  source_subset_data_1 <- data.table(source_subset_data)
  source_subset_data_1[,  c( "expert_p","lasso_p", "PrDem") := NULL]
  source_subset_data_1 <- na.omit(source_subset_data_1)
  #Expert
  source_subset_data_2 <- data.table(source_subset_data)
  source_subset_data_2[,  c( "hurd_p","lasso_p", "PrDem") := NULL]
  source_subset_data_2 <- na.omit(source_subset_data_2)
  #Lasso
  source_subset_data_3 <- data.table(source_subset_data)
  source_subset_data_3[,  c( "hurd_p","expert_p", "PrDem") := NULL]
  source_subset_data_3 <- na.omit(source_subset_data_3)
  #Latent
  source_subset_data_4 <- data.table(source_subset_data)
  source_subset_data_4[,  c( "hurd_p","expert_p", "lasso_p") := NULL]
  source_subset_data_4 <- na.omit(source_subset_data_4)
  
  #Education categories expanded version.
  sel <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "edu2", "edu3", "edu4", "edu5", 
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  #Same as the original Hurd
  sel2 <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "HSGED","GTHS",
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(train[,sel, with = FALSE])
  target$y <- as.integer(train$dementia)
  
  source1 <- list(x = NULL, y = NULL)
  source1$x <- data.matrix(source_subset_data_1[,sel, with = FALSE])
  source1$y <- as.integer(ifelse(source_subset_data_1$hurd_p > 0.5, 1, 0) )
  
  source2 <- list(x = NULL, y = NULL)
  source2$x<- data.matrix(source_subset_data_2[,sel, with = FALSE])
  source2$y<- as.integer(ifelse(source_subset_data_2$expert_p > 0.5, 1, 0))
  
  source3 <- list(x = NULL, y = NULL)
  source3$x<- data.matrix(source_subset_data_3[,sel, with = FALSE])
  source3$y<- as.integer(ifelse(source_subset_data_3$lasso_p > 0.5, 1, 0))
  
  source4 <- list(x = NULL, y = NULL)
  source4$x<- data.matrix(source_subset_data_4[,sel, with = FALSE])
  source4$y<- as.integer(ifelse(source_subset_data_4$PrDem > 0.5 ,1, 0))
  
  source <- list(source1, source2, source3, source4)
  
  my_list <- list(target = target,
                  source = source)
  
  sample_weight  <- as.integer(train$HCAP16WGTR)
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  target2 <- list(x = NULL, y = NULL)
  target2$x <- data.matrix(train[,sel2, with = FALSE])
  target2$y <- as.integer(train$dementia)
  
  test2 <- list(x = NULL, y = NULL)
  test2$x <- data.matrix(test[,sel2, with = FALSE])
  test2$y <- as.integer(test$dementia)
  
  weight_list <- list(source_subset_data_1$rwtcrnh, source_subset_data_2$rwtcrnh, source_subset_data_3$rwtcrnh,
                      source_subset_data_4$rwtcrnh)
  
  # Hurd
  test$hurd_p <- case_when(test$hurd_p == 0 ~ 0.000000000000001,
                           test$hurd_p == 1 ~ 0.999999999999999,
                           test$hurd_p != 0 | test$hurd_p != 1 ~ test$hurd_p)
  test <- test[!is.na(test$hurd_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_hurd <- test$hurd_p
  res[i,1]= mean((GP_hurd - my_list.test$target$y)^2)
  slope[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[2]
  intercept[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[1]
  
  auc[i,1] = auc(my_list.test$target$y, GP_hurd)
  
  pr_list <- pr.curve(GP_hurd[test$dementia == 1], GP_hurd[test$dementia == 0], curve = T)
  pr[i,1] = pr_list$auc.integral
  
  # Expert
  test$expert_p <- case_when(test$expert_p == 0 ~ 0.000000000000001,
                             test$expert_p == 1 ~ 0.999999999999999,
                             test$expert_p != 0 | test$expert_p != 1 ~ test$expert_p)
  test <- test[!is.na(test$expert_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_expert <- test$expert_p
  res[i,2]= mean((GP_expert - my_list.test$target$y)^2)
  slope[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[2]
  intercept[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[1]
  
  auc[i,2] = auc(my_list.test$target$y, GP_expert)
  
  pr_list <- pr.curve(GP_expert[test$dementia == 1], GP_expert[test$dementia == 0], curve = T)
  pr[i,2] = pr_list$auc.integral
  
  # Lasso
  test$lasso_p <- case_when(test$lasso_p == 0 ~ 0.000000000000001,
                            test$lasso_p == 1 ~ 0.999999999999999,
                            test$lasso_p != 0 | test$lasso_p != 1 ~ test$lasso_p)
  test <- test[!is.na(test$lasso_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_lasso <- test$lasso_p
  res[i,3]= mean((GP_lasso - my_list.test$target$y)^2)
  slope[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[2]
  intercept[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[1]
  auc[i,3] = auc(my_list.test$target$y, GP_lasso)
  
  pr_list <- pr.curve(GP_lasso[test$dementia == 1], GP_lasso[test$dementia == 0], curve = T)
  pr[i,3] = pr_list$auc.integral
  
  # Latent
  test$PrDem <- case_when(test$PrDem == 0 ~ 0.000000000000001,
                          test$PrDem == 1 ~ 0.999999999999999,
                          test$PrDem != 0 | test$PrDem != 1 ~ test$PrDem)
  Latent <- test$PrDem
  
  res[i,4]= mean((Latent - my_list.test$target$y)^2)
  slope[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[2]
  intercept[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[1]
  auc[i,4] = auc(my_list.test$target$y, Latent)
  
  pr_list <- pr.curve(Latent[test$dementia == 1], Latent[test$dementia == 0], curve = T)
  pr[i,4] = pr_list$auc.integral
  
  #Hurd-HCAP
  fit.hurd_hcap <- glmnet(target2$x, target2$y, family = binomial(link = "probit"), lambda = 0,  maxit=500000)
  
  y.pred.hurd_hcap <- predict(fit.hurd_hcap, test2$x, type = "response")
  
  res[i,5] = mean((y.pred.hurd_hcap - test2$y)^2)
  
  slope[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[2]
  intercept[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[1]
  auc[i,5] = auc(test2$y, y.pred.hurd_hcap)
  
  pr_list <- pr.curve(y.pred.hurd_hcap[test$dementia == 1], y.pred.hurd_hcap[test$dementia == 0], curve = T)
  pr[i,5] = pr_list$auc.integral
  
  #Transfer learning
  fit.TL <- glmtrans(target = my_list$target, source = my_list$source, standardize = TRUE, 
                     lambda = c(transfer = "lambda.min", debias = "lambda.min", detection = "lambda.1se"), 
                     family = "binomial", transfer.source.id = 1, cores = 2, target.weights = sample_weight, source.weights = weight_list)
  
  y.pred.TL <- predict(fit.TL, my_list.test$target$x, type = "response")
  
  y.pred.TL <- data.frame(y.pred.TL)
  y.pred.TL$y.pred.TL <- case_when(y.pred.TL$y.pred.TL == 0 ~ 0.000000000000001,
                                   y.pred.TL$y.pred.TL == 1 ~ 0.999999999999999,
                                   y.pred.TL$y.pred.TL != 0 | y.pred.TL$y.pred.TL != 1 ~ y.pred.TL$y.pred.TL)
  y.pred.TL <- data.matrix(y.pred.TL$y.pred.TL)
  
  res[i,6] = mean((y.pred.TL - my_list.test$target$y)^2)
  slope[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[2]
  intercept[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[1]
  auc[i,6] = auc(my_list.test$target$y, y.pred.TL)
  
  pr_list <- pr.curve(y.pred.TL[test$dementia == 1], y.pred.TL[test$dementia == 0], curve = T)
  pr[i,6] = pr_list$auc.integral
  
  dat <- data.frame(fit.TL[["beta"]], fit.TL[["fitting.list"]][["delta_a"]])
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat
  
  print(i)
}

show_res <- round(colMeans(res),3)
show_res_sd <- round(colSds(res),3)

show_slope <- round(colMeans(slope),2)
show_slope_sd <- round(colSds(slope),2)

show_intercept <- round(colMeans(intercept),2)
show_intercept_sd <- round(colSds(intercept),2)

show_auc <- round(colMeans(auc),2)
show_auc_sd <- round(colSds(auc),2)

show_rc <- round(colMeans(pr),2)
show_rc_sd <- round(colSds(pr),2)

comparisons_b <-cbind(show_res, show_res_sd,
                      show_slope, show_slope_sd,
                      show_intercept, show_intercept_sd,
                      show_auc, show_auc_sd, show_rc , show_rc_sd)


write.csv(comparisons_b, "comparisons_Black.csv", row.names=TRUE)

small_data = do.call(cbind, datalist)

small_data <- data.frame(small_data)

small_data$detected <- ifelse(small_data[ , grepl("delta", names(small_data))]!= 0, 1, 0)
small_data <- small_data %>% 
  mutate(  #detection_count = rowSums(small_data[ , grepl("detection", names(small_data))]),
    average_beta = rowMeans(small_data[ , grepl("beta", names(small_data))]),
    detection_bias = rowSums(small_data[ , grepl("detected", names(small_data))]),
    average_bias = rowMeans(small_data[ , grepl("delta", names(small_data))]))

#Change the column name

small_data <- small_data %>%
  select(-matches("^fit\\.TL\\.\\.\\.fitting\\.list\\.\\.\\.delta_a"), 
         starts_with("fit.TL...fitting.list......delta_a") %>% 
           setNames(str_replace_all(paste0("delta", 1:nTrials), "\\.", "_")))

small_data <- small_data  %>%
  select(-matches("^fit\\.TL\\.\\.\\.beta"), 
         starts_with("fit.TL...beta") %>% 
           setNames(str_replace_all(paste0("beta", 1:nTrials), "\\.", "_")))

write.csv(small_data, "small_data_b.csv", row.names=TRUE)



########################################################################################################################################################################
####################################For Hispanic participants######################################################################################
########################################################################################################################################################################
for(i in 1:nTrials){
  #for self-respondents
  
  gold_subset <- self_gold[self_gold$Hispanic == 1,]
  source_subset_data <- self_source[self_source$Hispanic == 1,]
  
  gold_subset <- data.table(gold_subset)
  
  train_size <- floor(1 * nrow(gold_subset))
  in_rows <- sample(c(1:nrow(gold_subset)), size = train_size, replace = FALSE)
  
  train <- gold_subset[sample(x = nrow(gold_subset), size = nrow(gold_subset), replace = TRUE),]
  test <- gold_subset[in_rows, ]
  
  #Hurd
  source_subset_data_1 <- data.table(source_subset_data)
  source_subset_data_1[,  c( "expert_p","lasso_p", "PrDem") := NULL]
  source_subset_data_1 <- na.omit(source_subset_data_1)
  #Expert
  source_subset_data_2 <- data.table(source_subset_data)
  source_subset_data_2[,  c( "hurd_p","lasso_p", "PrDem") := NULL]
  source_subset_data_2 <- na.omit(source_subset_data_2)
  #Lasso
  source_subset_data_3 <- data.table(source_subset_data)
  source_subset_data_3[,  c( "hurd_p","expert_p", "PrDem") := NULL]
  source_subset_data_3 <- na.omit(source_subset_data_3)
  #Latent
  source_subset_data_4 <- data.table(source_subset_data)
  source_subset_data_4[,  c( "hurd_p","expert_p", "lasso_p") := NULL]
  source_subset_data_4 <- na.omit(source_subset_data_4)

  #Education categories expanded version.
  sel <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "edu2", "edu3", "edu4", "edu5", 
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  #Same as the original Hurd
  sel2 <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "HSGED","GTHS",
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  
  self_gold$red
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(train[,sel, with = FALSE])
  target$y <- as.integer(train$dementia)
  
  source1 <- list(x = NULL, y = NULL)
  source1$x <- data.matrix(source_subset_data_1[,sel, with = FALSE])
  source1$y <- as.integer(ifelse(source_subset_data_1$hurd_p > 0.5, 1, 0) )
  
  source2 <- list(x = NULL, y = NULL)
  source2$x<- data.matrix(source_subset_data_2[,sel, with = FALSE])
  source2$y<- as.integer(ifelse(source_subset_data_2$expert_p > 0.5, 1, 0))
  
  source3 <- list(x = NULL, y = NULL)
  source3$x<- data.matrix(source_subset_data_3[,sel, with = FALSE])
  source3$y<- as.integer(ifelse(source_subset_data_3$lasso_p > 0.5, 1, 0))
  
  source4 <- list(x = NULL, y = NULL)
  source4$x<- data.matrix(source_subset_data_4[,sel, with = FALSE])
  source4$y<- as.integer(ifelse(source_subset_data_4$PrDem > 0.5 ,1, 0))
  
  source <- list(source1, source2, source3, source4)
  
  my_list <- list(target = target,
                  source = source)
  
  sample_weight  <- as.integer(train$HCAP16WGTR)
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  target2 <- list(x = NULL, y = NULL)
  target2$x <- data.matrix(train[,sel2, with = FALSE])
  target2$y <- as.integer(train$dementia)
  
  test2 <- list(x = NULL, y = NULL)
  test2$x <- data.matrix(test[,sel2, with = FALSE])
  test2$y <- as.integer(test$dementia)
  
  
  weight_list <- list(source_subset_data_1$rwtcrnh, source_subset_data_2$rwtcrnh, source_subset_data_3$rwtcrnh,
                      source_subset_data_4$rwtcrnh)
  
  # Hurd
  test$hurd_p <- case_when(test$hurd_p == 0 ~ 0.000000000000001,
                           test$hurd_p == 1 ~ 0.999999999999999,
                           test$hurd_p != 0 | test$hurd_p != 1 ~ test$hurd_p)
  test <- test[!is.na(test$hurd_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_hurd <- test$hurd_p
  res[i,1]= mean((GP_hurd - my_list.test$target$y)^2)
  slope[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[2]
  intercept[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[1]
  
  auc[i,1] = auc(my_list.test$target$y, GP_hurd)
  
  pr_list <- pr.curve(GP_hurd[test$dementia == 1], GP_hurd[test$dementia == 0], curve = T)
  pr[i,1] = pr_list$auc.integral
  
  # Expert
  test$expert_p <- case_when(test$expert_p == 0 ~ 0.000000000000001,
                             test$expert_p == 1 ~ 0.999999999999999,
                             test$expert_p != 0 | test$expert_p != 1 ~ test$expert_p)
  test <- test[!is.na(test$expert_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_expert <- test$expert_p
  res[i,2]= mean((GP_expert - my_list.test$target$y)^2)
  slope[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[2]
  intercept[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[1]
  
  auc[i,2] = auc(my_list.test$target$y, GP_expert)
  
  pr_list <- pr.curve(GP_expert[test$dementia == 1], GP_expert[test$dementia == 0], curve = T)
  pr[i,2] = pr_list$auc.integral
  
  # Lasso
  test$lasso_p <- case_when(test$lasso_p == 0 ~ 0.000000000000001,
                            test$lasso_p == 1 ~ 0.999999999999999,
                            test$lasso_p != 0 | test$lasso_p != 1 ~ test$lasso_p)
  test <- test[!is.na(test$lasso_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_lasso <- test$lasso_p
  res[i,3]= mean((GP_lasso - my_list.test$target$y)^2)
  slope[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[2]
  intercept[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[1]
  auc[i,3] = auc(my_list.test$target$y, GP_lasso)
  
  pr_list <- pr.curve(GP_lasso[test$dementia == 1], GP_lasso[test$dementia == 0], curve = T)
  pr[i,3] = pr_list$auc.integral
  
  # Latent
  test$PrDem <- case_when(test$PrDem == 0 ~ 0.000000000000001,
                          test$PrDem == 1 ~ 0.999999999999999,
                          test$PrDem != 0 | test$PrDem != 1 ~ test$PrDem)
  Latent <- test$PrDem
  
  res[i,4]= mean((Latent - my_list.test$target$y)^2)
  slope[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[2]
  intercept[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[1]
  auc[i,4] = auc(my_list.test$target$y, Latent)
  
  pr_list <- pr.curve(Latent[test$dementia == 1], Latent[test$dementia == 0], curve = T)
  pr[i,4] = pr_list$auc.integral
  
  #Hurd-HCAP
  fit.hurd_hcap <- glmnet(target2$x, target2$y, family = binomial(link = "probit"), lambda = 0,  maxit=500000)
  
  y.pred.hurd_hcap <- predict(fit.hurd_hcap, test2$x, type = "response")
  
  res[i,5] = mean((y.pred.hurd_hcap - test2$y)^2)
  
  slope[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[2]
  intercept[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[1]
  auc[i,5] = auc(test2$y, y.pred.hurd_hcap)
  
  pr_list <- pr.curve(y.pred.hurd_hcap[test$dementia == 1], y.pred.hurd_hcap[test$dementia == 0], curve = T)
  pr[i,5] = pr_list$auc.integral
  
  #Transfer learning
  fit.TL <- glmtrans(target = my_list$target, source = my_list$source, standardize = TRUE, 
                     lambda = c(transfer = "lambda.min", debias = "lambda.min", detection = "lambda.1se"), 
                     family = "binomial", transfer.source.id = 1, cores = 2, target.weights = sample_weight, source.weights = weight_list)
  
  y.pred.TL <- predict(fit.TL, my_list.test$target$x, type = "response")
  
  y.pred.TL <- data.frame(y.pred.TL)
  y.pred.TL$y.pred.TL <- case_when(y.pred.TL$y.pred.TL == 0 ~ 0.000000000000001,
                                   y.pred.TL$y.pred.TL == 1 ~ 0.999999999999999,
                                   y.pred.TL$y.pred.TL != 0 | y.pred.TL$y.pred.TL != 1 ~ y.pred.TL$y.pred.TL)
  y.pred.TL <- data.matrix(y.pred.TL$y.pred.TL)
  
  res[i,6] = mean((y.pred.TL - my_list.test$target$y)^2)
  slope[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[2]
  intercept[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[1]
  auc[i,6] = auc(my_list.test$target$y, y.pred.TL)
  
  pr_list <- pr.curve(y.pred.TL[test$dementia == 1], y.pred.TL[test$dementia == 0], curve = T)
  pr[i,6] = pr_list$auc.integral
  
  dat <- data.frame(fit.TL[["beta"]], fit.TL[["fitting.list"]][["delta_a"]])
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat
  
  print(i)
}

show_res <- round(colMeans(res),3)
show_res_sd <- round(colSds(res),3)

show_slope <- round(colMeans(slope),2)
show_slope_sd <- round(colSds(slope),2)

show_intercept <- round(colMeans(intercept),2)
show_intercept_sd <- round(colSds(intercept),2)

show_auc <- round(colMeans(auc),2)
show_auc_sd <- round(colSds(auc),2)

show_rc <- round(colMeans(pr),2)
show_rc_sd <- round(colSds(pr),2)

comparisons_h <-cbind(show_res, show_res_sd,
                      show_slope, show_slope_sd,
                      show_intercept, show_intercept_sd,
                      show_auc, show_auc_sd, show_rc , show_rc_sd)

write.csv(comparisons_h, "comparisons_Hispanic.csv", row.names=TRUE)

small_data = do.call(cbind, datalist)

small_data <- data.frame(small_data)

small_data$detected <- ifelse(small_data[ , grepl("delta", names(small_data))]!= 0, 1, 0)
small_data <- small_data %>% 
  mutate(  #detection_count = rowSums(small_data[ , grepl("detection", names(small_data))]),
    average_beta = rowMeans(small_data[ , grepl("beta", names(small_data))]),
    detection_bias = rowSums(small_data[ , grepl("detected", names(small_data))]),
    average_bias = rowMeans(small_data[ , grepl("delta", names(small_data))]))

#Change the column name

small_data <- small_data %>%
  select(-matches("^fit\\.TL\\.\\.\\.fitting\\.list\\.\\.\\.delta_a"), 
         starts_with("fit.TL...fitting.list......delta_a") %>% 
           setNames(str_replace_all(paste0("delta", 1:nTrials), "\\.", "_")))

small_data <- small_data  %>%
  select(-matches("^fit\\.TL\\.\\.\\.beta"), 
         starts_with("fit.TL...beta") %>% 
           setNames(str_replace_all(paste0("beta", 1:nTrials), "\\.", "_")))

write.csv(small_data, "small_data_h.csv", row.names=TRUE)

########################################################################################################################################################################
####################################For Non-Hispanic white participants######################################################################################
########################################################################################################################################################################
for(i in 1:nTrials){
  #for self-respondents
  
  gold_subset <- self_gold[self_gold$NH_white == 1,]
  source_subset_data <- self_source[self_source$NH_white == 1,]
  
  gold_subset <- data.table(gold_subset)
  
  train_size <- floor(1 * nrow(gold_subset))
  in_rows <- sample(c(1:nrow(gold_subset)), size = train_size, replace = FALSE)
  
  train <- gold_subset[sample(x = nrow(gold_subset), size = nrow(gold_subset), replace = TRUE),]
  test <- gold_subset[in_rows, ]
  
  #Hurd
  source_subset_data_1 <- data.table(source_subset_data)
  source_subset_data_1[,  c( "expert_p","lasso_p", "PrDem") := NULL]
  source_subset_data_1 <- na.omit(source_subset_data_1)
  #Expert
  source_subset_data_2 <- data.table(source_subset_data)
  source_subset_data_2[,  c( "hurd_p","lasso_p", "PrDem") := NULL]
  source_subset_data_2 <- na.omit(source_subset_data_2)
  #Lasso
  source_subset_data_3 <- data.table(source_subset_data)
  source_subset_data_3[,  c( "hurd_p","expert_p", "PrDem") := NULL]
  source_subset_data_3 <- na.omit(source_subset_data_3)
  #Latent
  source_subset_data_4 <- data.table(source_subset_data)
  source_subset_data_4[,  c( "hurd_p","expert_p", "lasso_p") := NULL]
  source_subset_data_4 <- na.omit(source_subset_data_4)
  
  #Education categories expanded version.
  sel <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "edu2", "edu3", "edu4", "edu5", 
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  #Same as the original Hurd
  sel2 <- c( 
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "HSGED","GTHS",
    "female",
    "date_recall", "bwc1","ser7", "iword","dword", "rscis", "rcact","rpres",
    "date_recallch", "bwc1ch", "ser7ch","rscisch", "rcactch", "rpresch", "iwordch", "dwordch",
    "adl", "iadl",
    "adlch", "iadlch")
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(train[,sel, with = FALSE])
  target$y <- as.integer(train$dementia)
  
  source1 <- list(x = NULL, y = NULL)
  source1$x <- data.matrix(source_subset_data_1[,sel, with = FALSE])
  source1$y <- as.integer(ifelse(source_subset_data_1$hurd_p > 0.5, 1, 0) )
  
  source2 <- list(x = NULL, y = NULL)
  source2$x<- data.matrix(source_subset_data_2[,sel, with = FALSE])
  source2$y<- as.integer(ifelse(source_subset_data_2$expert_p > 0.5, 1, 0))
  
  source3 <- list(x = NULL, y = NULL)
  source3$x<- data.matrix(source_subset_data_3[,sel, with = FALSE])
  source3$y<- as.integer(ifelse(source_subset_data_3$lasso_p > 0.5, 1, 0))
  
  source4 <- list(x = NULL, y = NULL)
  source4$x<- data.matrix(source_subset_data_4[,sel, with = FALSE])
  source4$y<- as.integer(ifelse(source_subset_data_4$PrDem > 0.5 ,1, 0))
  
  source <- list(source1, source2, source3, source4)
  
  my_list <- list(target = target,
                  source = source)
  
  sample_weight  <- as.integer(train$HCAP16WGTR)
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  target2 <- list(x = NULL, y = NULL)
  target2$x <- data.matrix(train[,sel2, with = FALSE])
  target2$y <- as.integer(train$dementia)
  
  test2 <- list(x = NULL, y = NULL)
  test2$x <- data.matrix(test[,sel2, with = FALSE])
  test2$y <- as.integer(test$dementia)
  
  
  weight_list <- list(source_subset_data_1$rwtcrnh, source_subset_data_2$rwtcrnh, source_subset_data_3$rwtcrnh,
                      source_subset_data_4$rwtcrnh)
  
  # Hurd
  test$hurd_p <- case_when(test$hurd_p == 0 ~ 0.000000000000001,
                           test$hurd_p == 1 ~ 0.999999999999999,
                           test$hurd_p != 0 | test$hurd_p != 1 ~ test$hurd_p)
  test <- test[!is.na(test$hurd_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_hurd <- test$hurd_p
  res[i,1]= mean((GP_hurd - my_list.test$target$y)^2)
  slope[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[2]
  intercept[i,1] <- glm(my_list.test$target$y ~ log(GP_hurd/(1-GP_hurd)), family = binomial)$coefficients[1]
  
  auc[i,1] = auc(my_list.test$target$y, GP_hurd)
  
  pr_list <- pr.curve(GP_hurd[test$dementia == 1], GP_hurd[test$dementia == 0], curve = T)
  pr[i,1] = pr_list$auc.integral
  
  # Expert
  test$expert_p <- case_when(test$expert_p == 0 ~ 0.000000000000001,
                             test$expert_p == 1 ~ 0.999999999999999,
                             test$expert_p != 0 | test$expert_p != 1 ~ test$expert_p)
  test <- test[!is.na(test$expert_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_expert <- test$expert_p
  res[i,2]= mean((GP_expert - my_list.test$target$y)^2)
  slope[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[2]
  intercept[i,2] <- glm(my_list.test$target$y ~ log(GP_expert/(1-GP_expert)), family = binomial)$coefficients[1]
  
  auc[i,2] = auc(my_list.test$target$y, GP_expert)
  
  pr_list <- pr.curve(GP_expert[test$dementia == 1], GP_expert[test$dementia == 0], curve = T)
  pr[i,2] = pr_list$auc.integral
  
  # Lasso
  test$lasso_p <- case_when(test$lasso_p == 0 ~ 0.000000000000001,
                            test$lasso_p == 1 ~ 0.999999999999999,
                            test$lasso_p != 0 | test$lasso_p != 1 ~ test$lasso_p)
  test <- test[!is.na(test$lasso_p),]
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(test[,sel, with = FALSE])
  target$y <- as.integer(test$dementia)
  my_list.test <- list(target = target)
  
  GP_lasso <- test$lasso_p
  res[i,3]= mean((GP_lasso - my_list.test$target$y)^2)
  slope[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[2]
  intercept[i,3] <- glm(my_list.test$target$y ~ log(GP_lasso/(1-GP_lasso)), family = binomial)$coefficients[1]
  auc[i,3] = auc(my_list.test$target$y, GP_lasso)
  
  pr_list <- pr.curve(GP_lasso[test$dementia == 1], GP_lasso[test$dementia == 0], curve = T)
  pr[i,3] = pr_list$auc.integral
  
  # Latent
  test$PrDem <- case_when(test$PrDem == 0 ~ 0.000000000000001,
                          test$PrDem == 1 ~ 0.999999999999999,
                          test$PrDem != 0 | test$PrDem != 1 ~ test$PrDem)
  Latent <- test$PrDem
  
  res[i,4]= mean((Latent - my_list.test$target$y)^2)
  slope[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[2]
  intercept[i,4] <- glm(my_list.test$target$y ~ log(Latent/(1-Latent)), family = binomial)$coefficients[1]
  auc[i,4] = auc(my_list.test$target$y, Latent)
  
  pr_list <- pr.curve(Latent[test$dementia == 1], Latent[test$dementia == 0], curve = T)
  pr[i,4] = pr_list$auc.integral
  
  #Hurd-HCAP
  fit.hurd_hcap <- glmnet(target2$x, target2$y, family = binomial(link = "probit"), lambda = 0,  maxit=500000)
  
  y.pred.hurd_hcap <- predict(fit.hurd_hcap, test2$x, type = "response")
  
  res[i,5] = mean((y.pred.hurd_hcap - test2$y)^2)
  
  slope[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[2]
  intercept[i,5] <- glm(test2$y ~ log(y.pred.hurd_hcap/(1-y.pred.hurd_hcap)), family = binomial)$coefficients[1]
  auc[i,5] = auc(test2$y, y.pred.hurd_hcap)
  
  pr_list <- pr.curve(y.pred.hurd_hcap[test$dementia == 1], y.pred.hurd_hcap[test$dementia == 0], curve = T)
  pr[i,5] = pr_list$auc.integral
  
  #Transfer learning
  fit.TL <- glmtrans(target = my_list$target, source = my_list$source, standardize = TRUE,  
                     lambda = c(transfer = "lambda.min", debias = "lambda.min", detection = "lambda.1se"), 
                     family = "binomial", transfer.source.id = 1, cores = 2, target.weights = sample_weight, source.weights = weight_list)
  
  y.pred.TL <- predict(fit.TL, my_list.test$target$x, type = "response")
  
  y.pred.TL <- data.frame(y.pred.TL)
  y.pred.TL$y.pred.TL <- case_when(y.pred.TL$y.pred.TL == 0 ~ 0.000000000000001,
                                   y.pred.TL$y.pred.TL == 1 ~ 0.999999999999999,
                                   y.pred.TL$y.pred.TL != 0 | y.pred.TL$y.pred.TL != 1 ~ y.pred.TL$y.pred.TL)
  y.pred.TL <- data.matrix(y.pred.TL$y.pred.TL)
  
  res[i,6] = mean((y.pred.TL - my_list.test$target$y)^2)
  slope[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[2]
  intercept[i,6] <- glm(my_list.test$target$y ~ log(y.pred.TL/(1-y.pred.TL)), family = binomial)$coefficients[1]
  auc[i,6] = auc(my_list.test$target$y, y.pred.TL)
  
  pr_list <- pr.curve(y.pred.TL[test$dementia == 1], y.pred.TL[test$dementia == 0], curve = T)
  pr[i,6] = pr_list$auc.integral
  
  dat <- data.frame(fit.TL[["beta"]], fit.TL[["fitting.list"]][["delta_a"]])
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat
  
  print(i)
}

show_res <- round(colMeans(res),3)
show_res_sd <- round(colSds(res),3)

show_slope <- round(colMeans(slope),2)
show_slope_sd <- round(colSds(slope),2)

show_intercept <- round(colMeans(intercept),2)
show_intercept_sd <- round(colSds(intercept),2)

show_auc <- round(colMeans(auc),2)
show_auc_sd <- round(colSds(auc),2)

show_rc <- round(colMeans(pr),2)
show_rc_sd <- round(colSds(pr),2)

comparisons_w <-cbind(show_res, show_res_sd,
                      show_slope, show_slope_sd,
                      show_intercept, show_intercept_sd,
                      show_auc, show_auc_sd, show_rc , show_rc_sd)

write.csv(comparisons_w, "comparisons_white.csv", row.names=TRUE)

small_data = do.call(cbind, datalist)

small_data <- data.frame(small_data)

#Change the column name

small_data <- small_data %>%
  select(-matches("^fit\\.TL\\.\\.\\.fitting\\.list\\.\\.\\.delta_a"), 
         starts_with("fit.TL...fitting.list......delta_a") %>% 
           setNames(str_replace_all(paste0("delta", 1:nTrials), "\\.", "_")))

small_data <- small_data  %>%
  select(-matches("^fit\\.TL\\.\\.\\.beta"), 
         starts_with("fit.TL...beta") %>% 
           setNames(str_replace_all(paste0("beta", 1:nTrials), "\\.", "_")))

write.csv(small_data, "small_data_w.csv", row.names=TRUE)


comparisons_b
comparisons_h
comparisons_w




