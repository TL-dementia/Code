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
proxy_source <- read_csv("imputed_HRS_proxy.csv")
proxy_gold <- read_csv("imputed_HCAP_proxy.csv")

# Assign dementia 
proxy_gold$dementia <- ifelse(proxy_gold$R1HCAPDX == 3, 1, 0)

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

datalist2 = list()
datalist2 = vector("list", length = nTrials)

options(scipen = 999)

proxy_source$edu1 <- ifelse(proxy_source$raedyrs < 6, 1, 0)
proxy_source$edu2 <- ifelse(proxy_source$raedyrs >= 6 & proxy_source$raedyrs < 9, 1, 0)
proxy_source$edu3 <- ifelse(proxy_source$raedyrs >= 9 & proxy_source$raedyrs < 12, 1, 0)
proxy_source$edu4 <- ifelse(proxy_source$raedyrs == 12, 1, 0)
proxy_source$edu5 <- ifelse(proxy_source$raedyrs > 12, 1, 0)


proxy_gold$edu1 <- ifelse(proxy_gold$raedyrs < 6, 1, 0)
proxy_gold$edu2 <- ifelse(proxy_gold$raedyrs >= 6 & proxy_gold$raedyrs < 9, 1, 0)
proxy_gold$edu3 <- ifelse(proxy_gold$raedyrs >= 9 & proxy_gold$raedyrs < 12, 1, 0)
proxy_gold$edu4 <- ifelse(proxy_gold$raedyrs == 12, 1, 0)
proxy_gold$edu5 <- ifelse(proxy_gold$raedyrs > 12, 1, 0)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

for(i in 1:nTrials){
  #for proxy
  
  gold_subset <- proxy_gold
  source <- proxy_source
  
  gold_subset <- data.table(gold_subset)
  
  train_size <- floor(1 * nrow(gold_subset))
  in_rows <- sample(c(1:nrow(gold_subset)), size = train_size, replace = FALSE)
  
  #Bootstrapped train sample
  train <- gold_subset[sample(x = nrow(gold_subset), size = nrow(gold_subset), replace = TRUE),]
  test <- gold_subset[in_rows, ]
  
  
  # test including both proxy and non proxy. 
  
  #Hurd
  source_1 <- data.table(source)
  source_1[,  c( "expert_p","lasso_p", "PrDem") := NULL]
  source_1 <- na.omit(source_1)
  #Expert
  source_2 <- data.table(source)
  source_2[,  c( "hurd_p","lasso_p", "PrDem") := NULL]
  source_2 <- na.omit(source_2)
  #Lasso
  source_3 <- data.table(source)
  source_3[,  c( "hurd_p","expert_p", "PrDem") := NULL]
  source_3 <- na.omit(source_3)
  #Latent
  source_4 <- data.table(source)
  source_4[,  c( "hurd_p","expert_p", "lasso_p") := NULL]
  source_4 <- na.omit(source_4)
  
  
  # Select the predictor for the proxy respondents
  
  sel <- c( 
    # Common part
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "edu2", "edu3", "edu4", "edu5", 
    "female",
    "adl", "iadl", 
    "adlch", "iadlch",
    "proxy_lag",
    # Only for self - proxy proxy respondents
    "date_recall_lag",  "ser7_lag",  "rpres_lag", "iword_lag", "dword_lag",
    # Only for proxy - proxy proxy respondents
    "IQCODE", "IQCODE_ch"
  )
  
  #Same as the original Hurd
  sel2 <- c( 
    # Common part
    "hagecat75","hagecat80","hagecat85","hagecat90",
    "HSGED","GTHS",
    "female",
    "adl", "iadl", 
    "adlch", "iadlch",
    "proxy_lag",
    # Only for self - proxy proxy respondents
    "date_recall_lag",  "ser7_lag",  "rpres_lag", "iword_lag", "dword_lag",
    # Only for proxy - proxy proxy respondents
    "IQCODE", "IQCODE_ch"
  )
  
  target <- list(x = NULL, y = NULL)
  target$x <- data.matrix(train[,sel, with = FALSE])
  target$y <- as.integer(train$dementia)
  
  #Learn from the sources - four existing models.
  #Hurd algorithm
  source1 <- list(x = NULL, y = NULL)
  source1$x <- data.matrix(source_1[,sel, with = FALSE])
  source1$y <- as.integer(ifelse(source_1$hurd_p > 0.5, 1, 0) )
  #Expert algorithm
  source2 <- list(x = NULL, y = NULL)
  source2$x<- data.matrix(source_2[,sel, with = FALSE])
  source2$y<- as.integer(ifelse(source_2$expert_p > 0.5, 1, 0))
  #Lasso algorithm
  source3 <- list(x = NULL, y = NULL)
  source3$x<- data.matrix(source_3[,sel, with = FALSE])
  source3$y<- as.integer(ifelse(source_3$lasso_p > 0.5, 1, 0))
  #Latent algorithm
  source4 <- list(x = NULL, y = NULL)
  source4$x<- data.matrix(source_4[,sel, with = FALSE])
  source4$y<- as.integer(ifelse(source_4$PrDem > 0.5 ,1, 0))
  
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
  
  weight_list <- list(source_1$rwtcrnh, source_2$rwtcrnh, source_3$rwtcrnh,
                      source_4$rwtcrnh)
  
  
  # test with bootstrapped sample.
  
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
  
  y.pred.TL <- data.frame(predict(fit.TL, my_list.test$target$x, type = "response"))
  y.pred.TL$i <- i
  datalist2[[i]] <- y.pred.TL
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

comparisons <-cbind(show_res, show_res_sd,
                    show_slope, show_slope_sd,
                    show_intercept, show_intercept_sd,
                    show_auc, show_auc_sd, show_rc , show_rc_sd)

write.csv(comparisons, "comparisons_proxy.csv", row.names=TRUE)

show_data = do.call(cbind, datalist)

show_data <- data.frame(show_data)

show_data$detected <- ifelse(show_data[ , grepl("delta", names(show_data))]!= 0, 1, 0)
show_data <- show_data %>% 
  mutate(  #detection_count = rowSums(show_data[ , grepl("detection", names(show_data))]),
    average_beta = rowMeans(show_data[ , grepl("beta", names(show_data))]),
    detection_bias = rowSums(show_data[ , grepl("detected", names(show_data))]),
    average_bias = rowMeans(show_data[ , grepl("delta", names(show_data))]))

show_detection_inside <- show_data[,c("average_beta", "detection_bias", "average_bias")]
show_detection_inside <- round(show_detection_inside, 3)

#Change the column name

show_data <- show_data %>%
  select(-matches("^fit\\.TL\\.\\.\\.fitting\\.list\\.\\.\\.delta_a"), 
         starts_with("fit.TL...fitting.list......delta_a") %>% 
           setNames(str_replace_all(paste0("delta", 1:nTrials), "\\.", "_")))

show_data <- show_data  %>%
  select(-matches("^fit\\.TL\\.\\.\\.beta"), 
         starts_with("fit.TL...beta") %>% 
           setNames(str_replace_all(paste0("beta", 1:nTrials), "\\.", "_")))

write.csv(show_data, "show_data_proxy.csv", row.names=TRUE)



