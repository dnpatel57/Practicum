################## DATA MODELLING ##################
##################### CL GROUP #####################
# ------------ WRITTEN BY:  M.CONDALARY -------------


#Notes:
#1. All files pertaining to the quarter we're predicting are name_yr.qtr
#   Where qtr is the quarter to be predicted (data comes from prior quarter)


#------------------ LOAD PACKAGES -------------------

library("caret")
library("fastAdaboost")
library("pROC")


# -------------- SET FILE PATH (USER) ---------------

setwd(file.path("C:","Users","mallo","Documents","Century Link Proj","Data Prep"))
setwd(file.path( "C:","Centurylink Project","LSU MSA Data Project (1)"))           
setwd(file.path( "F:","New folder","Century Link Project"))


# --------------- PREPROCESSING:  RAW -----------------

#Bring in Final Data set from Data Processing.R as Raw
file7<-"Final_2015Q2.csv"
Raw <- read.csv(file = file7)

#TEMP STEP:  Drop all variables from data set that won't be used!
#ie.      :  Num_calls, CURR_PAYING_RATE, X, BAN_SEQ, num_pr, ADR_STATE_CODE
#            ADR_ZIP_GEO_CODE, ADR_CITY, BAN_STATUS, CUSTOMER_TYPE
Raw$X <- NULL
Raw$BAN_SEQ <- NULL
Raw$CURR_PAYING_RATE <- NULL
Raw$num_pr <- NULL
Raw$Num_Calls <- NULL
Raw$BAN_STATUS <- NULL
Raw$CUSTOMER_TYPE <- NULL
Raw$ADR_ZIP_GEO_CODE <- NULL
Raw$ADR_CITY <- NULL
Raw$ADR_STATE_CODE <- NULL

#TEMP STEP:  Create indicators for vars
Raw$nocall_ind <- ifelse(Raw$DONT_CALL_IND %in% c('~','0','N'), 0, 1)
Raw$DONT_CALL_IND <- NULL

Raw$nomail_ind <- ifelse(Raw$DONT_MAIL_IND %in% c('N','~'), 0, 1)
Raw$DONT_MAIL_IND <- NULL

Raw$noemail_ind <- ifelse(Raw$MKT_DONT_EMAIL_IND %in% c('N','~'), 0, 1)
Raw$MKT_DONT_EMAIL_IND <- NULL

Raw$repcode_ind <- ifelse(Raw$REP_CODE %in% c('~','0'), 0, 1)
Raw$REP_CODE <- NULL

with( Raw, 
if (BILL_CYCLE <= 10 ){
  bcycle <- '1-10'
} else if (BILL_CYCLE > 10 & BILL_CYCLE <= 20){
  bcycle <- '11-20'
} else {
  bcycle <- '20plus'
} )
Raw$BILL_METHOD <- NULL

Raw$BLmail_ind <-

#Change OUTCOME (target var) to a factor
Raw$OUTCOME <- as.factor(Raw$OUTCOME)
levels(Raw$OUTCOME) <- make.names(c("No","Yes"))

#Get naive rule for classification
prop.table(table(Raw$OUTCOME))

#Partition data into 4 samples (20k rows each)
set.seed(2017)
trainindex_20k <- createFolds(Raw$OUTCOME, k = 4)
Samp1_20k<- Raw[trainindex_20k$Fold1 ,]

#Partition data into 8 samples (10k rows each)
trainindex_10k <- createFolds(Raw$OUTCOME, k = 8)
Samp1_10k<- Raw[trainindex_10k$Fold1 ,]
Samp2_10k<- Raw[trainindex_10k$Fold2 ,]
Samp3_10k<- Raw[trainindex_10k$Fold3 ,]
Samp4_10k<- Raw[trainindex_10k$Fold4 ,]
Samp5_10k<- Raw[trainindex_10k$Fold5 ,]
Samp6_10k<- Raw[trainindex_10k$Fold6 ,]
Samp7_10k<- Raw[trainindex_10k$Fold7 ,]
Samp8_10k<- Raw[trainindex_10k$Fold8 ,]

#Write the samples for k = 8
file_dhruv<-"sample_1.csv"
write.csv(x = Samp1_10k, file = file_dhruv) #repeat for each Sampx_10k


#-------------- PREPROCESSING:  NEW_IND --------------

#After recoding several variables, the data run much better through models
#We had errors because there were too many levels!  
#We need to implement coding in R script (working above)

file8<-"NewInd_2015Q2.csv"
New <- read.csv(file = file8)

#Change OUTCOME (target var) to a factor
New$OUTCOME <- as.factor(New$OUTCOME)
levels(New$OUTCOME) <- make.names(c("No","Yes"))

#Replace 'NA' in Region with 'Other'
New$Region = factor(New$Region, levels=c(levels(New$Region), 'Other'))
New$Region[is.na(New$Region)] <- 'Other'

###DROP ANY ROWS WITH ANY OTHER NA VALUES

#Get training and validation set
new_index <- createDataPartition(y = New$OUTCOME, p = 0.7)

#Build data sets from partition index
Train <- New[  new_index$Resample1 , ]
Valid <- New[-(new_index$Resample1), ]

#Eliminate num_prods and BAN_SEQ (so we can use all other vars to model)
Train$Num_Prods <- NULL
Train$BAN_SEQ <- NULL
Valid$Num_Prods <- NULL
Valid$BAN_SEQ <- NULL


#------------------ TEST VARIABLES -------------------

#1. Create Controls for training
fit_ctrl <- trainControl(method = "cv", 
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

#2. Model variable importance with multiple models

## - - - SVM - - - ##
svm_fit <- train(Class ~ ., 
                 data = Samp1_10k, 
                 method = "svmRadial", 
                 trControl = fit_ctrl, 
                 preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")

## - - - ADABOOST - - - ##
set.seed(2017)
ada_fit <- train(form = OUTCOME ~ .,
                 data = Samp1,
                 method = "adaboost",
                 trControl = fit_ctrl, 
                 metric = "ROC",
                 na.action = na.exclude)

## - - - TREEBAG - - - ##
set.seed(2017)
bag_fit <- train(form = OUTCOME ~ .,
                 data = Train, 
                 method = "treebag",
                 trControl = fit_ctrl,
                 nbagg = 5,
                 metric = "ROC",
                 na.action = na.exclude)

bag_fit
varImp(bag_fit)
plot(varImp(bag_fit), top = 10)

# Validation:
bag_valid <- predict(bag_fit, newdata=Valid, type='prob')

bag_valid$CLASSED <- ifelse(bag_valid > 0.5, "Yes", "No")

confusionMatrix(data=Valid$CLASSED, reference=Valid$OUTCOME, positive = "Yes")


## - - - LOGISTIC - - - ##
frmla_test <- OUTCOME ~ max_payrate + bun_ind + Num_Calls_Last + Region 
                        + bcycle + nocall_ind + nomail_ind + noemail_ind + BLmail_ind
frmla_best <- OUTCOME ~ max_payrate + Num_Calls_Last + bcycle

log_fit <- train(form = frmla_test,
             data=Valid, 
             method="glm",
             family="binomial",
             trControl = fit_ctrl,
             na.action = na.exclude)

summary(log_fit)
plot(varImp(log_fit))

#Validation:
log_valid <- predict(log_fit, newdata=Valid, type='prob')
log_valid$CLASSED <- ifelse(log_valid  > 0.5, "Yes", "No")

confusionMatrix(data=log_valid$CLASSED, reference=Valid$OUTCOME, positive = "Yes")


#--------------------- MODELS ------------------------

## a. Binomial Logistic

# building #

# validation #

##b. Multinomial Logistic (baseline = 0 calls)

# building #

# validation #

##c. Comparison of Models

#-------------------- FINAL MODEL --------------------



# --------------- RECLASSIFY TEST DATA ---------------



#-------------- OVERLAY CHARACTERISTICS --------------
