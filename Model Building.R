################## DATA MODELLING ##################
##################### CL GROUP #####################
# ------------ WRITTEN BY:  M.CONDALARY -------------


#Notes:
#1. All files pertaining to the quarter we're predicting are name_yr.qtr
#   Where qtr is the quarter to be predicted (data comes from prior quarter)
#2. 
#3. 


#------------------ LOAD PACKAGES -------------------


library("caret")
library("fastAdaboost")

# ----------------- PREPROCESSING -------------------

setwd(file.path("C:","Users","mallo","Documents","Century Link Proj","Data Prep")) #FILE PATH
setwd(file.path( "C:","Centurylink Project","LSU MSA Data Project (1)"))           #FILE PATH
setwd(file.path( "F:","New folder","Century Link Project"))

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


#------------------ TEST VARIABLES -------------------

## Create Controls for training
fit_ctrl <- trainControl(method = "none", 
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

#1. Use decision tree (rpart)
rpart_fit <- train(OUTCOME ~ ., 
                   data = Samp1_10k,
                   method = "rpart",
                   metric = "ROC",
                   trControl = fit_ctrl,
                   na.action = na.exclude)

#2. Use SVM
svm_fit <- train(Class ~ ., 
                 data = Samp1_10k, 
                 method = "svmRadial", 
                 trControl = fit_ctrl, 
                 preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")

#3. Use adaboost classification tree
set.seed(2017)
ada_fit <- train(form = OUTCOME ~ .,
                 data = Samp1,
                 method = "adaboost",
                 trControl = fit_ctrl, 
                 metric = "ROC",
                 na.action = na.exclude)

#4. Try "treebag" method
set.seed(2017)
bag_fit <- train(form = OUTCOME ~ .,
                 data = Samp1_10k, 
                 method = "treebag",
                 trControl = fit_ctrl,
                 nbagg = 5,
                 metric = "ROC",
                 na.action = na.exclude)

#5. Use logistic regression all vars
log_fit <- train(form = OUTCOME ~ .,
             data=Samp1_10k, 
             method="glm",
             family="binomial",
             trControl = fit_ctrl,
             na.action = na.exclude)

summary(log_fit)
varImp(log_fit)
log_pred <- predict(log_fit,
                    newdata=Samp2_10k)

confusionMatrix(data=log_pred, Samp2_10k$OUTCOME, positive = "Yes")


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
