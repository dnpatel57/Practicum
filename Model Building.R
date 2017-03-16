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

#Partition data into k samples (has equal priors as full sample)
set.seed(2017)
trainindex <- createFolds(Raw$OUTCOME, k = 4)
Samp1<- Raw[trainindex$Fold1 ,]


#------------------ TEST VARIABLES -------------------

## Create Controls

fit_ctrl <- trainControl(method = "none", 
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

fit_ctrl1 <- fit_ctrl <- trainControl(method = "oob", 
                                      classProbs = TRUE,
                                      summaryFunction = twoClassSummary)

#1. Use Random Forest



#2. Use SVM



#3. Use adaboost classification tree
set.seed(2017)
ada_fit <- train(form = OUTCOME ~ .,              #FORMULA
                 data = Samp1,
                 method = "adaboost",
                 trControl = fit_ctrl, 
                 metric = "ROC",
                 na.action = na.exclude)

#4. Try "treebag" method
set.seed(2017)
bag_fit <- train(form = OUTCOME ~ .,              #FORMULA
                 data = Samp1, 
                 method = "treebag",
                 trControl = fit_ctrl1, 
                 metric = "ROC",
                 na.action = na.exclude)

#4. Use logistic regression



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
