################################################################################
####  This script demonstrates the sample partitioning and model fitting    ####
####   procedures used to fit the main FTA model used in the paper.         ####
####    File names, directories, and random seeds have been changed.        ####
################################################################################
require(plyr)
require(dplyr)
require(caret)
require(ggplot2)
require(pROC)

### load total training set
df <- read.csv('bail_dataset.csv')

##### 1. Divide into Train / Test / Impute #####
set.seed(1234567890)

###
# set aside 20% for test
test.ix     <- createDataPartition(df$fta, p = .2, list = F)
test        <- df[test.ix,]

# remove test from rest
df          <- df[-test.ix,]

# split the remaining half
train.ix    <- createDataPartition(df$fta, p = .5, list = F)

train       <- df[train.ix, ]
impute      <- df[-train.ix,]

# releases
train.rel   <- which(train$released  == 'Definitely')
impute.rel  <- which(impute$released == 'Definitely')  

#### 2. Model Feature Selection ####
Xs <- c('arr_age','arr_counts','arr_county','arr_mo',
        'arr_charge_category','arr_class_fac', 
        'arr_vfo', 'cycle_cc_dv', 'arr_firearm', 'arr_weapon', 
        'arr_minors', 'arr_hate', 'arr_drug', 'arr_1192', 
        'p_ftas',
        'p_felarr', 'p_misdarr',
        'p_vfoarr', 'p_drugarr', 'p_minorsarr',
        'p_weparr', 'p_soarr', 'p_vtl','p_dwi','p_firearm',
        'p_felcon', 'p_misdcon', 'p_vfocon',
        'p_drugcon', 'p_drugfelcon','p_drugmisdcon','p_minorscon',
        'p_wepcon', 'p_socon', 'p_vtlcon','p_dwifelcon','p_dwimisdcon','p_firearmcon'
        )

options(na.action = 'na.pass')

#### 3. Fit Models ####
# make model matrices
train.mat     <- model.matrix(~ ., train[,Xs])[,-1]
impute.mat    <- model.matrix(~ ., impute[,Xs])[,-1]
test.mat      <- model.matrix(~ ., test[,Xs])[,-1]

# set up model parameters and grid search to try
ctrl          <- trainControl(method = 'cv', number = 5, classProbs = T, returnData = F)
trgrid        <- expand.grid(n.trees = c(1:25 *100),
                      interaction.depth = c(2,3,4), shrinkage = c(.025, .05, .1),  n.minobsinnode = 20)

### ---- Fit Models -----
train$fta     <- ifelse(train$fta == 1, 'Y', 'N')
train.fit     <- train(x = train.mat[train.rel,],
                       y = train[train.rel,'fta'],
                       method = 'gbm', 
                       trControl = ctrl,
                       tuneGrid = trgrid)                                              

impute$fta    <- ifelse(impute$fta == 1,'Y','N')
impute.fit    <- train(x = impute.mat[impute.rel,],
                       y = impute[impute.rel,'fta'],
                       method = 'gbm', 
                       trControl = ctrl,
                       tuneGrid = trgrid)

test$phat     <- predict(train.fit,  newdata = test.mat, type = 'prob')[,2]
test$impute   <- predict(impute.fit, newdata = test.mat, type = 'prob')[,2]

# update imputed values to observed for released people
test$impute   <- ifelse(test$released == 'Definitely', test$fta, test$impute)

#### 4. Summarise results ####
# Judges Total Crime and Release
judge.rel.n   <- length(which(test$released == 'Definitely'))
judge.fta.n   <- sum(test$fta)

# ML Total Crime and Release
# sort by predicted risk
test          <- arrange(test, phat)

ml.fta.n      <- sum(test[1:judge.rel.n,'impute'])

test$cum.fta  <- cumsum(test$impute)
ml.rel        <- min(which(test$cum.fta >= judge.fta.n))/nrow(test) 

# rates
print(paste('Judge FTA Rate:', judge.fta.n/nrow(test)))
print(paste('ML FTA Rate:', ml.fta.n/nrow(test)))

#### 5. Save Models ####
saveRDS(train.fit , file = 'output/models/training_model')
saveRDS(impute.fit, file = 'output/models/imputation_model')
