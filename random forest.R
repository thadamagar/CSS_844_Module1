#####Biological Question: Which VI will be more important to predict days to pollen formation??  #############
#there are few ways to do this #######
#1. Correlate trait to the VI and select VI with higher correlation with trait
getwd()
setwd("C:/Users/thadaraj/OneDrive - Michigan State University/Desktop/HTP_spring2025class/Genomic prediction project//")
#load data
library(readxl)
dt=read_excel("PlotID_Pheno_VI_JD.xlsx")
head(dt)
dt[dt=="NA"]= NA
sum(is.na(dt$Avg_Days_to_Pollen))#275
dtclean=dt[complete.cases(dt),]
dim(dtclean)

write.csv(dtclean, "dtwithJD.csv")

#conversion to factor
dtclean$Date <- as.factor(dtclean$Date)
dtclean$id<- as.factor(dtclean$id)
dtclean$Avg_Days_to_Pollen=as.numeric(dtclean$Avg_Days_to_Pollen)
Splitdt=split.data.frame(dtclean, (dtclean$Date))
spltdt1=as.data.frame(Splitdt$`7.12`)
spltdt2=as.data.frame(Splitdt$`8.08`)
spltdt3=as.data.frame(Splitdt$`8.22`)
write.csv(spltdt3, "date3.csv")
spltdt4=as.data.frame(Splitdt$`8.31`)
write.csv(spltdt4, "date4.csv")
spltdt5=as.data.frame(Splitdt$`10.02`)
write.csv(spltdt5, "date5.csv")

#ftable(id ~ Date, dt )

colSums(is.na(dtclean[,c("Avg_Days_to_Pollen","Norm_NDVI","Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI","Norm_RTVIcore")]))
cormatAll <- cor(
  dtclean[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
)
print(cormatAll)

#for individual date
cormat1 <- cor(
  spltdt1[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
)
print(cormat1)

cormat2 <- cor(
  spltdt2[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
)
print(cormat2)


cormat3 <- cor(
  spltdt3[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
)
print(cormat3)

cormat4 <- cor(
  spltdt4[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
) #date 4 data has more corrlation to the trait of interest
print(cormat4)


cormat5 <- cor(
  spltdt5[, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")],
  use = "pairwise.complete.obs" 
)
print(cormat5)

plot(Norm_NDVI ~ Date, 
     data = dtclean,
     main = "Norm_NDVI Over Time",
     xlab = "Date",
     ylab = "Norm_NDVI",
     col = "darkgreen")

plot(Norm_EVI ~ Date, 
     data = dtclean,
     main = "Norm_EVI Over Time",
     xlab = "Date",
     ylab = "Norm_EVI",
     col = "darkgreen")

############in corrlation analysis date 4 8-31 has higher corrlation with trait Norm_EVI= 0.223


#######Random forest ##############
### Import libraries
library(randomForest)
library(ggplot2)
dtcleanWOna=na.omit(dtclean [, c("Avg_Days_to_Pollen", "Norm_NDVI", "Norm_NDRE", "Norm_EVI", "Norm_CI", "Norm_SAVI", "Norm_RTVIcore")])
dim(dtcleanWOna)#2875    7
dim(dtclean) #[1] 2979   14


train<-as.matrix(sample(1:2875, 2300)) 
test<- setdiff(1:2875, train) 
Pheno_train<-dtcleanWOna[train,] 
hist(Pheno_train$Avg_Days_to_Pollen)
Pheno_test<-dtcleanWOna[test,] 
hist(Pheno_test$Avg_Days_to_Pollen)
set.seed(4543)
rf.fit <- randomForest( Avg_Days_to_Pollen ~ ., 
                       data=Pheno_train,
                       mtry=2,
                       ntree=2000,
                       keep.forest=TRUE,
                       importance=TRUE)
rf.fit
plot(rf.fit)
importance(rf.fit)
varImpPlot(rf.fit, type = 1, main = "Variable Importance (MSE Increase)")

result=data.frame(Actual=Pheno_test$Avg_Days_to_Pollen,
                  Predicted= predict(rf.fit, Pheno_test))
cor(result$Actual, result$Predicted)^2 # 0.236374 #0.05
plot(result$Actual, result$Predicted)

####does rf works better for the days to silk?
dtclean=dtclean[complete.cases(dtclean),]
dim(dtclean)
write.csv(dtclean, "cleandtWOna.csv")

train<-as.matrix(sample(1:2875, 2300)) 
test<- setdiff(1:2875, train) 
Pheno_train<-dtclean[train,] 
hist(Pheno_train$Avg_Days_to_Pollen)
Pheno_test<-dtclean[test,] 
hist(Pheno_test$Avg_Days_to_Pollen)
dtclean$Avg_Days_to_Silk=as.numeric(dtclean$Avg_Days_to_Silk)
rf.fit2 <- randomForest( Avg_Days_to_Silk ~ Norm_NDVI + Norm_NDRE + Norm_EVI + Norm_CI + Norm_SAVI + Norm_RTVIcore, 
                        data=dtclean,
                        mtry=2,
                        ntree=1000,
                        keep.forest=T,
                        importance=TRUE,
                        na.action = na.omit)
rf.fit2
plot(rf.fit)

result2=data.frame(observed=dtclean$Avg_Days_to_Silk, 
                   predicted= predict(rf.fit2,Pheno_test))

cor(result2$observed, result2$predicted)^2






##############Random forest is not good for this data how about XGboost ####################
############################################################################################
install.packages("xgboost")
library(xgboost) 
###prepare your data for the Xgboost 
data=as.data.frame(read.csv("finalphenodata.csv"))
data$JD_final=as.numeric(data$JD_final)
X=as.matrix(data[,-c(1:10)]) #predictor matrix
y=as.numeric(data[,8]) #response matrix

idx=sort(sample(length(y),round(length(y)*0.8))) # random 80% train
Xd=X[idx,]
yd=y[idx]
Xv=X[idx,]
yv=y[idx]

# for gpu support need to compile externally and replace the dll/so (R specific - cannot just "pinch" from e.g. python)
params=list(objective = "reg:squarederror", #Minimizes squared error for regression tasks.
            eval_metric="rmse", #Evaluates model performance using Root Mean Squared Error.
            eta=0.05,  # Learning Rate : Lower learning rate for finer convergence
            max_depth=3, #Sets the maximum depth of each decision tree.
            nthread = 36, #Number of CPU threads to use for parallel processing.
            max_bin = 64, #Number of bins used to discretize continuous features
            gamma=1, #Minimum loss reduction required to split a node (gamma = 0 allows all splits, even trivial ones, risking overfitting. A higher gamma (e.g., 1) prevents splits that don’t significantly improve the model (reduces overfitting).)
            subsample=0.8, #Fraction of training data randomly sampled for each tree ( subsample = 1 uses all data (no randomness).)
            tree_method = "hist") #Algorithm for building trees."hist": Approximates splits using histograms (fast, memory-efficient).
                                  #"exact": Evaluates all possible splits (slower but more precise).
# no gpu
#params2=list(objective = "reg:squarederror", 
            # eval_metric="rmse", 
            # eta=0.3, 
            # max_depth=3,
             #nthread = 36, 
             #max_bin = 64, 
             #gamma=0, 
             #subsample=1)

boost = xgboost(
  data = Xd,          # Feature matrix (predictors)
  label = yd,         # Target variable (what you're predicting)
  params = params,    # Hyperparameters for model behavior
  nrounds = 40,       # Number of boosting iterations (trees)
  booster = "gbtree"  # Type of model (gradient boosted trees)
)

pred = predict(boost,Xv)
accboost=cor(pred,yv)^2
print(accboost)

# save/load model
#xgb.save(boost, "boost.model")
#boost = xgb.load("boost.model")

# top ranked parameters in model
importance=as.data.frame(xgb.importance(feature_names = colnames(Xd), model = boost)) # get most important features
head(importance)

# collects junk in GPU memory need to force release or will get out of mem error over repeated iterations
rm(boost)
gc()




######Xgboost for the date 4 image ##########################
library(xgboost) 
###prepare your data for the Xgboost 
data=as.data.frame(read.csv("date4.csv", row.names = 1))
colSums(is.na(data))
X=as.matrix(data[,-c(1:7)]) #predictor matrix
y=as.numeric(data[,5]) #response matrix

idx=sort(sample(length(y),round(length(y)*0.8))) # random 80% train
Xd=X[idx,]
yd=y[idx]
Xv=X[idx,]
yv=y[idx]

# for gpu support need to compile externally and replace the dll/so (R specific - cannot just "pinch" from e.g. python)
params=list(objective = "reg:squarederror", #Minimizes squared error for regression tasks.
            eval_metric="rmse", #Evaluates model performance using Root Mean Squared Error.
            eta=0.05,  # Learning Rate : Lower learning rate for finer convergence
            max_depth=3, #Sets the maximum depth of each decision tree.
            nthread = 36, #Number of CPU threads to use for parallel processing.
            max_bin = 64, #Number of bins used to discretize continuous features
            gamma=1, #Minimum loss reduction required to split a node (gamma = 0 allows all splits, even trivial ones, risking overfitting. A higher gamma (e.g., 1) prevents splits that don’t significantly improve the model (reduces overfitting).)
            subsample=0.8, #Fraction of training data randomly sampled for each tree ( subsample = 1 uses all data (no randomness).)
            tree_method = "hist") #Algorithm for building trees."hist": Approximates splits using histograms (fast, memory-efficient).
#"exact": Evaluates all possible splits (slower but more precise).
# no gpu
#params2=list(objective = "reg:squarederror", 
# eval_metric="rmse", 
# eta=0.3, 
# max_depth=3,
#nthread = 36, 
#max_bin = 64, 
#gamma=0, 
#subsample=1)

boost = xgboost(
  data = Xd,          # Feature matrix (predictors)
  label = yd,         # Target variable (what you're predicting)
  params = params,    # Hyperparameters for model behavior
  nrounds = 40,       # Number of boosting iterations (trees)
  booster = "gbtree"  # Type of model (gradient boosted trees)
)


pred = predict(boost,Xv)
accboost=cor(pred,yv)^2 #0.18
print(accboost)

# save/load model
#xgb.save(boost, "boost.model")
#boost = xgb.load("boost.model")

# top ranked parameters in model
importance=as.data.frame(xgb.importance(feature_names = colnames(Xd), model = boost)) # get most important features
head(importance)

# collects junk in GPU memory need to force release or will get out of mem error over repeated iterations
rm(boost)
gc()



######Xgboost for the date 5 image ##########################
library(xgboost) 
###prepare your data for the Xgboost 
data=as.data.frame(read.csv("date5.csv", row.names = 1))
colSums(is.na(data))
X=as.matrix(data[,-c(1:7)]) #predictor matrix
y=as.numeric(data[,5]) #response matrix

idx=sort(sample(length(y),round(length(y)*0.8))) # random 80% train
Xd=X[idx,]
yd=y[idx]
Xv=X[idx,]
yv=y[idx]

# for gpu support need to compile externally and replace the dll/so (R specific - cannot just "pinch" from e.g. python)
params=list(objective = "reg:squarederror", #Minimizes squared error for regression tasks.
            eval_metric="rmse", #Evaluates model performance using Root Mean Squared Error.
            eta=0.05,  # Learning Rate : Lower learning rate for finer convergence
            max_depth=3, #Sets the maximum depth of each decision tree.
            nthread = 36, #Number of CPU threads to use for parallel processing.
            max_bin = 64, #Number of bins used to discretize continuous features
            gamma=1, #Minimum loss reduction required to split a node (gamma = 0 allows all splits, even trivial ones, risking overfitting. A higher gamma (e.g., 1) prevents splits that don’t significantly improve the model (reduces overfitting).)
            subsample=0.8, #Fraction of training data randomly sampled for each tree ( subsample = 1 uses all data (no randomness).)
            tree_method = "hist") #Algorithm for building trees."hist": Approximates splits using histograms (fast, memory-efficient).
#"exact": Evaluates all possible splits (slower but more precise).
# no gpu
#params2=list(objective = "reg:squarederror", 
# eval_metric="rmse", 
# eta=0.3, 
# max_depth=3,
#nthread = 36, 
#max_bin = 64, 
#gamma=0, 
#subsample=1)

boost = xgboost(
  data = Xd,          # Feature matrix (predictors)
  label = yd,         # Target variable (what you're predicting)
  params = params,    # Hyperparameters for model behavior
  nrounds = 40,       # Number of boosting iterations (trees)
  booster = "gbtree"  # Type of model (gradient boosted trees)
)


pred = predict(boost,Xv)
accboost=cor(pred,yv)^2 #0.18
print(accboost)

# save/load model
#xgb.save(boost, "boost.model")
#boost = xgb.load("boost.model")

# top ranked parameters in model
importance=as.data.frame(xgb.importance(feature_names = colnames(Xd), model = boost)) # get most important features
head(importance)

# collects junk in GPU memory need to force release or will get out of mem error over repeated iterations
rm(boost)
gc()






